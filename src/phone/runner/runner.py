"""The runner of the Telefonspiel (docs/plan-telefonspiel.md).

Asterisk hands every call to the Stasis app "telefonspiel". For each call
the runner answers, connects a stream of its own into the call (External
Media: RTP with 16 kHz audio as G.722, bridged with the caller), mixes music
and speech into it live (mixer.py), reads the keys from ARI events, plays
the menu and then runs the chosen game as a process of its own.

Running the game and its protocol is shared with the emulator in the
Workspace (src/phone/core/game.py), so a game behaves the same in both.
"""

import asyncio
import json
import os
import pwd
import resource
import socket
import sys
import time

import aiohttp

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'core'))
import game  # noqa: E402
import sounds  # noqa: E402
from mixer import Mixer, silence  # noqa: E402

ARI = os.environ.get('PHONE_ARI', 'http://asterisk:8088/ari')
ARI_USER = os.environ.get('PHONE_ARI_USER', 'runner')
ARI_PASSWORD = os.environ.get('PHONE_ARI_PASSWORD', 'runner')
APP = 'telefonspiel'
GAMES = os.environ.get('PHONE_GAMES', '/games')
RTP_PORTS = range(20000, 20100)
GAME_SECONDS = int(os.environ.get('PHONE_GAME_SECONDS', '900'))   # a call at most 15 minutes
GAME_MEMORY = 512 * 1024 * 1024
GAME_CPU = 120                                                    # seconds of processor time

_start = time.monotonic()


def log(*parts):
    print(f'{time.monotonic() - _start:8.2f}', *parts, file=sys.stderr, flush=True)


def game_limits():
    """in the game's process, before it starts: a session of its own, limits,
    the unprivileged user game"""
    os.setsid()
    resource.setrlimit(resource.RLIMIT_CPU, (GAME_CPU, GAME_CPU))
    resource.setrlimit(resource.RLIMIT_AS, (GAME_MEMORY, GAME_MEMORY))
    resource.setrlimit(resource.RLIMIT_NPROC, (64, 64))
    if os.getuid() == 0:
        user = pwd.getpwnam('game')
        os.setgid(user.pw_gid)
        os.setuid(user.pw_uid)


class Call(game.Phone):
    def __init__(self, runner, channel):
        super().__init__()
        self.runner = runner
        self.channel = channel
        self.folder = GAMES
        self.mixer = None
        self.sock = None
        self.port = None
        self.media_channel = None
        self.bridge = None

    def log(self, *parts):
        log(f'[{self.channel}]', *parts)

    async def ari(self, method, path, **params):
        return await self.runner.ari(method, path, **params)

    async def setup(self):
        await self.ari('POST', f'/channels/{self.channel}/answer')
        self.port = self.runner.take_port()
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.sock.bind(('0.0.0.0', self.port))
        self.sock.setblocking(False)
        host = socket.gethostbyname(socket.gethostname())
        media = await self.ari('POST', '/channels/externalMedia', app=APP,
                               external_host=f'{host}:{self.port}', format='g722',
                               encapsulation='rtp', transport='udp', direction='both')
        self.media_channel = media['id']
        self.runner.media_channels.add(self.media_channel)
        vars_ = media.get('channelvars', {})
        remote = (vars_['UNICASTRTP_LOCAL_ADDRESS'], int(vars_['UNICASTRTP_LOCAL_PORT']))
        bridge = await self.ari('POST', '/bridges', type='mixing')
        self.bridge = bridge['id']
        await self.ari('POST', f'/bridges/{self.bridge}/addChannel', channel=f'{self.channel},{self.media_channel}')
        self.mixer = Mixer(self.sock, remote)
        asyncio.get_running_loop().add_reader(self.sock.fileno(), self._receive)
        self.mixer.start()

    def _receive(self):
        try:
            while True:
                self.sock.recv(2048)        # the caller's audio: not used (yet)
        except (BlockingIOError, OSError):
            pass

    async def teardown(self):
        if self.mixer:
            self.mixer.stop()
        if self.sock:
            asyncio.get_running_loop().remove_reader(self.sock.fileno())
            self.sock.close()
            self.runner.give_port(self.port)
        for path in ([f'/channels/{self.media_channel}'] if self.media_channel else []) + \
                    ([f'/bridges/{self.bridge}'] if self.bridge else []) + [f'/channels/{self.channel}']:
            try:
                await self.ari('DELETE', path)
            except Exception:
                pass
        self.runner.media_channels.discard(self.media_channel)

    # what a game can do --------------------------------------------------

    async def say(self, text, interrupt=False, mood=None):
        """sentence by sentence: the first plays while the next is made;
        interrupt: a key cuts the speech (press) and stays for keys()"""
        self.log('say', repr(text[:80]))
        heard = None
        self.cut = False
        self.interruptible = interrupt
        try:
            for sentence in game.sentences(text):
                samples = await sounds.speak(self.runner.session, sentence, mood)
                if self.ended.is_set() or self.cut:
                    return
                heard = self.mixer.say(samples)
            if heard:
                await heard
        finally:
            self.interruptible = False

    def press(self, key):
        super().press(key)
        if getattr(self, 'interruptible', False):
            self.cut = True
            self.mixer.cut_speech()

    async def music(self, source, volume=1.0, start=0.0, stop=None):
        self.log('music', source, volume, start, stop)
        if not source:
            self.mixer.set_music(None)
            return
        try:
            samples = await sounds.music(self.runner.session, self.folder, str(source), start, stop)
        except Exception as error:
            self.log('music failed:', repr(error))
            return
        try:
            volume = float(volume)
        except (TypeError, ValueError):
            volume = 1.0
        self.mixer.set_music(samples, volume)

    # the menu and the games ---------------------------------------------

    def games(self):
        found = {}
        if os.path.isdir(GAMES):
            for code in sorted(os.listdir(GAMES)):
                meta = os.path.join(GAMES, code, 'game.json')
                if code.isdigit() and os.path.exists(meta):
                    with open(meta) as f:
                        found[code] = json.load(f)
        return found

    async def menu(self):
        games = self.games()
        if not games:
            await self.say('Hallo! Hier gibt es gerade keine Spiele. Schreib doch selbst eins.')
            return None
        await self.say('Hallo und willkommen beim Telefonspiel der Hackschule.')
        for attempt in range(3):
            for code, meta in games.items():
                await self.say(f"Drücke die {' '.join(code)} für: {meta.get('title', 'ein Spiel')}.")
                if not self.keys.empty():
                    break
            width = max(len(code) for code in games)
            choice = await self.read_keys(width, 8000)
            if choice in games:
                return choice
            if choice:
                await self.say(f"Ein Spiel mit der {' '.join(choice)} gibt es nicht.")
        await self.say('Bis bald!')
        return None

    async def run_game(self, code):
        self.folder = os.path.join(GAMES, code)
        with open(os.path.join(self.folder, 'game.json')) as f:
            meta = json.load(f)
        self.mixer.set_music(None)
        env = game.environment({'PATH': '/usr/local/bin:/usr/bin:/bin', 'LANG': 'C.UTF-8', 'HOME': '/tmp'})
        reason, status = await game.run_game(
            self, game.command_for(meta['command']), self.folder, env,
            lambda kind, line: log(f'[{code} {kind}]', line),
            preexec_fn=game_limits, seconds=GAME_SECONDS)
        self.log('game ended:', reason, status)
        if reason == 'time':
            await self.say('Die Zeit ist um. Tschüss!')

    async def run(self):
        try:
            await self.setup()
            code = await self.menu()
            if code and not self.ended.is_set():
                await self.run_game(code)
            if not self.ended.is_set():
                self.mixer.say(silence(600))
                await asyncio.sleep(0.6)
        except Exception as error:
            log('call failed:', repr(error))
        finally:
            await self.teardown()


class Runner:
    def __init__(self):
        self.session = None
        self.calls = {}
        self.media_channels = set()
        self.free_ports = list(RTP_PORTS)

    def take_port(self):
        return self.free_ports.pop(0)

    def give_port(self, port):
        self.free_ports.append(port)

    async def ari(self, method, path, **params):
        async with self.session.request(method, ARI + path, params=params,
                                        auth=aiohttp.BasicAuth(ARI_USER, ARI_PASSWORD)) as response:
            if response.status >= 400:
                raise RuntimeError(f'{method} {path}: {response.status} {await response.text()}')
            if response.content_type == 'application/json':
                return await response.json()
            return None

    async def events(self):
        url = ARI.replace('http', 'ws', 1) + f'/events?app={APP}&api_key={ARI_USER}:{ARI_PASSWORD}'
        async with self.session.ws_connect(url, heartbeat=30) as ws:
            log('connected to Asterisk')
            async for message in ws:
                event = json.loads(message.data)
                kind = event.get('type')
                channel = event.get('channel', {}).get('id')
                if kind == 'StasisStart' and channel not in self.media_channels \
                        and not event['channel'].get('name', '').startswith('UnicastRTP'):
                    log('a call')          # no caller's number in the log (docs/plan-telefonspiel.md, »Open«)
                    call = Call(self, channel)
                    self.calls[channel] = call
                    asyncio.create_task(self._run_call(call))
                elif kind == 'ChannelDtmfReceived' and channel in self.calls:
                    log(f'[{channel}] key', event['digit'])
                    self.calls[channel].press(event['digit'])
                elif kind in ('StasisEnd', 'ChannelDestroyed') and channel in self.calls:
                    self.calls[channel].ended.set()

    async def _run_call(self, call):
        try:
            await call.run()
        finally:
            self.calls.pop(call.channel, None)

    async def main(self):
        async with aiohttp.ClientSession() as self.session:
            while True:
                try:
                    await self.events()
                except (aiohttp.ClientError, OSError) as error:
                    log('waiting for Asterisk:', error)
                await asyncio.sleep(2)


if __name__ == '__main__':
    asyncio.run(Runner().main())
