"""The runner of the Telefonspiel (docs/plan-telefonspiel.md).

Asterisk hands every call to the Stasis app "telefonspiel". For each call
the runner answers, connects a stream of its own into the call (External
Media: RTP with 16 kHz audio as G.722, bridged with the caller), mixes music and
speech into it live (mixer.py), reads the keys from ARI events, plays the
menu and then runs the chosen game as a process of its own.

A game speaks the protocol of JSON lines: it writes a command on stdout
and reads the answer on stdin (docs/plan-telefonspiel.md, »The protocol«).
Lines that are not JSON are its ordinary output and go to the log.
"""

import asyncio
import json
import os
import resource
import socket
import sys

import aiohttp

import sounds
from mixer import Mixer, silence

ARI = os.environ.get('PHONE_ARI', 'http://asterisk:8088/ari')
ARI_USER = os.environ.get('PHONE_ARI_USER', 'runner')
ARI_PASSWORD = os.environ.get('PHONE_ARI_PASSWORD', 'runner')
APP = 'telefonspiel'
GAMES = os.environ.get('PHONE_GAMES', '/games')
RTP_PORTS = range(20000, 20100)
GAME_SECONDS = int(os.environ.get('PHONE_GAME_SECONDS', '900'))   # a call at most 15 minutes
GAME_MEMORY = 512 * 1024 * 1024
GAME_CPU = 120                                                    # seconds of processor time


import time

_start = time.monotonic()


def log(*parts):
    print(f'{time.monotonic() - _start:8.2f}', *parts, file=sys.stderr, flush=True)


class Call:
    def __init__(self, runner, channel):
        self.runner = runner
        self.channel = channel
        self.keys = asyncio.Queue()
        self.mixer = None
        self.sock = None
        self.port = None
        self.media_channel = None
        self.bridge = None
        self.process = None
        self.ended = asyncio.Event()

    # ARI -----------------------------------------------------------------

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
        loop = asyncio.get_running_loop()
        loop.add_reader(self.sock.fileno(), self._receive)
        self.mixer.start()

    def _receive(self):
        try:
            while True:
                self.sock.recv(2048)        # the caller's audio: not used (yet)
        except (BlockingIOError, OSError):
            pass

    async def teardown(self):
        if self.process and self.process.returncode is None:
            try:
                os.killpg(self.process.pid, 9)
            except ProcessLookupError:
                pass
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

    async def say(self, text):
        """speak, sentence by sentence: the first plays while the next is made"""
        log(f'[{self.channel}] say', repr(str(text)[:80]))
        heard = None
        for sentence in sounds.sentences(str(text)):
            samples = await sounds.in_thread(sounds.speak_sentence, sentence)
            if self.ended.is_set():
                return
            heard = self.mixer.say(samples)
        if heard:
            await heard

    async def sleep(self, ms):
        await asyncio.sleep(max(0, float(ms)) / 1000)

    async def music(self, source, volume=0.5):
        log(f'[{self.channel}] music', source, volume)
        if not source:
            self.mixer.set_music(None)
            return
        samples = await sounds.in_thread(sounds.fetch_music, str(source))
        self.mixer.set_music(samples, volume)

    async def read_keys(self, count, timeout_ms):
        """up to `count` keys; keys pressed earlier (during speech) count too;
        # ends early"""
        digits = ''
        while len(digits) < count:
            try:
                key = await asyncio.wait_for(self.keys.get(), timeout_ms / 1000)
            except asyncio.TimeoutError:
                break
            if key == '#':
                break
            digits += key
        log(f'[{self.channel}] keys', repr(digits))
        return digits

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
        await self.music('/app/sounds/menu.ogg' if os.path.exists('/app/sounds/menu.ogg') else None)
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
        folder = os.path.join(GAMES, code)
        with open(os.path.join(folder, 'game.json')) as f:
            meta = json.load(f)
        self.mixer.set_music(None)

        def limits():
            os.setsid()
            resource.setrlimit(resource.RLIMIT_CPU, (GAME_CPU, GAME_CPU))
            resource.setrlimit(resource.RLIMIT_AS, (GAME_MEMORY, GAME_MEMORY))
            resource.setrlimit(resource.RLIMIT_NPROC, (64, 64))
            if os.getuid() == 0:
                import pwd
                user = pwd.getpwnam('game')
                os.setgid(user.pw_gid)
                os.setuid(user.pw_uid)

        env = {'PATH': '/usr/local/bin:/usr/bin:/bin', 'PYTHONPATH': '/app/lib/python', 'PYTHONUNBUFFERED': '1',
               'LANG': 'C.UTF-8', 'HOME': '/tmp'}
        self.process = await asyncio.create_subprocess_exec(
            *meta['command'], cwd=folder, env=env, preexec_fn=limits,
            stdin=asyncio.subprocess.PIPE, stdout=asyncio.subprocess.PIPE, stderr=asyncio.subprocess.PIPE)
        stderr_task = asyncio.create_task(self._pipe_log(self.process.stderr, f'[{code} stderr]'))
        try:
            await asyncio.wait_for(self._protocol(code), GAME_SECONDS)
        except asyncio.TimeoutError:
            await self.say('Die Zeit ist um. Tschüss!')
        finally:
            stderr_task.cancel()

    async def _pipe_log(self, stream, prefix):
        async for line in stream:
            log(prefix, line.decode('utf-8', 'replace').rstrip())

    async def _protocol(self, code):
        while not self.ended.is_set():
            line = await self.process.stdout.readline()
            if not line:
                await self.process.wait()
                log(f'[{self.channel}] game ended with', self.process.returncode)
                return                                   # the game ended
            text = line.decode('utf-8', 'replace').strip()
            if not text.startswith('{'):
                log(f'[{code}]', text)                   # an ordinary print
                continue
            try:
                command = json.loads(text)
            except json.JSONDecodeError:
                log(f'[{code}] not a command:', text)
                continue
            answer = await self.execute(command)
            if answer is None or self.ended.is_set():
                return                                   # hang up, or the caller did
            try:
                self.process.stdin.write((json.dumps(answer) + '\n').encode('utf-8'))
                await self.process.stdin.drain()
            except (ConnectionResetError, BrokenPipeError):
                return                                   # the game is gone

    async def execute(self, command):
        if 'say' in command:
            await self.say(command['say'])
            return {'ok': True}
        if 'sleep' in command:
            await self.sleep(command['sleep'])
            return {'ok': True}
        if 'music' in command:
            await self.music(command['music'], command.get('volume', 0.5))
            return {'ok': True}
        if 'keys' in command:
            digits = await self.read_keys(int(command['keys']), float(command.get('timeout', 10000)))
            return {'keys': digits}
        if command.get('hangup'):
            return None
        return {'error': 'unknown command'}

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
        async with self.session.request(method, ARI + path, params=params) as response:
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
                    log('call from', event['channel'].get('caller', {}).get('number', '?') and 'a caller')
                    call = Call(self, channel)
                    self.calls[channel] = call
                    asyncio.create_task(self._run_call(call))
                elif kind == 'ChannelDtmfReceived' and channel in self.calls:
                    log(f'[{channel}] key', event['digit'])
                    self.calls[channel].keys.put_nowait(event['digit'])
                elif kind in ('StasisEnd', 'ChannelDestroyed') and channel in self.calls:
                    self.calls[channel].ended.set()
                    if self.calls[channel].process and self.calls[channel].process.returncode is None:
                        try:
                            os.killpg(self.calls[channel].process.pid, 9)
                        except ProcessLookupError:
                            pass

    async def _run_call(self, call):
        try:
            await call.run()
        finally:
            self.calls.pop(call.channel, None)

    async def main(self):
        auth = aiohttp.BasicAuth(ARI_USER, ARI_PASSWORD)
        async with aiohttp.ClientSession(auth=auth) as self.session:
            while True:
                try:
                    await self.events()
                except (aiohttp.ClientError, OSError) as error:
                    log('waiting for Asterisk:', error)
                await asyncio.sleep(2)


if __name__ == '__main__':
    asyncio.run(Runner().main())
