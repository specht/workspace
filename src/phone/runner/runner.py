"""The runner of the Telefonspiel (docs/plan-telefonspiel.md).

Asterisk hands every call to the Stasis app "telefonspiel". For each call
the runner answers, connects a stream of its own into the call (External
Media: RTP with 16 kHz audio as G.722, bridged with the caller), mixes music
and speech into it live (mixer.py), reads the keys from ARI events and runs
the Telefonbuch's menu (src/phone/line/menu.py) as a process of its own,
which becomes the published game the caller chooses, or the one dialled
after the line's number (PHONE_DIRECT_DIAL).

Running the game and its protocol is shared with the emulator in the
Workspace (src/phone/core/game.py), so a game behaves the same in both.
"""

import asyncio
import json
import os
import pwd
import re
import resource
import socket
import sys
import time

import aiohttp

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'core'))
import ari_guard  # noqa: E402
import game  # noqa: E402
import sounds  # noqa: E402
from mixer import Mixer, silence  # noqa: E402

ARI = os.environ.get('PHONE_ARI', 'http://asterisk:8088/ari')
ARI_USER = os.environ.get('PHONE_ARI_USER', 'runner')
ARI_PASSWORD = os.environ.get('PHONE_ARI_PASSWORD', 'runner')
APP = 'telefonspiel'
GAMES = os.environ.get('PHONE_GAMES', '/games')
# the Telefonbuch's menu (src/phone/line/menu.py): the same on the phone as
# in the browser; it reads the published games (GAMES/published.json) and
# becomes the chosen one
MENU_FOLDER = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'line')
MENU = [sys.executable, os.path.join(MENU_FOLDER, 'menu.py')]
# the line's number (PHONE_NUMBER) and whether the provider passes digits
# dialled after it on (PHONE_DIRECT_DIAL): »030 22908772 123« starts game 123
DIRECT_DIAL = os.environ.get('PHONE_DIRECT_DIAL', '') in ('1', 'true', 'yes')
RTP_PORTS = range(20000, 20100)
GAME_SECONDS = int(os.environ.get('PHONE_GAME_SECONDS', '900'))   # a call at most 15 minutes
# how often one call may be forwarded from game to game (transfer)
MAX_TRANSFERS = 5
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


LINE_NUMBER = os.environ.get('PHONE_NUMBER', '')


def direct_digits(exten):
    """the game's number dialled after the line's (»030/22 908 772-123«), or
    '' for the menu: a call to the bare number arrives with the SIP user's
    name instead of the number (easybell, tested 12 November 2026)"""
    if not DIRECT_DIAL or not LINE_NUMBER or not re.fullmatch(r'\+?\d+', exten or ''):
        return ''
    # only the line's number with digits after it: a trunk reports the
    # whole number, never a game's alone
    return game.game_number(exten, LINE_NUMBER) if game.international(exten).startswith(game.international(LINE_NUMBER)) else ''


def published():
    """the published games: (folders, numbers)"""
    try:
        with open(os.path.join(GAMES, 'published.json')) as f:
            games = [g for g in json.load(f).get('games', []) if isinstance(g, dict)]
    except (OSError, ValueError, AttributeError):
        games = []
    return {str(g.get('folder')) for g in games}, {str(g.get('number')) for g in games}


class Call(game.Phone):
    def __init__(self, runner, channel):
        super().__init__()
        self.runner = runner
        self.channel = channel
        self.folder = MENU_FOLDER
        self.dialed = ''            # a game's number dialled after the line's (direct_digits)
        self.switched = False
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
        # nothing the caller says leaves Asterisk: their voice becomes
        # silence before the bridge; the keys are events of their own and
        # still arrive (ChannelDtmfReceived)
        await self.ari('POST', f'/channels/{self.channel}/mute', direction='in')
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
                self.sock.recv(2048)        # silence (the caller is muted): read and dropped, never kept
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

    def transfer_problem(self, number):
        """a forwarded call goes only to a published game"""
        if number not in published()[1]:
            return f'transfer: Ein Spiel mit der Nummer {number} gibt es nicht.'
        return None

    async def execute(self, command):
        # the menu becomes a published game once: from now on its music
        # and recordings come from that game's folder (as in line.py)
        if isinstance(command, dict) and 'game' in command:
            folder = str(command['game'])
            if self.switched or folder not in published()[0]:
                return {'error': 'no game'}
            self.switched = True
            self.folder = os.path.join(GAMES, os.path.basename(folder))
            self.log('game', os.path.basename(folder))
            return {'ok': True}
        return await super().execute(command)

    async def run(self):
        try:
            await self.setup()
            dialed = self.dialed
            # a game may forward the call to another game's number
            # (transfer): the menu starts again as if that number had been
            # dialled, five times at most, all within the call's time
            deadline = time.monotonic() + GAME_SECONDS
            for _ in range(1 + MAX_TRANSFERS):
                left = int(deadline - time.monotonic())
                if self.ended.is_set() or left <= 0:
                    break
                self.folder, self.switched, self.transfer_to = MENU_FOLDER, False, None
                self.mixer.set_music(None)
                env = game.environment({'PATH': '/usr/local/bin:/usr/bin:/bin', 'LANG': 'C.UTF-8', 'HOME': '/tmp',
                                        'PHONE_GAMES': GAMES, 'PHONE_DIALED': dialed})
                reason, status = await game.run_game(
                    self, MENU, MENU_FOLDER, env, lambda kind, line: log(f'[{self.channel} {kind}]', line),
                    preexec_fn=game_limits, seconds=left)
                self.log('game ended:', reason, status)
                if reason == 'time':
                    await self.say('Die Zeit ist um. Tschüss!')
                if reason != 'transfer':
                    break
                dialed = self.transfer_to
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
        # it only answers: never a call out, a recording or listening in
        if not ari_guard.allowed(method, path):
            raise RuntimeError(f'{method} {path}: the runner never makes a call, records or listens in')
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
                    call.dialed = direct_digits(event['channel'].get('dialplan', {}).get('exten', ''))
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
