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
import numpy as np

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
    # whole number, never a game's alone. Digits of any length go to the
    # menu, which says »Kein Anschluss« for a number without a game
    number, line = game.international(exten), game.international(LINE_NUMBER)
    return number[len(line):][:game.MAX_DIALED] if number.startswith(line) else ''


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
        self.reached = []           # the numbers of the games the call reached (record_call)
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
        # its address on the network it shares with Asterisk (PHONE_RTP_HOST,
        # its alias there; on several networks the host name is ambiguous)
        host = socket.gethostbyname(os.environ.get('PHONE_RTP_HOST') or socket.gethostname())
        media = await self.ari('POST', '/channels/externalMedia', app=APP,
                               external_host=f'{host}:{self.port}', format='g722',
                               encapsulation='rtp', transport='udp', direction='both')
        self.media_channel = media['id']
        self.runner.media_channels.add(self.media_channel)
        vars_ = media.get('channelvars', {})
        remote = (vars_['UNICASTRTP_LOCAL_ADDRESS'], int(vars_['UNICASTRTP_LOCAL_PORT']))
        bridge = await self.ari('POST', '/bridges', type='mixing')
        self.bridge = bridge['id']
        # the stream enters the app a moment after it was made (the first
        # call after Asterisk started was refused, 422): a few tries
        for attempt in range(20):
            try:
                await self.ari('POST', f'/bridges/{self.bridge}/addChannel', channel=f'{self.channel},{self.media_channel}')
                break
            except RuntimeError as error:
                if ' 422 ' not in str(error) or attempt == 19:
                    raise
                await asyncio.sleep(0.05)
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

    async def play(self, source, interrupt=False):
        """a recording of the game's folder (or the menu's: its tones) in
        place of the voice; returns when it has been heard, like say"""
        self.log('play', source)
        try:
            samples = await sounds.music(self.runner.session, self.folder, str(source))
        except Exception as error:
            self.log('play failed:', repr(error))
            return
        if self.ended.is_set():
            return
        self.cut = False
        self.interruptible = interrupt
        try:
            await self.mixer.say(samples)
        finally:
            self.interruptible = False

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
        """a forwarded call goes only to a published game (its number, and
        what is dialled after it)"""
        if game.match_number(number, published()[1])[0] is None:
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
            # what was dialled after the number: the first keys the game gets
            self.first_keys(str(command.get('extension', ''))[:game.MAX_DIALED])
            number = game.published_number(GAMES, folder)
            if number:
                self.reached.append(number)
            return {'ok': True}
        return await super().execute(command)

    def output(self, kind, line):
        log(f'[{self.channel} {kind}]', line)
        if kind == 'err':
            self.errors.append(line)
            del self.errors[:-12]

    async def connect(self, dialed):
        """one connection: the menu as if this number had been dialled, which
        becomes its game, and the games it forwards to (transfer, five times
        at most); all within the call's time. Returns why the last ended.
        A game that crashes: the caller hears so and is back in the menu, or
        (called by another game) back with that one"""
        reason = 'time'
        for _ in range(1 + MAX_TRANSFERS):
            left = int(self.deadline - time.monotonic())
            if self.ended.is_set() or left <= 0:
                break
            self.folder, self.switched, self.transfer_to, self.returned = MENU_FOLDER, False, None, None
            self.fresh_keys()
            self.mixer.set_music(None)
            env = game.environment({'PATH': '/usr/local/bin:/usr/bin:/bin', 'LANG': 'C.UTF-8', 'HOME': '/tmp',
                                    'PHONE_GAMES': GAMES, 'PHONE_DIALED': dialed, 'PHONE_DEPTH': str(self.depth)})
            self.errors = []
            reason, status = await game.run_game(
                self, MENU, MENU_FOLDER, env, self.output, preexec_fn=game_limits, seconds=left)
            self.log('game ended:', reason, status, 'depth', self.depth)
            if reason == 'time' and self.depth == 0:
                await self.say('Die Zeit ist um. Tschüss!')
            # a game that stopped with an error (not the menu itself
            # crashing, and twice at most)
            if reason == 'ended' and status and self.switched and not self.ended.is_set() and len(self.crashed) < 2:
                self.crashed.append({'game': self.reached[-1] if self.reached else '', 'text': '\n'.join(self.errors)})
                self.mixer.set_music(None)
                if self.depth:
                    await self.say(game.CRASHED_CALLED)
                    return 'crashed'
                await self.say(game.CRASHED)
                dialed = ''
                continue
            if reason != 'transfer':
                return reason
            dialed = self.transfer_to
        return reason

    async def dtmf(self, digits):
        self.log('tones', digits)
        heard = self.mixer.say(np.array(game.tones(digits), dtype=np.float32))
        await heard

    async def call_game(self, dialed):
        """this game on hold (its folder, its music, its keys), the called one
        on the line; afterwards this one again, with only the answer"""
        held = (self.folder, self.switched, self.mixer.music, self.mixer.music_volume)
        self.depth += 1
        try:
            reason = await self.connect(dialed)
            answer = self.returned if reason == 'back' and self.returned is not None else ''
        finally:
            self.depth -= 1
            self.folder, self.switched = held[0], held[1]
            self.transfer_to, self.returned = None, None
            self.fresh_keys()
            if held[2] is not None:
                self.mixer.set_music(held[2], held[3])
            else:
                self.mixer.set_music(None)
        self.log('call answered', repr(answer))
        return answer

    async def run(self):
        began = None
        self.crashed = []
        self.errors = []
        try:
            await self.setup()
            began = time.monotonic()
            self.deadline = time.monotonic() + GAME_SECONDS
            await self.connect(self.dialed)
            if not self.ended.is_set():
                self.mixer.say(silence(600))
                await asyncio.sleep(0.6)
        except Exception as error:
            log('call failed:', repr(error))
        finally:
            await self.teardown()
            if began is not None:
                game.record_call('telefon', self.reached, time.monotonic() - began, self.crashed)


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
