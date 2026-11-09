"""The public line of the Telefonbuch (docs/plan-telefonspiel.md, »Calling
from the Telefonbuch«): the phone of `telefon` (emulator/page.html with
?line) for everyone who visits /telefonbuch, many at once. Each visitor's
page has a session of its own; a call runs the menu (menu.py), which then
becomes the chosen game.

The sandbox: the container has no secrets and a network of its own without
the internet (only the speech service and nginx), the published games are
mounted read-only; each call's process runs as the unprivileged user game
with limits on processor time, memory and processes, at most 15 minutes.
One call at a time per visitor (nginx's X-Real-IP), PHONE_LINES at once.
Nothing about a visitor is kept.
"""
import asyncio
import importlib.machinery
import importlib.util
import os
import pwd
import resource
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, os.path.join(ROOT, 'core'))
import game  # noqa: E402

# the emulator's phone, server and page, loaded from its script
_loader = importlib.machinery.SourceFileLoader('telefon_emulator', os.path.join(ROOT, 'emulator', 'telefon'))
emulator = importlib.util.module_from_spec(importlib.util.spec_from_loader(_loader.name, _loader))
_loader.exec_module(emulator)
emulator.show = lambda *args, **kwargs: None          # no terminal here: nothing to print

GAMES = os.environ.get('PHONE_GAMES', '/games')
LINES = int(os.environ.get('PHONE_LINES', '10'))       # calls at once
SECONDS = int(os.environ.get('PHONE_GAME_SECONDS', '900'))
MEMORY = 512 * 1024 * 1024
CPU = 120
RESCAN = 30                                             # seconds between looks for new game folders
IDLE = 3600                                             # a session without its page for this long goes
MENU = [sys.executable, os.path.join(HERE, 'menu.py')]


def limits():
    """in the call's process, before the menu starts: a session of its own,
    limits, the unprivileged user game (as phone-runner's runner does)"""
    os.setsid()
    resource.setrlimit(resource.RLIMIT_CPU, (CPU, CPU))
    resource.setrlimit(resource.RLIMIT_AS, (MEMORY, MEMORY))
    if os.getuid() == 0:
        # processes count per user: only for game, whose processes are the calls'
        resource.setrlimit(resource.RLIMIT_NPROC, (64, 64))
        user = pwd.getpwnam('game')
        os.setgroups([])
        os.setgid(user.pw_gid)
        os.setuid(user.pw_uid)


def published_folders():
    """the folders the menu may switch to: those of the published list"""
    import json
    try:
        with open(os.path.join(GAMES, 'published.json')) as f:
            return {g.get('folder') for g in json.load(f).get('games', []) if isinstance(g, dict)}
    except (OSError, ValueError):
        return set()


def folder_path(folder):
    return os.path.join(GAMES, os.path.basename(folder))


class Preparer(emulator.Server):
    """one folder's sentences and music made before anyone calls, with the
    emulator's watcher; the folders take turns at the speech service"""
    turn = None

    async def render(self, sentences):
        async with Preparer.turn:
            await super().render(sentences)


async def prepare():
    """every game folder there is: the copies in PHONE_GAMES (as soon as a
    game is submitted, so the review and the first call find them ready)
    and the menu; a folder that goes is dropped"""
    Preparer.turn = asyncio.Semaphore(1)
    watchers = {}
    while True:
        folders = {HERE}
        try:
            folders.update(os.path.join(GAMES, n) for n in os.listdir(GAMES) if os.path.isdir(os.path.join(GAMES, n)))
        except OSError:
            pass
        for folder in folders - watchers.keys():
            watchers[folder] = asyncio.ensure_future(Preparer(MENU, folder).watch_music())
        for folder in watchers.keys() - folders:
            watchers.pop(folder).cancel()
        await asyncio.sleep(RESCAN)


class LinePhone(emulator.Emulator):
    """the emulator's phone; the menu tells it once which game it became"""

    def __init__(self, server):
        super().__init__(server)
        self.switched = False

    async def execute(self, command):
        if isinstance(command, dict) and 'game' in command:
            folder = str(command['game'])
            if self.switched or folder not in published_folders():
                return {'error': 'no game'}
            self.switched = True
            self.server.folder = folder_path(folder)
            return {'ok': True}
        return await super().execute(command)


class Line(emulator.Server):
    """one visitor's phone"""

    def __init__(self, ip):
        super().__init__(MENU, GAMES)
        self.ip = ip
        self.seen = time.monotonic()
        self.dialed = ''                                # the number dialled before the call, or ''

    def title(self):
        return ''                                       # the display says nothing until a game names itself

    async def run_call(self):
        self.folder = GAMES
        self.phone = phone = LinePhone(self)
        env = game.environment()
        env['PHONE_DIALED'] = self.dialed
        try:
            reason, _ = await game.run_game(phone, self.command, HERE, env, lambda kind, line: print('game', kind, line, file=sys.stderr, flush=True) if kind == 'err' else None,
                                            preexec_fn=limits, seconds=SECONDS)
        except OSError:
            reason = 'ended'
        if self.phone is phone:
            self.phone = None
            self.send({'ended': reason})

    def calling(self):
        return self.call is not None and not self.call.done()


class Router(emulator.Server):
    """the one server for all visitors: the page, and each request to its session"""

    def __init__(self):
        super().__init__(MENU, GAMES)
        self.lines = {}

    async def handle(self, reader, writer):
        # the emulator's handle, keeping the visitor's address from nginx
        import urllib.parse
        try:
            request = await asyncio.wait_for(reader.readline(), 30)
            parts = request.decode('latin-1').split()
            if len(parts) < 2:
                return
            method, target = parts[0], parts[1]
            length, ip = 0, ''
            while True:
                line = await asyncio.wait_for(reader.readline(), 30)
                if line in (b'\r\n', b'\n', b''):
                    break
                name, _, value = line.decode('latin-1').partition(':')
                if name.strip().lower() == 'content-length':
                    length = min(int(value.strip() or 0), 65536)
                elif name.strip().lower() == 'x-real-ip':
                    ip = value.strip()
            body = await reader.readexactly(length) if length else b''
            url = urllib.parse.urlsplit(target)
            query = dict(urllib.parse.parse_qsl(url.query))
            await self.dispatch(method, url.path, query, body, reader, writer, ip)
        except (ConnectionError, asyncio.TimeoutError, asyncio.IncompleteReadError, ValueError):
            pass
        finally:
            try:
                writer.close()
            except RuntimeError:
                pass

    def prune(self):
        now = time.monotonic()
        for key, line in list(self.lines.items()):
            if line.events is None and not line.calling() and now - line.seen > IDLE:
                del self.lines[key]

    async def dispatch(self, method, path, query, body, reader, writer, ip):
        if method == 'GET' and path == '/':
            await self.route(method, path, query, body, reader, writer)
            return
        session = query.get('s', '')
        if not (8 <= len(session) <= 64 and session.isalnum()):
            self.respond(writer, 404, b'no session')
            return
        self.prune()
        line = self.lines.get(session)
        if line is None:
            if path != '/events':
                self.respond(writer, 404, b'no session')
                return
            line = self.lines[session] = Line(ip)
        line.seen = time.monotonic()
        if method == 'POST' and path == '/call':
            busy = sum(1 for other in self.lines.values() if other.calling() and other is not line)
            mine = any(other.calling() and other.ip == line.ip and other is not line for other in self.lines.values())
            if mine or busy >= LINES:
                line.send({'ended': 'one' if mine else 'busy'})
                self.respond(writer, 204)
                return
            number = query.get('number', '')
            line.dialed = number if number.isdigit() and len(number) <= 4 else ''
        await line.route(method, path, query, body, reader, writer)


async def main():
    router = Router()
    asyncio.ensure_future(prepare())
    server = await asyncio.start_server(router.handle, '0.0.0.0', int(os.environ.get('PHONE_LINE_PORT', '8040')))
    async with server:
        await server.serve_forever()


if __name__ == '__main__':
    asyncio.run(main())
