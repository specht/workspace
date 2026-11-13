"""The public line of the Telefonbuch (docs/plan-telefonspiel.md, »Calling
from the Telefonbuch«): the phone of `telefon` (emulator/page.html with
?line) for everyone who visits /telefonbuch, many at once. Each visitor's
page has a session of its own; a call runs the menu (menu.py), which then
becomes the chosen game.

The sandbox: the container has no secrets and a network of its own without
the internet (only the speech service and nginx), the published games are
mounted read-only; each call's process runs as the unprivileged user game
with limits on processor time, memory and processes, at most 15 minutes.
PHONE_LINES calls at once on the whole line, any number of them from one
address (a class at school shares one).
Nothing about a visitor is kept.
"""
import asyncio
import importlib.machinery
import importlib.util
import os
import pwd
import re
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
MAX_TRANSFERS = 5                                       # how often one call may go from game to game
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


def trial_of(token):
    """the game a ticket of /telefon-freigabe lets one try before it is
    published (PhoneGames.trial: GAMES/trials.json), or None"""
    import json
    if not (16 <= len(token) <= 64 and token.isalnum()):
        return None
    try:
        with open(os.path.join(GAMES, 'trials.json')) as f:
            trial = json.load(f).get(token)
    except (OSError, ValueError, AttributeError):
        return None
    if not isinstance(trial, dict) or float(trial.get('expires', 0)) < time.time():
        return None
    folder, file = str(trial.get('folder', '')), os.path.basename(str(trial.get('file', '')))
    if not folder.startswith('games/') or os.path.splitext(file)[1] not in ('.py', '.rb', '.js'):
        return None
    return {'folder': folder, 'file': file}


def workspace_origin(origin, host):
    """origin is a Workspace's forwarded port of the site at host
    (https://<tag>-<port>.<site>, nginx_config.rb), nothing else"""
    import urllib.parse
    site = host.split(':')[0].lower()
    if not origin or not site:
        return False
    parts = urllib.parse.urlsplit(origin)
    name = (parts.hostname or '').lower()
    return parts.scheme in ('http', 'https') and bool(re.fullmatch(r'[a-z0-9]+-\d+\.' + re.escape(site), name))


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

    def transfer_problem(self, number):
        """a forwarded call goes only to a published game (its number, and
        what is dialled after it)"""
        import json
        try:
            with open(os.path.join(GAMES, 'published.json')) as f:
                numbers = {str(g.get('number')) for g in json.load(f).get('games', []) if isinstance(g, dict)}
        except (OSError, ValueError):
            numbers = set()
        if game.match_number(number, numbers)[0] is None:
            return f'transfer: Ein Spiel mit der Nummer {number} gibt es nicht.'
        return None

    def titled(self, title, subtitle):
        self.shown = title
        super().titled(title, subtitle)

    async def execute(self, command):
        if isinstance(command, dict) and 'game' in command:
            folder = str(command['game'])
            trial = self.server.trial and self.server.trial['folder'] == folder
            if self.switched or not (trial or folder in published_folders()):
                return {'error': 'no game'}
            self.switched = True
            self.server.folder = folder_path(folder)
            # what was dialled after the number: the first keys the game gets
            self.first_keys(str(command.get('extension', ''))[:game.MAX_DIALED])
            number = None if trial else game.published_number(GAMES, folder)
            if number:
                self.server.reached.append(number)
            return {'ok': True}
        return await super().execute(command)


class Line(emulator.Server):
    """one visitor's phone"""

    def __init__(self, ip):
        super().__init__(MENU, GAMES)
        self.ip = ip
        self.seen = time.monotonic()
        self.dialed = ''                                # the number dialled before the call, or ''
        self.trial = None                               # a game tried before it is published (trial_of)
        self.reached = []                               # the numbers of the games the call reached (record_call)

    def title(self):
        return ''                                       # the display says nothing until a game names itself

    def output(self, kind, line):
        if kind == 'err':
            print('game', kind, line, file=sys.stderr, flush=True)
            self.errors.append(line)
            del self.errors[:-12]

    async def connect(self, dialed, trial, depth):
        """one connection: the menu as if this number had been dialled, which
        becomes its game, and the games it forwards to (transfer, five times
        at most), each on a phone of its own (a new connection keeps no keys
        from before); all within the call's time. Returns why the last ended
        and its phone. A game that crashes: the caller hears so and is back
        in the menu, or (called by another game) back with that one"""
        reason, phone = 'time', None
        for _ in range(1 + MAX_TRANSFERS):
            # the menu's folder (its tones) until it becomes a game
            self.folder = HERE
            self.phone = phone = LinePhone(self)
            phone.depth = depth
            env = game.environment()
            env['PHONE_DIALED'] = dialed
            env['PHONE_DEPTH'] = str(depth)
            if trial:
                import json
                env['PHONE_TRIAL'] = json.dumps(trial)
            left = int(self.deadline - time.monotonic())
            if left <= 0:
                return 'time', phone
            self.errors = []
            status = None
            try:
                reason, status = await game.run_game(phone, self.command, HERE, env, self.output, preexec_fn=limits, seconds=left)
            except OSError:
                reason = 'ended'
            if reason == 'ended' and status and phone.switched and self.phone is phone and len(self.crashed) < 2:
                self.crashed.append({'game': self.reached[-1] if self.reached else '', 'text': '\n'.join(self.errors)})
                if depth:
                    await phone.say(game.CRASHED_CALLED)
                    return 'crashed', phone
                await phone.say(game.CRASHED)
                dialed, trial = '', None
                continue
            if reason != 'transfer' or self.phone is not phone:
                return reason, phone
            dialed, trial = phone.transfer_to, None
        return reason, phone

    async def call_from(self, holding, dialed):
        """the game on the phone holding calls this number: the next
        connection runs while it waits; afterwards it is on the line again
        (its folder, its display), and the answer is what it gets"""
        folder = self.folder
        reason, phone = await self.connect(dialed, None, holding.depth + 1)
        answer = phone.returned if reason == 'back' and phone is not None and phone.returned is not None else ''
        # the caller hung up meanwhile: the holding game ends with the call
        if phone is not None and phone.ended.is_set():
            holding.ended.set()
        if self.phone is phone or self.phone is None:
            self.phone = holding
        self.folder = folder
        holding.fresh_keys()
        if getattr(holding, 'shown', ''):
            self.send({'title': holding.shown})
        return answer

    async def run_call(self):
        dialed, trial = self.dialed, self.trial
        was_trial = bool(trial)
        self.deadline = time.monotonic() + SECONDS
        began, self.reached = time.monotonic(), []
        # the last lines a game wrote to stderr: its error, if it crashes
        self.errors, self.crashed = [], []
        reason, phone = await self.connect(dialed, trial, 0)
        if self.phone is phone:
            self.phone = None
            self.send({'ended': reason})
        # for /statistik (record_call); a game tried before it is published
        # is its author's, not a call
        if not was_trial:
            game.record_call('browser', self.reached, time.monotonic() - began, self.crashed)

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
            length, ip, origin, host = 0, '', '', ''
            while True:
                line = await asyncio.wait_for(reader.readline(), 30)
                if line in (b'\r\n', b'\n', b''):
                    break
                name, _, value = line.decode('latin-1').partition(':')
                if name.strip().lower() == 'content-length':
                    length = min(int(value.strip() or 0), 65536)
                elif name.strip().lower() == 'x-real-ip':
                    ip = value.strip()
                elif name.strip().lower() == 'origin':
                    origin = value.strip()
                elif name.strip().lower() == 'host':
                    host = value.strip()
            # the page of telefon in a Workspace of this site continues a
            # forwarded call here (page.html): only its origin may read. The
            # site's name comes from the configuration: nginx passes the
            # request on with the line's own name as its host
            site = os.environ.get('PHONE_SITE_HOST') or host
            emulator.ORIGIN.set(origin if workspace_origin(origin, site) else None)
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
            # the whole line's limit only: no limit per visitor's address,
            # which a class at school shares (the maintainer, 12 November 2026)
            busy = sum(1 for other in self.lines.values() if other.calling() and other is not line)
            if busy >= LINES:
                line.send({'ended': 'busy'})
                self.respond(writer, 204)
                return
            number = query.get('number', '')
            line.dialed = number if number.isdigit() and len(number) <= game.MAX_DIALED else ''
            line.trial = trial_of(query.get('try', ''))
        await line.route(method, path, query, body, reader, writer)


async def main():
    router = Router()
    asyncio.ensure_future(prepare())
    server = await asyncio.start_server(router.handle, '0.0.0.0', int(os.environ.get('PHONE_LINE_PORT', '8040')))
    async with server:
        await server.serve_forever()


if __name__ == '__main__':
    asyncio.run(main())
