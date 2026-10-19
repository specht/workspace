"""Folders a tool makes and one command makes again: what »Meine Daten
herunterladen« leaves out unless asked (zip_stream.py), and what »Platz
schaffen« in the profile offers to delete. Run as the Workspace user, with
python3 -I, never as root:

    python3 -I rebuildable.py scan FOLDER          JSON list, one entry each
    python3 -I rebuildable.py delete FOLDER PATH…  deletes these, if they still
                                                   are rebuildable folders

The folder is a student's: the walk never follows a link, and a path to
delete must be one the scan finds at that moment.
"""

import json
import os
import shutil
import stat
import sys

# Folders a tool makes and one command makes again, with that command.
# Some names are certain; others count only next to the project file that
# makes them, since a student may have a folder of their own named »build«.
# The text says only what is certain: what the student types, or what
# happens by itself where the code that does it has been read (bif-watcher
# runs npm ci when a BIF project is opened). Never »von selbst« on a guess.
ALWAYS = {
    'node_modules': 'npm install',   # npm ci with a lock file, see command_for
    '__pycache__': 'ein Zwischenspeicher von Python; ohne ihn läuft alles wie vorher',
    '.gradle': 'ein Zwischenspeicher von Gradle; gradle build legt ihn wieder an',
    '.dart_tool': 'flutter pub get',
    '.pub-cache': 'flutter pub get',
}
NEXT_TO = {
    'build': (('pubspec.yaml', 'flutter build web (oder das Ziel, das du gebaut hast)'), ('build.gradle', 'gradle build'),
              ('build.gradle.kts', 'gradle build'), ('CMakeLists.txt', 'cmake -B build, dann cmake --build build')),
    'target': (('Cargo.toml', 'cargo build'), ('pom.xml', 'mvn package')),
}
LOCK_FILES = ('package-lock.json', 'npm-shrinkwrap.json')
# A file in node_modules written this long after npm's record of the install
# (node_modules/.package-lock.json) was written afterwards.
NPM_SLACK = 120


def command_for(name, command, siblings):
    """The command, precise for node_modules: npm ci brings back the same
    versions from the lock file; a BIF project's extension runs it when the
    folder is opened (src/vscode-extensions/bif-watcher)."""
    if name != 'node_modules' or not (set(LOCK_FILES) & siblings):
        return command
    if '.bif-project' in siblings:
        return 'npm ci; die BIF-Erweiterung macht das, wenn du den Ordner in VS Code öffnest'
    return 'npm ci'


def in_tool_folder(rel):
    """Whether rel (relative to the Workspace) lies inside a hidden folder at
    its top: .extensions, .local, .config belong to VS Code and the tools,
    not to a project. Nothing in there counts as rebuildable, not even an
    extension's node_modules, which npm install would not bring back."""
    first = rel.split(os.sep, 1)[0]
    return first != '.' and first.startswith('.')


def rebuildable(dir_fd, name, siblings):
    """The command that makes this folder again, or None to keep it."""
    if name in ALWAYS:
        return ALWAYS[name]
    for marker, command in NEXT_TO.get(name, ()):
        if marker in siblings:
            return command
    if name in ('.venv', 'venv', 'env'):
        try:
            os.stat(os.path.join(name, 'pyvenv.cfg'), dir_fd=dir_fd, follow_symlinks=False)
            return 'python3 -m venv ' + name + ', dann die Pakete neu installieren'
        except OSError:
            return None
    return None




# Files that tools write into node_modules as a matter of course do not
# count as changed.
NPM_OWN = {'.package-lock.json', '.modules.yaml', '.yarn-integrity'}
NPM_OWN_DIRS = {'.bin', '.cache', '.vite', '.vite-temp', '.svelte-kit'}

# What a folder needs beside it so that its command can make it again.
NEEDS = {
    'node_modules': ('package.json',),
    '.dart_tool': ('pubspec.yaml',),
    '.pub-cache': ('pubspec.yaml',),
}


def size_and_changed(dir_fd, name, check_npm):
    """Bytes on disk below the folder; for node_modules also the first file
    written after npm's record of the install, and whether that record
    exists (npm 7 and later write node_modules/.package-lock.json last)."""
    total, changed, installed = 0, None, None
    top = os.open(name, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=dir_fd)
    if check_npm:
        try:
            installed = os.stat('.package-lock.json', dir_fd=top, follow_symlinks=False).st_mtime
        except OSError:
            installed = None
    try:
        for folder, dirs, files, fd in os.fwalk('.', dir_fd=top, follow_symlinks=False):
            parts = folder.split(os.sep)
            own = check_npm and any(p in NPM_OWN_DIRS for p in parts)
            for f in files:
                try:
                    st = os.stat(f, dir_fd=fd, follow_symlinks=False)
                except OSError:
                    continue
                total += st.st_blocks * 512
                if (installed is not None and changed is None and not own and stat.S_ISREG(st.st_mode)
                        and f not in NPM_OWN and st.st_mtime > installed + NPM_SLACK and folder != '.'):
                    changed = os.path.normpath(os.path.join(folder, f))
    finally:
        os.close(top)
    return total, changed, installed is not None


def scan(top):
    found = []
    for folder, dirs, files, dir_fd in os.fwalk(top, follow_symlinks=False):
        rel = os.path.relpath(folder, top)
        siblings = set(files) | set(dirs)
        if rel == '.':
            # The tools' hidden folders (in_tool_folder) are not walked at
            # all, unless one is itself rebuildable (a .venv at the top).
            dirs[:] = [d for d in dirs if not d.startswith('.') or rebuildable(dir_fd, d, siblings)]
        for name in list(dirs):
            command = rebuildable(dir_fd, name, siblings)
            if not command:
                continue
            dirs.remove(name)
            path = name if rel == '.' else os.path.join(rel, name)
            npm = name == 'node_modules'
            try:
                size, changed, recorded = size_and_changed(dir_fd, name, npm)
            except OSError:
                continue
            command = command_for(name, command, siblings)
            missing = [m for m in NEEDS.get(name, ()) if m not in siblings]
            # Ticked only where the command certainly brings back the same.
            note = None
            if missing:
                note = f'{missing[0]} fehlt daneben: Es lässt sich nicht sicher neu anlegen.'
            elif npm and not recorded:
                note = 'npm hat hier nicht vermerkt, was es installiert hat: Ob alles wiederkommt, ist nicht sicher.'
            elif npm and changed:
                note = f'Darin wurde nach dem Installieren etwas geändert ({changed}).'
            elif npm and not (set(LOCK_FILES) & siblings):
                note = 'Ohne package-lock.json installiert npm vielleicht andere Versionen als vorher.'
            elif name in ('.venv', 'venv', 'env'):
                note = ('Welche Pakete in welcher Version darin waren, steht nur in der Umgebung selbst. '
                        'Prüfe vorher, ob requirements.txt sie alle nennt.')
            found.append({'path': path, 'command': command, 'bytes': size, 'tick': note is None, 'note': note})
    return sorted(found, key=lambda f: -f['bytes'])


def delete(top, paths):
    current = {f['path'] for f in scan(top)}
    deleted = []
    for path in paths:
        if path not in current:
            continue
        target = os.path.join(top, path)
        if os.path.islink(target) or not os.path.isdir(target):
            continue
        shutil.rmtree(target)
        deleted.append(path)
    return deleted


def main(args):
    command, top = args[0], args[1]
    if command == 'scan':
        print(json.dumps(scan(top)))
    elif command == 'delete':
        print(json.dumps(delete(top, args[2:])))
    else:
        sys.exit('scan oder delete')


if __name__ == '__main__':
    main(sys.argv[1:])
