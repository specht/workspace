"""Write a ZIP of one or more folders to stdout, as it goes (»Meine Daten
herunterladen«, AccountExport). Run with python3 -I and, for a student's
folder, as the Workspace user, not as root:

    python3 -I zip_stream.py [--exclude workspace/.cache] [--skip-rebuildable]
        [--skipped-note NAME] PREFIX=FOLDER ...

PREFIX is the path in the archive ('' for the top). --skip-rebuildable leaves
out folders that tools make and that one command makes again
(node_modules, build output, Python environments; REBUILDABLE below); the
archive then ends with a file NAME that lists them, with the command. The folders may be
controlled by a student, who can put a link or a FIFO anywhere, also while
this runs, so: the walk never follows a link (os.fwalk), every file is
opened relative to its folder with O_NOFOLLOW and checked to be a regular
file after opening, links are stored as links, anything else is skipped.
"""

import os
import stat
import sys
import time
import zipfile

CHUNK = 1 << 16

# Already compressed: stored as they are, deflating them again costs time
# and saves nothing. Everything else gets the fastest level: a Workspace of
# several gigabytes goes out several times faster for a slightly bigger file.
STORED = {
    '.png', '.jpg', '.jpeg', '.gif', '.webp', '.avif', '.mp4', '.webm', '.mov', '.mkv', '.mp3', '.ogg', '.m4a',
    '.zip', '.gz', '.tgz', '.bz2', '.xz', '.zst', '.7z', '.rar', '.jar', '.apk', '.aab', '.whl', '.pdf', '.woff', '.woff2',
}
LEVEL = 1

# Folders a tool makes and one command makes again, with that command.
# Some names are certain; others count only next to the project file that
# makes them, since a student may have a folder of their own named »build«.
ALWAYS = {
    'node_modules': 'npm install',
    '__pycache__': 'entsteht beim Ausführen von selbst',
    '.gradle': 'entsteht beim Bauen von selbst',
    '.dart_tool': 'flutter pub get',
    '.pub-cache': 'flutter pub get',
}
NEXT_TO = {
    'build': (('pubspec.yaml', 'flutter build'), ('build.gradle', 'gradle build'), ('build.gradle.kts', 'gradle build'),
              ('CMakeLists.txt', 'cmake --build build')),
    'target': (('Cargo.toml', 'cargo build'), ('pom.xml', 'mvn package')),
}


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
            return 'python3 -m venv ' + name + ' und die Pakete neu installieren'
        except OSError:
            return None
    return None


def zip_time(seconds):
    t = time.localtime(max(seconds, 315532800))  # ZIP cannot go before 1980
    return t[:6]


def add_tree(archive, prefix, top, excludes, skip_rebuildable, skipped):
    for folder, dirs, files, dir_fd in os.fwalk(top, follow_symlinks=False):
        rel = os.path.relpath(folder, top)
        base = prefix if rel == '.' else os.path.join(prefix, rel) if prefix else rel
        if any(base == e or base.startswith(e + '/') for e in excludes):
            dirs[:] = []
            continue
        if skip_rebuildable:
            siblings = set(files) | set(dirs)
            for name in list(dirs):
                command = rebuildable(dir_fd, name, siblings)
                if command:
                    dirs.remove(name)
                    skipped.append((os.path.join(base, name) if base else name, command))
        if base:
            info = zipfile.ZipInfo(base.rstrip('/') + '/', zip_time(os.stat(dir_fd).st_mtime))
            info.external_attr = (stat.S_IFDIR | 0o755) << 16
            archive.writestr(info, b'')
        for name in sorted(files + [d for d in dirs if os.path.islink(os.path.join(folder, d))]):
            path = os.path.join(base, name) if base else name
            try:
                st = os.stat(name, dir_fd=dir_fd, follow_symlinks=False)
            except OSError:
                continue
            if stat.S_ISLNK(st.st_mode):
                info = zipfile.ZipInfo(path, zip_time(st.st_mtime))
                info.external_attr = (stat.S_IFLNK | 0o777) << 16
                archive.writestr(info, os.readlink(name, dir_fd=dir_fd))
                continue
            if not stat.S_ISREG(st.st_mode):
                continue
            try:
                fd = os.open(name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=dir_fd)
            except OSError:
                continue
            with os.fdopen(fd, 'rb') as source:
                opened = os.fstat(fd)
                if not stat.S_ISREG(opened.st_mode):
                    continue
                info = zipfile.ZipInfo(path, zip_time(opened.st_mtime))
                info.external_attr = (stat.S_IFREG | (opened.st_mode & 0o777)) << 16
                if os.path.splitext(name)[1].lower() in STORED:
                    info.compress_type = zipfile.ZIP_STORED
                else:
                    info.compress_type = zipfile.ZIP_DEFLATED
                    info._compresslevel = LEVEL
                with archive.open(info, 'w', force_zip64=opened.st_size > (1 << 30)) as target:
                    while True:
                        chunk = source.read(CHUNK)
                        if not chunk:
                            break
                        target.write(chunk)


def main(args):
    excludes = []
    trees = []
    skip_rebuildable = False
    note = 'weggelassen.txt'
    while args:
        arg = args.pop(0)
        if arg == '--exclude':
            excludes.append(args.pop(0).strip('/'))
        elif arg == '--skip-rebuildable':
            skip_rebuildable = True
        elif arg == '--skipped-note':
            note = args.pop(0)
        else:
            prefix, _, folder = arg.partition('=')
            trees.append((prefix.strip('/'), folder))
    skipped = []
    with zipfile.ZipFile(sys.stdout.buffer, 'w', zipfile.ZIP_DEFLATED, allowZip64=True, compresslevel=LEVEL) as archive:
        for prefix, folder in trees:
            if os.path.isdir(folder):
                add_tree(archive, prefix, folder, excludes, skip_rebuildable, skipped)
        if skipped:
            lines = ['Diese Ordner fehlen im Archiv. Werkzeuge legen sie an, und ein Befehl im',
                     'jeweiligen Projektordner legt sie wieder an. Mit allen Ordnern gibt es das',
                     'Archiv im Profil unter »Meine Daten«, wenn du das Häkchen dafür setzt.', '']
            lines += [f'{path}/  –  {command}' for path, command in skipped]
            archive.writestr(note, '\n'.join(lines) + '\n')


if __name__ == '__main__':
    main(sys.argv[1:])
