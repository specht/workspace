"""Write a ZIP of one or more folders to stdout, as it goes (»Meine Daten
herunterladen«, AccountExport). Run with python3 -I and, for a student's
folder, as the Workspace user, not as root:

    python3 -I zip_stream.py [--exclude workspace/.cache] PREFIX=FOLDER ...

PREFIX is the path in the archive ('' for the top). The folders may be
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


def zip_time(seconds):
    t = time.localtime(max(seconds, 315532800))  # ZIP cannot go before 1980
    return t[:6]


def add_tree(archive, prefix, top, excludes):
    for folder, dirs, files, dir_fd in os.fwalk(top, follow_symlinks=False):
        rel = os.path.relpath(folder, top)
        base = prefix if rel == '.' else os.path.join(prefix, rel) if prefix else rel
        if any(base == e or base.startswith(e + '/') for e in excludes):
            dirs[:] = []
            continue
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
                info.compress_type = zipfile.ZIP_DEFLATED
                with archive.open(info, 'w', force_zip64=opened.st_size > (1 << 30)) as target:
                    while True:
                        chunk = source.read(CHUNK)
                        if not chunk:
                            break
                        target.write(chunk)


def main(args):
    excludes = []
    trees = []
    while args:
        arg = args.pop(0)
        if arg == '--exclude':
            excludes.append(args.pop(0).strip('/'))
        else:
            prefix, _, folder = arg.partition('=')
            trees.append((prefix.strip('/'), folder))
    with zipfile.ZipFile(sys.stdout.buffer, 'w', zipfile.ZIP_DEFLATED, allowZip64=True) as archive:
        for prefix, folder in trees:
            if os.path.isdir(folder):
                add_tree(archive, prefix, folder, excludes)


if __name__ == '__main__':
    main(sys.argv[1:])
