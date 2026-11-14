"""Unpack a ZIP file into a folder, refusing what does not belong in an
exam package: a path out of the folder (absolute, ..), a link, more than
MAX_FILES files or MAX_BYTES bytes unpacked. Nothing is written unless the
whole archive passes. For a ZIP dropped into the exam dialog
(exam_builder.rb, ExamBuilder.stage_archive).

    python3 -I unzip_safe.py ARCHIVE FOLDER

Exit 0, or 2 with the reason (German, for the teacher) on stderr."""
import os
import stat
import sys
import zipfile

MAX_FILES = 10000
MAX_BYTES = 100 * 1024 * 1024


def fail(message):
    sys.stderr.write(message + '\n')
    sys.exit(2)


def main(archive, dest):
    try:
        z = zipfile.ZipFile(archive)
    except zipfile.BadZipFile:
        fail('Die ZIP-Datei lässt sich nicht öffnen.')
    infos = z.infolist()
    if len(infos) > MAX_FILES:
        fail(f'Mehr als {MAX_FILES} Dateien im Archiv.')
    if sum(i.file_size for i in infos) > MAX_BYTES:
        fail(f'Ausgepackt mehr als {MAX_BYTES // 1024 // 1024} MB.')
    for i in infos:
        name = i.filename
        parts = name.replace('\\', '/').split('/')
        if name.startswith('/') or '..' in parts or ':' in parts[0]:
            fail(f'Unsicherer Pfad im Archiv: {name}')
        mode = i.external_attr >> 16
        if mode and stat.S_ISLNK(mode):
            fail(f'Links gehen nicht in ein Paket: {name}')
    for i in infos:
        target = os.path.join(dest, i.filename.replace('\\', '/'))
        if i.is_dir():
            os.makedirs(target, exist_ok=True)
            continue
        os.makedirs(os.path.dirname(target), exist_ok=True)
        with z.open(i) as src, open(target, 'wb') as out:
            while True:
                chunk = src.read(65536)
                if not chunk:
                    break
                out.write(chunk)


if __name__ == '__main__':
    if len(sys.argv) != 3:
        fail('usage: unzip_safe.py ARCHIVE FOLDER')
    main(sys.argv[1], sys.argv[2])
