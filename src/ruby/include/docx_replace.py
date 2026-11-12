# A copy of a .docx with pieces of its text replaced (DocumentDates, the
# dates of a starting file filled in on download). Run by the Ruby app:
#
#     python3 -I docx_replace.py FILE < {"2. März": "12. November"}
#
# writes the new file to stdout. Only the text of the body, headers and
# footers changes (word/document.xml, word/header*.xml, word/footer*.xml);
# a piece must stand inside one run, as the scripts that make the starting
# files write it (dokumente.py). Every other entry is copied as it is.

import io
import json
import re
import sys
import zipfile
from xml.sax.saxutils import escape

PARTS = re.compile(r'word/(document|header\d*|footer\d*)\.xml')


def main():
    replacements = {escape(old): escape(new) for old, new in json.load(sys.stdin).items()}
    # all in one pass, so that a new date is never replaced again (on
    # 9 March »2. März« becomes »9. März«, itself a piece to replace);
    # »2. März« is not the end of »12. März«
    pattern = re.compile(r'(?<![0-9])(?:' + '|'.join(re.escape(old) for old in sorted(replacements, key=len, reverse=True)) + ')')
    out = io.BytesIO()
    with zipfile.ZipFile(sys.argv[1]) as source, zipfile.ZipFile(out, 'w') as target:
        for info in source.infolist():
            data = source.read(info)
            if PARTS.fullmatch(info.filename):
                text = data.decode('utf-8')
                if replacements:
                    text = pattern.sub(lambda m: replacements[m.group(0)], text)
                data = text.encode('utf-8')
            target.writestr(info, data)
    sys.stdout.buffer.write(out.getvalue())


if __name__ == '__main__':
    main()
