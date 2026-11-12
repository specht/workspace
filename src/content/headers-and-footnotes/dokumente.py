# The documents of »Kopfzeile, Fußzeile, Fußnoten«: the 7b's travel report as
# »Seiten und Verzeichnisse« left it (page breaks, a table of contents), with
# what is still typed by hand (the starting file), and built with a header, a
# footer and footnotes (the solution and a stage of task-examples.yaml). Run
# in the office image, after src/content/pages-and-contents/dokumente.py:
#
#     src/scripts/office python3 src/content/headers-and-footnotes/dokumente.py
#
# The starting file: »Reisebericht der 7b« typed as the first line of every
# day's page; »Seite 1« typed in the footer, the same on every page; two notes
# numbered by hand, »Karabiner¹« in the text and »¹ Ein Karabiner …« as a
# small paragraph at the end of the day.
#
# The solution: the name in the header with »Stand:« and the date as a field
# that updates; the page number as a field in the footer; the two notes as
# footnotes the program numbers. The stage »datum-fest«: the same with a date
# that does not update.

import os
import shutil
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
sys.path.insert(0, os.path.join(HERE, '..', 'pictures-in-text'))
from page_picture import page_picture as draw_page  # noqa: E402

STATIC = os.path.join(HERE, '..', '..', 'static', 'headers-and-footnotes')
STARTER = '08 Reisebericht.docx'
REPORT = os.path.join(HERE, '..', 'pages-and-contents', 'stufen', 'reisebericht.docx')
HEAD = 'Reisebericht der 7b'
# (the paragraph it is in, the word, its note)
NOTES = [
    ('Vorher gab es eine Einweisung', 'Karabiner',
     'Ein Karabiner ist ein Haken mit einem Verschluss, der nicht von selbst aufgeht.'),
    ('Zurück ging es über den Moorpfad', 'Moorpfad',
     'Ein Moorpfad ist ein Weg aus Holzbrettern, auf dem man durch ein Moor gehen kann, ohne einzusinken.'),
]
SUPERSCRIPT = '¹²³⁴⁵⁶⁷⁸⁹'
DAYS = ('Montag: Anreise', 'Dienstag: Kletterwald', 'Mittwoch: Wanderung', 'Donnerstag: Schifffahrt', 'Freitag: Rückfahrt')


def enum(name, value):
    import uno
    return uno.Enum(name, value)


def page_styles(doc):
    family = doc.StyleFamilies.getByName('PageStyles')
    return [family.getByName(n) for n in family.getElementNames() if family.getByName(n).isInUse()]


def starter(path):
    import uno
    import dokumente as d
    from com.sun.star.text.ControlCharacter import PARAGRAPH_BREAK
    process, profile, desktop = d.office()
    try:
        doc = desktop.loadComponentFromURL(uno.systemPathToFileUrl(os.path.abspath(REPORT)), '_blank', 0,
                                           (d.prop('Hidden', True),))
        text = doc.Text
        # the name typed as the first line of each day's page: the page
        # break moves from the heading to that line
        for day in DAYS:
            heading = next(p for p in d.paragraphs(doc) if p.getString() == day)
            cursor = text.createTextCursorByRange(heading.getStart())
            text.insertString(cursor, HEAD, False)
            text.insertControlCharacter(cursor, PARAGRAPH_BREAK, False)
            # the new line is the paragraph right before the day's heading
            ps = list(d.paragraphs(doc))
            line = ps[[q.getString() for q in ps].index(day) - 1]
            line.ParaStyleName = 'Standard'
            line.BreakType = enum('com.sun.star.style.BreakType', 'PAGE_BEFORE')
            line.ParaAdjust = enum('com.sun.star.style.ParagraphAdjust', 'RIGHT')
            line.CharHeight = 10
            line.CharColor = 0x6B7280
            heading = next(p for p in d.paragraphs(doc) if p.getString() == day)
            heading.ParaStyleName = 'Heading 1'
            heading.BreakType = enum('com.sun.star.style.BreakType', 'NONE')
        # the notes numbered by hand
        for n, (start, word, note) in enumerate(NOTES):
            p = next(p for p in d.paragraphs(doc) if p.getString().startswith(start))
            at = p.getString().index(word) + len(word)
            cursor = text.createTextCursorByRange(p.getStart())
            cursor.goRight(at, False)
            text.insertString(cursor, SUPERSCRIPT[n], False)
            cursor = text.createTextCursorByRange(p.getEnd())
            text.insertControlCharacter(cursor, PARAGRAPH_BREAK, False)
            text.insertString(cursor, f'{SUPERSCRIPT[n]} {note}', False)
            typed = next(q for q in d.paragraphs(doc) if q.getString().startswith(SUPERSCRIPT[n] + ' '))
            typed.ParaStyleName = 'Standard'
            typed.CharHeight = 10
        # »Seite 1« typed in the footer
        for st in page_styles(doc):
            st.FooterIsOn = True
            st.FooterText.setString('Seite 1')
        doc.getDocumentIndexes().getByIndex(0).update()
        doc.storeToURL(uno.systemPathToFileUrl(os.path.abspath(path)), (d.prop('FilterName', 'MS Word 2007 XML'),))
        doc.close(True)
    finally:
        process.kill()
        subprocess.run(['pkill', '-f', 'pipe,name=pictures_in_text'], capture_output=True)
        shutil.rmtree(profile, ignore_errors=True)


def solution(path, fixed=False):
    import uno
    import dokumente as d
    process, profile, desktop = d.office()
    try:
        doc = desktop.loadComponentFromURL(uno.systemPathToFileUrl(os.path.abspath(os.path.join(STATIC, STARTER))),
                                           '_blank', 0, (d.prop('Hidden', True),))
        text = doc.Text
        # the typed names go, the page breaks go back to the headings
        for day in DAYS:
            # (not the title on the first page, which reads the same)
            line = next(p for p in d.paragraphs(doc) if p.getString() == HEAD and p.ParaStyleName != 'Title')
            d.remove_paragraph(doc, line)
            heading = next(p for p in d.paragraphs(doc) if p.getString() == day)
            heading.ParaStyleName = 'Heading 1'
            heading.BreakType = enum('com.sun.star.style.BreakType', 'PAGE_BEFORE')
        # the notes as footnotes, the typed ones go
        for n, (start, word, note) in enumerate(NOTES):
            typed = next(q for q in d.paragraphs(doc) if q.getString().startswith(SUPERSCRIPT[n] + ' '))
            d.remove_paragraph(doc, typed)
            p = next(p for p in d.paragraphs(doc) if p.getString().startswith(start))
            at = p.getString().index(word + SUPERSCRIPT[n]) + len(word)
            cursor = text.createTextCursorByRange(p.getStart())
            cursor.goRight(at, False)
            cursor.goRight(1, True)
            cursor.setString('')
            footnote = doc.createInstance('com.sun.star.text.Footnote')
            text.insertTextContent(cursor, footnote, False)
            footnote.setString(note)
        # removing a paragraph joins the next one into it: each day gets its
        # style and its page break again
        for p in d.paragraphs(doc):
            if p.getString() in DAYS:
                p.ParaStyleName = 'Heading 1'
                p.BreakType = enum('com.sun.star.style.BreakType', 'PAGE_BEFORE')
        for st in page_styles(doc):
            # the header: the name, and »Stand:« with the date at the right
            st.HeaderIsOn = True
            header = st.HeaderText
            header.setString('')
            cursor = header.createTextCursor()
            # German, so that the date reads as one (12.11.2026)
            from com.sun.star.lang import Locale
            cursor.CharLocale = Locale('de', 'DE', '')
            # the header style has a centred and a right tab: two tabs
            # put »Stand:« at the right
            header.insertString(cursor, HEAD + '\t\tStand: ', False)
            date = doc.createInstance('com.sun.star.text.textfield.DateTime')
            date.IsDate = True
            date.IsFixed = fixed
            # as a German date (12.11.2026), not as the image's language
            from com.sun.star.lang import Locale
            german = Locale('de', 'DE', '')
            formats = doc.NumberFormats
            key = formats.queryKey('TT.MM.JJJJ', german, False)
            date.NumberFormat = key if key != -1 else formats.addNew('TT.MM.JJJJ', german)
            header.insertTextContent(cursor, date, False)
            # the footer: the page number as a field
            footer = st.FooterText
            footer.setString('')
            cursor = footer.createTextCursor()
            footer.insertString(cursor, 'Seite ', False)
            number = doc.createInstance('com.sun.star.text.textfield.PageNumber')
            number.NumberingType = 4
            number.SubType = enum('com.sun.star.text.PageNumberType', 'CURRENT')
            footer.insertTextContent(cursor, number, False)
        doc.getDocumentIndexes().getByIndex(0).update()
        doc.storeToURL(uno.systemPathToFileUrl(os.path.abspath(path)), (d.prop('FilterName', 'MS Word 2007 XML'),))
        doc.close(True)
    finally:
        process.kill()
        subprocess.run(['pkill', '-f', 'pipe,name=pictures_in_text'], capture_output=True)
        shutil.rmtree(profile, ignore_errors=True)


if __name__ == '__main__':
    os.makedirs(STATIC, exist_ok=True)
    os.makedirs(os.path.join(HERE, 'stufen'), exist_ok=True)
    os.makedirs(os.path.join(HERE, 'bilder'), exist_ok=True)
    for folder in (STATIC, os.path.join(HERE, 'stufen')):
        for name in os.listdir(folder):
            if name.startswith('.~lock.'):
                os.remove(os.path.join(folder, name))
    starter(os.path.join(STATIC, STARTER))
    solution(os.path.join(HERE, 'stufen', 'reisebericht.docx'))
    solution(os.path.join(HERE, 'stufen', 'datum-fest.docx'), fixed=True)
    starter_file = os.path.join(STATIC, STARTER)
    # the top of Tuesday's page with the typed name, its bottom with the
    # typed note, and the same in the solution
    draw_page(starter_file, os.path.join(HERE, 'bilder', 'dienstag.svg'), page=3, top_cm=1.0, height_cm=10.5, left_cm=1.8, width_cm=17.4)
    draw_page(os.path.join(HERE, 'stufen', 'reisebericht.docx'), os.path.join(HERE, 'bilder', 'dienstag-fertig.svg'),
              page=3, top_cm=0.4, height_cm=29.3, left_cm=1.8, width_cm=17.4)
    draw_page(starter_file, os.path.join(HERE, 'teaser.webp'), page=3, top_cm=1.0, height_cm=9.8, left_cm=1.8, width_cm=17.4)
    print('written: static/headers-and-footnotes/08 Reisebericht.docx, stufen/*.docx, bilder/*.svg, teaser.webp')
