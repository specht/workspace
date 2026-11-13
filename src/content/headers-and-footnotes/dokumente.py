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


# the report of the page; the extras' report (»Noch eine Runde«) is built
# the same way from another one
SPEC = {'report': REPORT, 'head': HEAD, 'notes': NOTES, 'days': DAYS, 'source': STARTER}


def starter(path, spec=None):
    import uno
    import dokumente as d
    from com.sun.star.text.ControlCharacter import PARAGRAPH_BREAK
    sp = spec or SPEC
    HEAD, NOTES, DAYS = sp['head'], sp['notes'], sp['days']
    process, profile, desktop = d.office()
    try:
        doc = desktop.loadComponentFromURL(uno.systemPathToFileUrl(os.path.abspath(sp['report'])), '_blank', 0,
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


def solution(path, fixed=False, spec=None):
    import uno
    import dokumente as d
    sp = spec or SPEC
    HEAD, NOTES, DAYS = sp['head'], sp['notes'], sp['days']
    process, profile, desktop = d.office()
    try:
        doc = desktop.loadComponentFromURL(uno.systemPathToFileUrl(os.path.abspath(os.path.join(STATIC, sp['source']))),
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


# ---- The extras (chapters for the quick, extra: true)
#
#     src/scripts/office python3 src/content/headers-and-footnotes/dokumente.py extras
#
# Another round: the 7b's report on the school garden as »Seiten und
# Verzeichnisse« has it built (stufen/schulgarten.docx there), with the name
# typed above each chapter, two notes numbered by hand and »Seite 1« typed in
# the footer, as in the travel report.
GARDEN = {
    'report': os.path.join(HERE, '..', 'pages-and-contents', 'stufen', 'schulgarten.docx'),
    'head': 'Unser Schulgarten',
    'notes': [
        ('Im Frühjahr haben wir zwei Hochbeete', 'Hochbeete',
         'Ein Hochbeet ist ein Beet in einem Kasten, etwa so hoch wie ein Tisch: Man gärtnert, ohne sich zu bücken.'),
        ('Der Teich ist älter als wir alle', 'Laichschnüre',
         'Erdkröten legen ihre Eier in langen Schnüren aus Gallert ab, die sie um Wasserpflanzen wickeln.'),
    ],
    'days': ('Die Hochbeete', 'Der Teich', 'Die Bäume', 'Die Vogelhäuser'),
    'source': '08 Schulgarten.docx',
}

# A title page for itself: the project folder of the bird houses, a title
# page and three pages, with a header and »Seite« and the page number as a
# field in the footer, on every page, the title page too. Built: the title
# page without them (Word: »Erste Seite anders«) and »Seite 2 von 4«.
FOLDER_TITLE = 'Vogelhäuser für den Schulgarten'
FOLDER_SUBTITLE = 'Projektmappe der 7b'
FOLDER_WHEN = 'Projektwoche vom 22. bis 26. Juni'
FOLDER_HEAD = 'Projektmappe der 7b'
FOLDER = [
    ('Was wir bauen', [
        'Fünf Vogelhäuser für Blaumeisen, Kohlmeisen und Spatzen. Jedes Haus hat ein Dach, das man zum Saubermachen '
        'aufklappen kann, und ein Einflugloch in der Größe, die zu seinem Vogel passt: 28 Millimeter für Blaumeisen, '
        '32 Millimeter für Kohlmeisen und Spatzen.',
        'Die Häuser hängen wir in zwei bis drei Metern Höhe auf, mit dem Einflugloch nach Osten oder Südosten, weg vom '
        'Regen, der meistens aus dem Westen kommt.']),
    ('Wie wir bauen', [
        'Am Montag zeichnet jede Gruppe ihren Plan, am Dienstag sägen wir die Bretter zu. Am besten ist Holz ohne '
        'Lack: Fichte oder Kiefer, zwei Zentimeter dick, damit es innen weder zu heiß noch zu kalt wird.',
        'Am Mittwoch schrauben wir die Teile zusammen und bohren die Einfluglöcher, am Donnerstag streichen wir die '
        'Häuser von außen mit Farbe auf Wasserbasis. Innen bleibt das Holz, wie es ist.']),
    ('Wer einzieht', [
        'Meisen suchen sich schon im Februar und März einen Platz für ihr Nest. Wer seine Häuser im Herbst aufhängt, '
        'hat bessere Chancen, und im Winter schlafen manche Vögel darin.',
        'Im Herbst, wenn die Jungen längst ausgeflogen sind, nehmen wir die alten Nester heraus. Sonst leben darin '
        'Flöhe und Milben, die im nächsten Frühjahr die neuen Jungen plagen.']),
]


def docx_field(paragraph, instr, shown):
    """a field (PAGE, NUMPAGES) as Word writes a simple one"""
    from docx.oxml import OxmlElement
    from docx.oxml.ns import qn
    field = OxmlElement('w:fldSimple')
    field.set(qn('w:instr'), f' {instr} ')
    run = OxmlElement('w:r')
    t = OxmlElement('w:t')
    t.text = shown
    run.append(t)
    field.append(run)
    paragraph._p.append(field)


def folder(path, title_page=False, total=False, no_parts=False):
    """the project folder: as handed out (header and footer on every page,
    »Seite« and the page number), title_page: without them on the title
    page, total: »Seite 2 von 4«, no_parts: header and footer deleted"""
    from docx import Document
    from docx.enum.text import WD_ALIGN_PARAGRAPH
    from docx.shared import Cm, Pt
    doc = Document()
    doc.core_properties.title = FOLDER_TITLE
    section = doc.sections[0]
    section.page_height, section.page_width = Cm(29.7), Cm(21.0)
    section.top_margin = section.bottom_margin = Cm(2.5)
    section.left_margin = section.right_margin = Cm(2.5)
    normal = doc.styles['Normal']
    normal.font.name = 'Calibri'
    normal.font.size = Pt(11)
    normal.paragraph_format.space_after = Pt(8)
    normal.paragraph_format.line_spacing = 1.15
    # the title page: lower down, so the top of the page is empty
    p = doc.add_paragraph(FOLDER_TITLE, style='Title')
    p.paragraph_format.space_before = Cm(7)
    doc.add_paragraph(FOLDER_SUBTITLE, style='Subtitle')
    doc.add_paragraph(FOLDER_WHEN)
    for head, paragraphs in FOLDER:
        h = doc.add_paragraph(head, style='Heading 1')
        h.paragraph_format.page_break_before = True
        for text in paragraphs:
            doc.add_paragraph(text)
    if not no_parts:
        header = section.header.paragraphs[0]
        header.text = FOLDER_HEAD
        footer = section.footer.paragraphs[0]
        footer.alignment = WD_ALIGN_PARAGRAPH.CENTER
        footer.add_run('Seite ')
        docx_field(footer, 'PAGE', '1')
        if total:
            footer.add_run(' von ')
            docx_field(footer, 'NUMPAGES', '4')
        if title_page:
            section.different_first_page_header_footer = True
            section.first_page_header.is_linked_to_previous = False
            section.first_page_footer.is_linked_to_previous = False
    doc.save(path)


def folder_libreoffice(path):
    """the folder built as in LibreOffice: page 1 in the page style
    »Erste Seite« (no header, no footer), »von« and the page count as a field"""
    import uno
    import dokumente as d
    process, profile, desktop = d.office()
    try:
        doc = desktop.loadComponentFromURL(uno.systemPathToFileUrl(os.path.abspath(os.path.join(STATIC, '08 Projektmappe.docx'))),
                                           '_blank', 0, (d.prop('Hidden', True),))
        styles = page_styles(doc)
        family = doc.StyleFamilies.getByName('PageStyles')
        first = family.getByName('First Page')
        first.HeaderIsOn = False
        first.FooterIsOn = False
        first.FollowStyle = styles[0].Name
        title = next(iter(d.paragraphs(doc)))
        title.PageDescName = 'First Page'
        footer = styles[0].FooterText
        cursor = footer.createTextCursor()
        cursor.gotoEnd(False)
        footer.insertString(cursor, ' von ', False)
        count = doc.createInstance('com.sun.star.text.textfield.PageCount')
        count.NumberingType = 4
        footer.insertTextContent(cursor, count, False)
        doc.storeToURL(uno.systemPathToFileUrl(os.path.abspath(path)), (d.prop('FilterName', 'MS Word 2007 XML'),))
        doc.close(True)
    finally:
        process.kill()
        subprocess.run(['pkill', '-f', 'pipe,name=pictures_in_text'], capture_output=True)
        shutil.rmtree(profile, ignore_errors=True)


# Thirteen traps: the letter for the open day, two pages, built with python-docx
# as the Harz letter of »Seiten und Verzeichnisse« (its helpers): six traps of
# the first sheet, one of each sheet from the second to the seventh, and one
# of this one, a footnote typed by hand.
def sheet7():
    import importlib.util
    spec = importlib.util.spec_from_file_location('pages_and_contents', os.path.join(HERE, '..', 'pages-and-contents', 'dokumente.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


DAY_TITLE = 'Tag der offenen Tür'
DAY_DATE = '20. Januar'
DAY_HELLO = 'Liebe Eltern,'
DAY_WHAT = ('am Samstag, dem 6. Februar, öffnet unsere Schule von 10 bis 14 Uhr ihre Türen. Die 7b zeigt die '
            'Vogelhäuser aus der Projektwoche und erklärt, wie sie gebaut sind.')
DAY_MORE = 'Auf der zweiten Seite können Sie sich für das Kuchenbuffet eintragen.'
DAY_HEADINGS = ['Wo was ist', 'Was wir noch brauchen', 'Wohin das Geld geht']
DAY_BY_HAND = 'Was wir noch brauchen'
DAY_ROOMS = [('Raum 104', 'Vogelhäuser der 7b'), ('Raum 112', 'Versuche aus der Physik'), ('Aula', 'Theater der 6a'),
             ('Mensa', 'Kuchenbuffet der 7b')]
DAY_NEED = ['Kuchenteller', 'Servietten', 'eine Kasse mit Wechselgeld', 'eine große Thermoskanne']
DAY_PICTURE_LINE = 'So sieht eines der Vogelhäuser aus:'
DAY_PICTURE = os.path.join(HERE, '..', 'pictures-in-text', 'bilder', 'vogelhaus.png')
DAY_PICTURE_ALT = 'Ein Vogelhaus aus hellem Holz mit einem schrägen Dach und einem runden Einflugloch vorn.'
# the money: »Förderverein« with its note, the line ending at DAY_BREAK
DAY_MONEY = ('Was das Kuchenbuffet einnimmt, geht an den Förderverein. Er kauft davon Holz und Werkzeug für die '
             'nächste Projektwoche und hilft, wenn eine Familie eine Klassenfahrt nicht bezahlen kann.')
DAY_NOTE_WORD = 'Förderverein'
DAY_NOTE = 'Im Förderverein sind Eltern, Lehrkräfte und Ehemalige, die die Schule unterstützen.'
# the word that ends the first line of DAY_MONEY (»python3 dokumente.py lines«)
DAY_BREAK = 'und'
DAY_QUESTIONS = 'Bei Fragen schreiben Sie uns gern eine E-Mail.'
DAY_SLIP = 'Kuchen für das Buffet'
DAY_SLIP_LINES = ['Name des Kindes: ______________________________',
                  'Wir backen diesen Kuchen: ______________________________',
                  'Wir bringen ihn am Samstag bis 9:30 Uhr in die Mensa.']


def picture_bytes():
    import io
    from PIL import Image
    image = Image.open(DAY_PICTURE).convert('RGB')
    out = io.BytesIO()
    image.save(out, 'JPEG', quality=88)
    out.seek(0)
    return out


def open_day(path, empties=0, money_break=True, solved=False):
    """empties: the empty paragraphs that push the list on page 2; solved:
    all fixed but the footnote, which is still typed (it is made a footnote
    by open_day_footnote: a footnote python-docx cannot write)"""
    from docx.oxml import OxmlElement
    from docx.oxml.ns import qn
    from docx.enum.text import WD_ALIGN_PARAGRAPH, WD_TAB_ALIGNMENT
    from docx.shared import Cm, Pt
    p7 = sheet7()
    para, fixed = p7.harz_paragraph, solved
    doc = p7.base()
    doc.core_properties.title = DAY_TITLE
    doc.styles['Normal'].paragraph_format.space_after = Pt(0)
    doc.styles['Normal'].paragraph_format.line_spacing = 1.0
    h1 = doc.styles['Heading 1']
    h1.font.name = 'Calibri'
    h1.font.size = Pt(14)
    h1.paragraph_format.space_before = Pt(12)
    h1.paragraph_format.space_after = Pt(4)
    for el in h1.element.rPr.findall(qn('w:rFonts')):
        for k in ('w:asciiTheme', 'w:hAnsiTheme', 'w:eastAsiaTheme', 'w:cstheme'):
            el.attrib.pop(qn(k), None)
    if fixed:
        para(doc, DAY_TITLE, align=WD_ALIGN_PARAGRAPH.CENTER, size=16, bold=True)
        para(doc, DAY_DATE, align=WD_ALIGN_PARAGRAPH.RIGHT)
    else:
        para(doc, ' ' * 48 + DAY_TITLE, size=16, bold=True)
        para(doc, ' ' * 140 + DAY_DATE)
    para(doc, DAY_HELLO)
    para(doc, DAY_WHAT if fixed else DAY_WHAT.replace('Türen. ', 'Türen.  '))
    if fixed:
        para(doc, DAY_MORE, after=6)
    else:
        para(doc, '     ' + DAY_MORE, after=0)
        empty = doc.add_paragraph()
        marks = OxmlElement('w:rPr')
        sz = OxmlElement('w:sz')
        sz.set(qn('w:val'), '12')
        marks.append(sz)
        empty._p.get_or_add_pPr().append(marks)

    def heading(words):
        if words == DAY_BY_HAND and not fixed:
            p, r = para(doc, words, after=4, size=14, bold=True)
            p.paragraph_format.space_before = Pt(12)
            r.font.color.rgb = p7.GREEN
            p.paragraph_format.keep_with_next = True
        else:
            doc.add_paragraph(words, style='Heading 1')

    heading(DAY_HEADINGS[0])
    p7.harz_table(doc, DAY_ROOMS if fixed else DAY_ROOMS[:-1])
    if not fixed:
        p = doc.add_paragraph()
        p.paragraph_format.tab_stops.add_tab_stop(p7.HARZ_COLUMN, WD_TAB_ALIGNMENT.LEFT)
        p.add_run('\t'.join(DAY_ROOMS[-1]))
    para(doc, DAY_PICTURE_LINE, after=4).__getitem__(0).paragraph_format.space_before = Pt(8)
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.paragraph_format.space_after = Pt(4)
    shape = p.add_run().add_picture(picture_bytes(), width=Cm(5), height=Cm(3.33) if fixed else Cm(2.97))
    shape._inline.docPr.set('name', 'Grafik 1')
    shape._inline.docPr.set('descr', DAY_PICTURE_ALT)
    heading(DAY_HEADINGS[1])
    for i, item in enumerate(DAY_NEED):
        last = i == len(DAY_NEED) - 1
        if last and not fixed:
            ref = doc.styles['List Bullet'].paragraph_format
            p = doc.add_paragraph()
            p.paragraph_format.space_after = Pt(8)
            p.paragraph_format.left_indent = ref.left_indent or Cm(0.63)
            p.paragraph_format.first_line_indent = ref.first_line_indent or Cm(-0.63)
            p.paragraph_format.tab_stops.add_tab_stop(p.paragraph_format.left_indent)
            p.add_run('•\t' + item)
        else:
            p = doc.add_paragraph(style='List Bullet')
            p.paragraph_format.space_after = Pt(8 if last else 0)
            p.add_run(item)
    heading(DAY_HEADINGS[2])
    # the money, with »Förderverein¹« and the note typed under it
    money = DAY_MONEY.replace(DAY_NOTE_WORD, DAY_NOTE_WORD + SUPERSCRIPT[0], 1)
    p, r = para(doc, '', after=4)
    if money_break and not fixed:
        head, tail = money.split(' ' + DAY_BREAK + ' ', 1)
        r.text = head + ' ' + DAY_BREAK
        r.add_break()
        p.add_run(tail)
    else:
        r.text = money
    para(doc, SUPERSCRIPT[0] + ' ' + DAY_NOTE, after=12, size=10)
    if fixed:
        para(doc, DAY_QUESTIONS, after=12)
    else:
        para(doc, DAY_QUESTIONS, after=12, font='Arial', size=12)
    para(doc, 'Viele Grüße', after=0)
    para(doc, 'die Klasse 7b', after=0)
    if not fixed:
        for _ in range(empties):
            doc.add_paragraph()
    slip = doc.add_paragraph(DAY_SLIP, style='Heading 1')
    if fixed:
        slip.paragraph_format.page_break_before = True
    for line in DAY_SLIP_LINES:
        para(doc, line, after=18)
    doc.save(path)


def open_day_footnote(source, path):
    """the solved letter with the typed note made a footnote, in LibreOffice"""
    import uno
    import dokumente as d
    process, profile, desktop = d.office()
    try:
        doc = desktop.loadComponentFromURL(uno.systemPathToFileUrl(os.path.abspath(source)), '_blank', 0, (d.prop('Hidden', True),))
        text = doc.Text
        typed = next(q for q in d.paragraphs(doc) if q.getString().startswith(SUPERSCRIPT[0] + ' '))
        d.remove_paragraph(doc, typed)
        p = next(p for p in d.paragraphs(doc) if (DAY_NOTE_WORD + SUPERSCRIPT[0]) in p.getString())
        at = p.getString().index(DAY_NOTE_WORD + SUPERSCRIPT[0]) + len(DAY_NOTE_WORD)
        cursor = text.createTextCursorByRange(p.getStart())
        cursor.goRight(at, False)
        cursor.goRight(1, True)
        cursor.setString('')
        footnote = doc.createInstance('com.sun.star.text.Footnote')
        text.insertTextContent(cursor, footnote, False)
        footnote.setString(DAY_NOTE)
        # removing a paragraph joins the next one into it: the greeting
        # keeps its look, the list its page break
        for q in d.paragraphs(doc):
            if q.getString() == DAY_SLIP:
                q.ParaStyleName = 'Heading 1'
                q.BreakType = enum('com.sun.star.style.BreakType', 'PAGE_BEFORE')
        doc.storeToURL(uno.systemPathToFileUrl(os.path.abspath(path)), (d.prop('FilterName', 'MS Word 2007 XML'),))
        doc.close(True)
    finally:
        process.kill()
        subprocess.run(['pkill', '-f', 'pipe,name=pictures_in_text'], capture_output=True)
        shutil.rmtree(profile, ignore_errors=True)


def open_day_empties():
    """as many empty paragraphs as push the list onto page 2"""
    import server
    work = tempfile.mkdtemp()
    try:
        path = os.path.join(work, 'try.docx')
        for n in range(0, 80):
            open_day(path, n)
            with open(path, 'rb') as f:
                model = server.analyze(f.read(), 'try.docx')
            slip = [p for p in model['paragraphs'] if p['text'] == DAY_SLIP][0]
            if slip['box']['page'] >= 2:
                return n
        raise RuntimeError('the list never reached page 2')
    finally:
        server.OFFICE.stop()
        shutil.rmtree(work, ignore_errors=True)


def open_day_lines():
    """where LibreOffice ends the lines of the letter without the break by
    hand (to choose DAY_BREAK)"""
    work = tempfile.mkdtemp()
    try:
        path = os.path.join(work, 'brief.docx')
        open_day(path, 0, money_break=False)
        subprocess.run(['soffice', '--headless', f'-env:UserInstallation=file://{work}/profile', '--convert-to', 'pdf', '--outdir', work, path],
                       check=True, capture_output=True)
        print(subprocess.run(['pdftotext', '-layout', os.path.join(work, 'brief.pdf'), '-'], capture_output=True, text=True, check=True).stdout)
    finally:
        shutil.rmtree(work, ignore_errors=True)


def extras():
    os.makedirs(os.path.join(HERE, 'stufen'), exist_ok=True)
    for folder_ in (STATIC, os.path.join(HERE, 'stufen')):
        for name in os.listdir(folder_):
            if name.startswith('.~lock.'):
                os.remove(os.path.join(folder_, name))
    starter(os.path.join(STATIC, GARDEN['source']), spec=GARDEN)
    solution(os.path.join(HERE, 'stufen', 'schulgarten.docx'), spec=GARDEN)
    folder(os.path.join(STATIC, '08 Projektmappe.docx'))
    folder(os.path.join(HERE, 'stufen', 'projektmappe.docx'), title_page=True, total=True)
    folder(os.path.join(HERE, 'stufen', 'projektmappe-ohne-anzahl.docx'), title_page=True)
    folder(os.path.join(HERE, 'stufen', 'projektmappe-ohne-kopf.docx'), total=True, no_parts=True)
    folder_libreoffice(os.path.join(HERE, 'stufen', 'projektmappe-libreoffice.docx'))
    n = open_day_empties()
    print('empty paragraphs before the list:', n)
    open_day(os.path.join(STATIC, '08 Elternbrief.docx'), n)
    twelve = os.path.join(HERE, 'stufen', 'elternbrief-zwoelf.docx')
    open_day(twelve, solved=True)
    open_day_footnote(twelve, os.path.join(HERE, 'stufen', 'elternbrief.docx'))
    print('written: static/headers-and-footnotes/08 Schulgarten.docx, 08 Projektmappe.docx, 08 Elternbrief.docx and the stages in stufen/')


if __name__ == '__main__' and sys.argv[1:] == ['lines']:
    open_day_lines()
    sys.exit(0)
if __name__ == '__main__' and sys.argv[1:] == ['extras']:
    extras()
    sys.exit(0)


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
