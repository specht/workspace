# The documents of »Seiten und Verzeichnisse«: the 7b's travel report, six
# pages, put together in a hurry (the starting file), and built with page
# breaks, a table of contents and page numbers (the solution and the stages
# of task-examples.yaml). Run in the office image:
#
#     src/scripts/office python3 src/content/pages-and-contents/dokumente.py
#     src/scripts/office python3 src/content/pages-and-contents/dokumente.py extras   # the documents of »Üben und knobeln«
#
# The starting file: a title page with a table of contents typed by hand
# (dots and page numbers), each day pushed onto a page of its own with empty
# paragraphs (as many as it took: the office service lays the document out
# and one more is added until the heading jumps), »Seite 1« typed into the
# footer, the same on every page. The headings already have their styles
# (worksheet 6).
#
# The solution is made by LibreOffice through UNO: no empty paragraphs,
# »Seitenumbruch oberhalb« in the style Überschrift 1, a table of contents
# from the headings, the page number as a field in the footer.

import os
import shutil
import subprocess
import sys
import tempfile
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.shared import Pt, Cm, RGBColor

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
from page_picture import page_picture as draw_page  # noqa: E402
STATIC = os.path.join(HERE, '..', '..', 'static', 'pages-and-contents')
STARTER = '07 Reisebericht.docx'
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
sys.path.insert(0, os.path.join(HERE, '..', 'pictures-in-text'))

GREEN = RGBColor(0x2E, 0x6B, 0x3A)
TITLE = 'Reisebericht der 7b'
SUBTITLE = 'Unsere Klassenfahrt an den Kranichsee'
# (day, [(part or None, text)]), as in »Formatvorlagen«, each day longer
DAYS = [
    ('Montag: Anreise', [(None,
        'Um zehn Uhr fuhr der Bus am Schultor ab. Nach einer Pause an einer Raststätte kamen wir gegen zwei Uhr '
        'an der Jugendherberge an. Die Zimmer haben vier Betten, und von unserem sieht man den See.'),
        (None, 'Nach dem Abendessen haben wir die Gegend erkundet. Hinter der Jugendherberge beginnt der Wald, '
         'und ein schmaler Weg führt hinunter zum Bootssteg.')]),
    ('Dienstag: Kletterwald', [(None,
        'Im Kletterwald gab es Strecken in drei Höhen. Fast alle haben die mittlere geschafft, sechs sogar die '
        'höchste. Die Seilrutsche über den Bach wollten alle zweimal fahren.'),
        (None, 'Vorher gab es eine Einweisung: Immer ein Karabiner am Seil, beim Umhängen nie beide zugleich '
         'lösen. Wer unsicher war, durfte jederzeit an einer Leiter absteigen.')]),
    ('Mittwoch: Wanderung', [
        ('Am Vormittag: um den See',
         'Wir sind einmal um den ganzen Kranichsee gewandert, gut acht Kilometer. Am Aussichtsturm haben wir '
         'Picknick gemacht und mit dem Fernglas Kraniche im flachen Wasser beobachtet.'),
        ('Am Nachmittag: über den Moorpfad',
         'Zurück ging es über den Moorpfad, auf Brettern durch das Moor. Wer neben die Bretter tritt, steht '
         'sofort bis zum Knöchel im Wasser. Das haben drei von uns ausprobiert.'),
    ]),
    ('Donnerstag: Schifffahrt', [
        ('Auf dem Schiff',
         'Mit dem Ausflugsschiff sind wir über den See gefahren. Der Kapitän hat erzählt, dass im Herbst '
         'Tausende Kraniche hier rasten, bevor sie nach Süden ziehen.'),
        ('Der letzte Abend',
         'Am Abend gab es ein Lagerfeuer am Ufer mit Stockbrot. Die Klassenleitung hat eine Gruselgeschichte '
         'vorgelesen, die niemand gruselig fand.'),
    ]),
    ('Freitag: Rückfahrt', [(None,
        'Nach dem Frühstück haben wir die Zimmer aufgeräumt und die Betten abgezogen. Um drei Uhr waren wir '
        'wieder an der Schule, müde und ziemlich laut.')]),
]
MORE = (' Auf der Fahrt haben wir Spiele gespielt und Musik gehört. Zwei von uns ist im Bus schlecht geworden, '
        'aber nach der Pause ging es wieder. Am Ziel hat uns die Herbergsmutter begrüßt und alles gezeigt.')
# a heading pushed onto a new page leaves this much room at most below
PAGE_BOTTOM = 29.7 - 2.0


def base():
    doc = Document()
    section = doc.sections[0]
    section.page_width, section.page_height = Cm(21), Cm(29.7)
    section.left_margin = section.right_margin = Cm(2.5)
    section.top_margin, section.bottom_margin = Cm(2.5), Cm(2)
    normal = doc.styles['Normal']
    normal.font.name = 'Calibri'
    normal.font.size = Pt(12)
    normal.paragraph_format.space_before = Pt(0)
    normal.paragraph_format.space_after = Pt(8)
    normal.paragraph_format.line_spacing = 1.08
    for name, size in (('Heading 1', 16), ('Heading 2', 13)):
        doc.styles[name].font.color.rgb = GREEN
        doc.styles[name].font.size = Pt(size)
    doc.styles['Title'].font.color.rgb = GREEN
    doc.core_properties.author = 'Klasse 7b'
    doc.core_properties.title = TITLE
    return doc


# the report of the page; the extras' report (»Noch eine Runde«) is built
# the same way with other chapters
REPORT = {'title': TITLE, 'subtitle': SUBTITLE, 'chapters': DAYS, 'source': STARTER}


def starter(path, empties, longer=False, report=None):
    """empties: how many empty paragraphs end the title page and each day
    (six numbers); longer: Monday with four sentences more (the prediction)"""
    rep = report or REPORT
    DAYS = rep['chapters']
    doc = base()
    doc.core_properties.title = rep['title']
    t = doc.add_paragraph(rep['title'], style='Title')
    t.alignment = WD_ALIGN_PARAGRAPH.CENTER
    sub = doc.add_paragraph(rep['subtitle'])
    sub.alignment = WD_ALIGN_PARAGRAPH.CENTER
    sub.paragraph_format.space_after = Pt(30)
    head = doc.add_paragraph()
    r = head.add_run('Inhalt')
    r.bold = True
    r.font.size = Pt(14)
    # typed: the name, dots until it looked about right, the page
    for i, (day, _) in enumerate(DAYS):
        doc.add_paragraph(day + ' ' + '.' * (60 - 2 * len(day) // 2 * 1 + (i % 2) * 3) + ' ' + str(i + 2))
    for _ in range(empties[0]):
        doc.add_paragraph()
    for k, (day, parts) in enumerate(DAYS):
        doc.add_paragraph(day, style='Heading 1')
        for j, (part, text) in enumerate(parts):
            if part:
                doc.add_paragraph(part, style='Heading 2')
            doc.add_paragraph(text + (MORE * 2 if longer and k == 0 and j == len(parts) - 1 else ''))
        if k + 1 < len(DAYS):
            for _ in range(empties[k + 1]):
                doc.add_paragraph()
    footer = doc.sections[0].footer.paragraphs[0]
    footer.alignment = WD_ALIGN_PARAGRAPH.CENTER
    footer.add_run('Seite 1')
    doc.save(path)


def count_empties(report=None):
    """as many empty paragraphs as push each next heading onto the next
    page, found with the office service's layout"""
    import server
    DAYS = (report or REPORT)['chapters']
    work = tempfile.mkdtemp()
    empties = [0] * len(DAYS)
    try:
        path = os.path.join(work, 'try.docx')
        for k in range(len(DAYS)):
            for _ in range(80):
                starter(path, empties, report=report)
                with open(path, 'rb') as f:
                    model = server.analyze(f.read(), 'try.docx')
                heading = [p for p in model['paragraphs'] if p['text'] == DAYS[k][0]][0]
                if heading['box']['page'] >= k + 2:
                    break
                # room left below the heading on its page, in empty lines
                room = (PAGE_BOTTOM / 2.54 * 72 - heading['box']['top']) / 22.0
                empties[k] += max(1, int(room * 0.7))
        return empties
    finally:
        server.OFFICE.stop()
        shutil.rmtree(work, ignore_errors=True)


# ---- the solution, made by LibreOffice

def solution(path, stale=False, breaks=True, report=None):
    """stale: a heading renamed after the table of contents was made (it
    shows the old name); breaks: False leaves the days without page breaks
    (a stage: no empty paragraphs, but no new pages either)"""
    import uno
    import dokumente as d
    rep = report or REPORT
    DAYS = rep['chapters']
    process, profile, desktop = d.office()
    try:
        doc = desktop.loadComponentFromURL(uno.systemPathToFileUrl(os.path.abspath(os.path.join(STATIC, rep['source']))),
                                           '_blank', 0, (d.prop('Hidden', True),))
        # the empty paragraphs and the typed table of contents go, one at a
        # time (a removal disposes the paragraph after it)
        gone = lambda text: text == '' or any(text.startswith(day + ' .') for day, _ in DAYS)
        for _ in range(500):
            p = next((p for p in d.paragraphs(doc) if gone(p.getString())), None)
            if p is None:
                break
            d.remove_paragraph(doc, p)
        head = next(p for p in d.paragraphs(doc) if p.getString() == 'Inhalt')
        # the table of contents where »Inhalt« was
        index = doc.createInstance('com.sun.star.text.ContentIndex')
        index.CreateFromOutline = True
        index.Level = 2
        index.Title = 'Inhalt'
        doc.Text.insertTextContent(head.getStart(), index, False)
        d.remove_paragraph(doc, head)
        # removing a paragraph joins the next one into it, with its format:
        # the days and their parts get their styles back
        parts = {part for _, ps in DAYS for part, _ in ps if part}
        for p in d.paragraphs(doc):
            text = p.getString()
            if any(text == day for day, _ in DAYS):
                p.ParaStyleName = 'Heading 1'
                # the break on each day, not in the style: Word bases the
                # heading of the table of contents on Überschrift 1, which
                # would push »Inhalt« onto a page of its own too
                if breaks:
                    p.BreakType = uno.Enum('com.sun.star.style.BreakType', 'PAGE_BEFORE')
            elif text in parts:
                p.ParaStyleName = 'Heading 2'
        # the page number as a field in the footer
        for name in doc.StyleFamilies.getByName('PageStyles').getElementNames():
            st = doc.StyleFamilies.getByName('PageStyles').getByName(name)
            if not (st.isInUse() and st.FooterIsOn):
                continue
            text = st.FooterText
            text.setString('')
            cursor = text.createTextCursor()
            text.insertString(cursor, 'Seite ', False)
            field = doc.createInstance('com.sun.star.text.textfield.PageNumber')
            field.NumberingType = 4
            field.SubType = uno.Enum('com.sun.star.text.PageNumberType', 'CURRENT')
            text.insertTextContent(cursor, field, False)
        index.update()
        if stale:
            for p in d.paragraphs(doc):
                if p.getString() == 'Freitag: Rückfahrt':
                    p.setString('Freitag: Heimfahrt')
        doc.storeToURL(uno.systemPathToFileUrl(os.path.abspath(path)), (d.prop('FilterName', 'MS Word 2007 XML'),))
        doc.close(True)
    finally:
        process.kill()
        subprocess.run(['pkill', '-f', 'pipe,name=pictures_in_text'], capture_output=True)
        shutil.rmtree(profile, ignore_errors=True)


def picture(docx, target, page=1, top_cm=1.6, height_cm=12, left_cm=1.8, width_cm=17.4):
    work = tempfile.mkdtemp()
    try:
        draw_page(docx, target, page=page, top_cm=top_cm, height_cm=height_cm, left_cm=left_cm, width_cm=width_cm)
    finally:
        shutil.rmtree(work, ignore_errors=True)


# ---- The extras (chapters for the quick, extra: true)

# Another round: the 7b's report on the school garden, built like the
# travel report (a typed table of contents, chapters pushed with empty
# paragraphs).
GARDEN = {
    'title': 'Unser Schulgarten',
    'subtitle': 'Ein Bericht der 7b für die Schülerzeitung',
    'source': '07 Schulgarten.docx',
    'chapters': [
        ('Die Hochbeete', [(None,
            'Im Frühjahr haben wir zwei Hochbeete aus alten Brettern gebaut und mit Ästen, Laub und Erde gefüllt. '
            'Unten verrottet das Holz langsam und gibt den Pflanzen Wärme und Nährstoffe.'),
            (None, 'In das eine Beet kamen Radieschen und Salat, in das andere Kräuter: Schnittlauch, Petersilie und '
             'Minze, die sich schneller ausbreitet, als uns lieb ist.')]),
        ('Der Teich', [(None,
            'Der Teich ist älter als wir alle. Im Frühling legen Erdkröten dort ihre Laichschnüre ab, im Sommer '
            'jagen Libellen über dem Wasser.'),
            (None, 'Damit kein Tier ertrinkt, haben wir an einer Seite flache Steine ins Wasser gelegt, über die ein '
             'Igel wieder herausklettern kann.')]),
        ('Die Bäume', [
            ('Der Apfelbaum', 'Er trägt jedes zweite Jahr so viele Äpfel, dass die ganze Schule davon isst. In den '
             'anderen Jahren hängen nur ein paar an den Zweigen.'),
            ('Die Eiche', 'Die Eiche ist der höchste Baum im Garten. In ihr wohnen Eichhörnchen, die im Herbst '
             'Eicheln im ganzen Garten vergraben und viele davon nie wiederfinden.'),
        ]),
        ('Die Vogelhäuser', [(None,
            'In der Projektwoche hat die 7b fünf Vogelhäuser gebaut und an die Bäume gehängt. Im ersten Frühling '
            'ist in drei davon eine Blaumeise eingezogen.')]),
    ],
}


# One page turned: the plan of the project week as a table of six columns,
# 4 cm each, on a page of its own between the title page and the list of
# what the class needs. Upright it sticks out past the right edge of the
# page; turned, it fits.
WEEK_TITLE = 'Projektwoche der 7b'
WEEK_SUBTITLE = 'Wir bauen Vogelhäuser für den Schulgarten'
WEEK_INTRO = ('Vom 22. bis 26. Juni baut die 7b Vogelhäuser. Auf der nächsten Seite steht, wer wann was macht, '
              'auf der Seite danach, was wir dafür brauchen.')
WEEK_PLAN_HEAD = 'Der Plan'
WEEK_PLAN = [('Zeit', 'Montag', 'Dienstag', 'Mittwoch', 'Donnerstag', 'Freitag'),
             ('8:00 bis 9:30', 'Pläne zeichnen', 'Sägen, Gruppe A', 'Zusammenbauen', 'Streichen, erster Anstrich', 'Aufhängen im Garten'),
             ('10:00 bis 11:30', 'Holz aussuchen', 'Sägen, Gruppe B', 'Dächer aufsetzen', 'Streichen, zweiter Anstrich', 'Aufräumen'),
             ('12:00 bis 13:00', 'Werkzeug kennenlernen', 'Schleifen', 'Einflugloch bohren', 'Namen aufmalen', 'Schulfest vorbereiten')]
WEEK_NEED_HEAD = 'Was wir brauchen'
WEEK_NEED = ('Fünf Bretter aus Fichtenholz, Schrauben, Holzleim, Farbe auf Wasserbasis und Pinsel. Sägen und '
             'Akkuschrauber leiht uns der Hausmeister.')
COLUMN = Cm(4)


def week_table(doc):
    from docx.oxml import OxmlElement
    from docx.oxml.ns import qn
    table = doc.add_table(rows=len(WEEK_PLAN), cols=6)
    table.style = 'Table Grid'
    for i, row in enumerate(WEEK_PLAN):
        for j, words in enumerate(row):
            cell = table.rows[i].cells[j]
            cell.paragraphs[0].paragraph_format.space_after = Pt(0)
            r = cell.paragraphs[0].add_run(words)
            r.bold = i == 0
    table.autofit = False
    for col in table._tbl.tblGrid.findall(qn('w:gridCol')):
        col.set(qn('w:w'), str(int(COLUMN.twips)))
    for row in table.rows:
        for cell in row.cells:
            cell.width = COLUMN
    layout = OxmlElement('w:tblLayout')
    layout.set(qn('w:type'), 'fixed')
    table._tbl.tblPr.append(layout)


def week(path, turned=False):
    """turned: the plan's page in a section of its own, landscape (as Word
    makes it)"""
    from docx.enum.section import WD_SECTION, WD_ORIENT
    doc = base()
    doc.core_properties.title = WEEK_TITLE
    t = doc.add_paragraph(WEEK_TITLE, style='Title')
    t.alignment = WD_ALIGN_PARAGRAPH.CENTER
    sub = doc.add_paragraph(WEEK_SUBTITLE)
    sub.alignment = WD_ALIGN_PARAGRAPH.CENTER
    sub.paragraph_format.space_after = Pt(30)
    doc.add_paragraph(WEEK_INTRO)
    if turned:
        section = doc.add_section(WD_SECTION.NEW_PAGE)
        section.orientation = WD_ORIENT.LANDSCAPE
        section.page_width, section.page_height = Cm(29.7), Cm(21)
        h = doc.add_paragraph(WEEK_PLAN_HEAD, style='Heading 1')
    else:
        h = doc.add_paragraph(WEEK_PLAN_HEAD, style='Heading 1')
        h.paragraph_format.page_break_before = True
    week_table(doc)
    if turned:
        section = doc.add_section(WD_SECTION.NEW_PAGE)
        section.orientation = WD_ORIENT.PORTRAIT
        section.page_width, section.page_height = Cm(21), Cm(29.7)
        doc.add_paragraph(WEEK_NEED_HEAD, style='Heading 1')
    else:
        h = doc.add_paragraph(WEEK_NEED_HEAD, style='Heading 1')
        h.paragraph_format.page_break_before = True
    doc.add_paragraph(WEEK_NEED)
    doc.save(path)


def week_libreoffice(path, all_turned=False):
    """the plan's page turned in LibreOffice, as a student there turns it: the
    page style »Landscape« from its heading on, the default style again from
    the next heading; all_turned: the whole document turned (an example
    that fails)"""
    import uno
    import dokumente as d
    process, profile, desktop = d.office()
    try:
        doc = desktop.loadComponentFromURL(uno.systemPathToFileUrl(os.path.abspath(os.path.join(STATIC, '07 Projektwoche.docx'))),
                                           '_blank', 0, (d.prop('Hidden', True),))
        first = doc.Text.createTextCursor().PageStyleName
        pages = doc.StyleFamilies.getByName('PageStyles')
        if all_turned:
            st = pages.getByName(first)
            st.IsLandscape = True
            st.Width, st.Height = 29700, 21000
        else:
            wide = pages.getByName('Landscape')
            for side in ('LeftMargin', 'RightMargin', 'TopMargin'):
                setattr(wide, side, 2500)
            wide.BottomMargin = 2000
            for p in d.paragraphs(doc):
                if p.getString() == WEEK_PLAN_HEAD:
                    p.PageDescName = 'Landscape'
                elif p.getString() == WEEK_NEED_HEAD:
                    p.PageDescName = first
        doc.storeToURL(uno.systemPathToFileUrl(os.path.abspath(path)), (d.prop('FilterName', 'MS Word 2007 XML'),))
        doc.close(True)
    finally:
        process.kill()
        subprocess.run(['pkill', '-f', 'pipe,name=pictures_in_text'], capture_output=True)
        shutil.rmtree(profile, ignore_errors=True)


# Twelve traps: an information sheet about a trip to the Harz with a reply
# slip on its second page. The six of the first sheet, then one of each
# sheet: Arial, a typed point, a row outside the table, a stretched photo,
# a heading by hand, and of this sheet: the slip pushed onto page 2 by empty
# paragraphs, no page break.
HARZ_TITLE = 'Klassenfahrt in den Harz'
HARZ_DATE = '15. Januar'
HARZ_HELLO = 'Liebe Eltern,'
HARZ_WHAT = ('im April fährt die 7b für vier Tage in den Harz, in die Jugendherberge am Brockenweg. Hier stehen '
             'die wichtigsten Angaben.')
HARZ_MORE = 'Die Anmeldung finden Sie auf der zweiten Seite.'
HARZ_WHEN = ('Wir fahren am Dienstag, dem 14. April, um 8 Uhr mit dem Bus ab und kommen am Freitag gegen 17 Uhr '
             'zurück. Treffpunkt ist das Schultor.')
HARZ_COSTS = [('Busfahrt', '60,00 €'), ('Jugendherberge', '128,00 €'), ('Bergwerk', '12,00 €'), ('Zusammen', '200,00 €')]
HARZ_BRING = ['Wanderschuhe', 'eine warme Jacke', 'Regenkleidung', 'eine Taschenlampe']
HARZ_MONEY = ('Das Geld überweisen Sie bitte bis zum 1. März an den Förderverein. Wer dabei Hilfe braucht, spricht '
              'uns einfach an.')
# the word that ends the first line of HARZ_MONEY (»python3 dokumente.py lines«)
HARZ_BREAK = 'braucht,'
HARZ_QUESTIONS = 'Fragen beantworten wir gern per E-Mail.'
HARZ_HEADINGS = ['Wann und wohin', 'Was es kostet', 'Was mitkommt']
HARZ_BY_HAND = 'Was mitkommt'
HARZ_SLIP = 'Anmeldung zur Klassenfahrt'
HARZ_SLIP_LINES = ['Name des Kindes: ______________________________',
                   'Ich melde mein Kind verbindlich zur Klassenfahrt in den Harz an.',
                   'Ort, Datum und Unterschrift: ______________________________']
HARZ_COLUMN = Cm(4.0)
CRANE = os.path.join(HERE, '..', 'pictures-in-text', 'bilder', 'kranich-uckermark.jpg')
CRANE_ALT = 'Ein grauer Kranich mit schwarz-weißem Hals und rotem Fleck auf dem Kopf steht in einer hohen Wiese.'
CRANE_LINE = 'Vielleicht sehen wir unterwegs Kraniche; im April ziehen sie nach Norden.'


def crane_bytes():
    import io
    from PIL import Image
    image = Image.open(CRANE).convert('RGB').resize((1500, 1000), Image.LANCZOS)
    out = io.BytesIO()
    image.save(out, 'JPEG', quality=86)
    out.seek(0)
    return out


def harz_paragraph(doc, text, after=12, align=None, font=None, size=None, bold=False):
    p = doc.add_paragraph()
    p.paragraph_format.space_after = Pt(after)
    if align is not None:
        p.alignment = align
    r = p.add_run(text)
    if font:
        r.font.name = font
    if size:
        r.font.size = Pt(size)
    if bold:
        r.bold = True
    return p, r


def harz_table(doc, rows):
    """no borders, no margins in the cells (the column at HARZ_COLUMN)"""
    from docx.oxml import OxmlElement
    from docx.oxml.ns import qn
    table = doc.add_table(rows=len(rows), cols=2)
    for i, row in enumerate(rows):
        for j, words in enumerate(row):
            cell = table.rows[i].cells[j]
            cell.paragraphs[0].paragraph_format.space_after = Pt(0)
            cell.paragraphs[0].add_run(words)
    widths = [HARZ_COLUMN, Cm(12)]
    table.autofit = False
    for col, width in zip(table._tbl.tblGrid.findall(qn('w:gridCol')), widths):
        col.set(qn('w:w'), str(int(width.twips)))
    for row in table.rows:
        for cell, width in zip(row.cells, widths):
            cell.width = width
    pr = table._tbl.tblPr
    margins = OxmlElement('w:tblCellMar')
    for side in ('left', 'right'):
        m = OxmlElement(f'w:{side}')
        m.set(qn('w:w'), '0')
        m.set(qn('w:type'), 'dxa')
        margins.append(m)
    pr.append(margins)
    layout = OxmlElement('w:tblLayout')
    layout.set(qn('w:type'), 'fixed')
    pr.append(layout)


def harz(path, empties=0, money_break=True, solved=False, slip_left=False):
    """empties: the empty paragraphs that push the slip onto page 2;
    solved: all twelve fixed; slip_left: all fixed but the slip (an example
    that finds eleven of twelve)"""
    from docx.oxml import OxmlElement
    from docx.oxml.ns import qn
    from docx.enum.text import WD_TAB_ALIGNMENT
    fixed = solved or slip_left
    doc = base()
    doc.core_properties.title = HARZ_TITLE
    doc.styles['Normal'].paragraph_format.space_after = Pt(0)
    doc.styles['Normal'].paragraph_format.line_spacing = 1.0
    # the headings in the text's font: the sheet has one font
    h1 = doc.styles['Heading 1']
    h1.font.name = 'Calibri'
    h1.font.size = Pt(14)
    h1.paragraph_format.space_before = Pt(12)
    h1.paragraph_format.space_after = Pt(4)
    for el in h1.element.rPr.findall(qn('w:rFonts')):
        for k in ('w:asciiTheme', 'w:hAnsiTheme', 'w:eastAsiaTheme', 'w:cstheme'):
            el.attrib.pop(qn(k), None)
    if fixed:
        harz_paragraph(doc, HARZ_TITLE, align=WD_ALIGN_PARAGRAPH.CENTER, size=16, bold=True)
        harz_paragraph(doc, HARZ_DATE, align=WD_ALIGN_PARAGRAPH.RIGHT)
    else:
        harz_paragraph(doc, ' ' * 44 + HARZ_TITLE, size=16, bold=True)
        harz_paragraph(doc, ' ' * 140 + HARZ_DATE)
    harz_paragraph(doc, HARZ_HELLO)
    harz_paragraph(doc, HARZ_WHAT if fixed else HARZ_WHAT.replace('Brockenweg. ', 'Brockenweg.  '))
    if fixed:
        harz_paragraph(doc, HARZ_MORE, after=6)
    else:
        harz_paragraph(doc, '     ' + HARZ_MORE, after=0)
        empty = doc.add_paragraph()
        marks = OxmlElement('w:rPr')
        sz = OxmlElement('w:sz')
        sz.set(qn('w:val'), '12')
        marks.append(sz)
        empty._p.get_or_add_pPr().append(marks)

    def heading(words):
        if words == HARZ_BY_HAND and not fixed:
            p, r = harz_paragraph(doc, words, after=4, size=14, bold=True)
            p.paragraph_format.space_before = Pt(12)
            r.font.color.rgb = GREEN
            p.paragraph_format.keep_with_next = True
        else:
            doc.add_paragraph(words, style='Heading 1')

    heading(HARZ_HEADINGS[0])
    harz_paragraph(doc, HARZ_WHEN, after=6)
    heading(HARZ_HEADINGS[1])
    harz_table(doc, HARZ_COSTS if fixed else HARZ_COSTS[:-1])
    if not fixed:
        p = doc.add_paragraph()
        p.paragraph_format.tab_stops.add_tab_stop(HARZ_COLUMN, WD_TAB_ALIGNMENT.LEFT)
        p.add_run('\t'.join(HARZ_COSTS[-1]))
    heading(HARZ_HEADINGS[2])
    for i, item in enumerate(HARZ_BRING):
        last = i == len(HARZ_BRING) - 1
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
    harz_paragraph(doc, CRANE_LINE, after=4)
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.paragraph_format.space_after = Pt(8)
    shape = p.add_run().add_picture(crane_bytes(), width=Cm(5), height=Cm(3.33) if fixed else Cm(2.97))
    shape._inline.docPr.set('name', 'Grafik 1')
    shape._inline.docPr.set('descr', CRANE_ALT)
    p, r = harz_paragraph(doc, '')
    if money_break and not fixed:
        head, tail = HARZ_MONEY.split(HARZ_BREAK + ' ', 1)
        r.text = head + HARZ_BREAK
        r.add_break()
        p.add_run(tail)
    else:
        r.text = HARZ_MONEY
    if fixed:
        harz_paragraph(doc, HARZ_QUESTIONS, after=12)
    else:
        harz_paragraph(doc, HARZ_QUESTIONS, after=12, font='Arial', size=11)
    harz_paragraph(doc, 'Viele Grüße', after=0)
    harz_paragraph(doc, 'die Klasse 7b', after=0)
    # the slip on page 2
    if not solved:
        for _ in range(empties):
            doc.add_paragraph()
    slip = doc.add_paragraph(HARZ_SLIP, style='Heading 1')
    if solved:
        slip.paragraph_format.page_break_before = True
    for line in HARZ_SLIP_LINES:
        harz_paragraph(doc, line, after=18)
    doc.save(path)


def harz_empties():
    """as many empty paragraphs as push the slip onto page 2"""
    import server
    work = tempfile.mkdtemp()
    try:
        path = os.path.join(work, 'try.docx')
        for n in range(0, 80):
            harz(path, n)
            with open(path, 'rb') as f:
                model = server.analyze(f.read(), 'try.docx')
            slip = [p for p in model['paragraphs'] if p['text'] == HARZ_SLIP][0]
            if slip['box']['page'] >= 2:
                return n
        raise RuntimeError('the slip never reached page 2')
    finally:
        server.OFFICE.stop()
        shutil.rmtree(work, ignore_errors=True)


def extras():
    os.makedirs(os.path.join(HERE, 'stufen'), exist_ok=True)
    for folder in (STATIC, os.path.join(HERE, 'stufen')):
        for name in os.listdir(folder):
            if name.startswith('.~lock.'):
                os.remove(os.path.join(folder, name))
    n = harz_empties()
    print('empty paragraphs before the slip:', n)
    harz(os.path.join(STATIC, '07 Harz.docx'), n)
    harz(os.path.join(HERE, 'stufen', 'harz.docx'), solved=True)
    harz(os.path.join(HERE, 'stufen', 'harz-elf.docx'), n, slip_left=True)
    week(os.path.join(STATIC, '07 Projektwoche.docx'))
    week(os.path.join(HERE, 'stufen', 'projektwoche.docx'), turned=True)
    week_libreoffice(os.path.join(HERE, 'stufen', 'projektwoche-libreoffice.docx'))
    week_libreoffice(os.path.join(HERE, 'stufen', 'projektwoche-alles-quer.docx'), all_turned=True)
    empties = garden_empties()
    print('empty paragraphs in the garden report:', empties)
    starter(os.path.join(STATIC, GARDEN['source']), empties, report=GARDEN)
    solution(os.path.join(HERE, 'stufen', 'schulgarten.docx'), report=GARDEN)
    print('written: static/pages-and-contents/07 Schulgarten.docx, 07 Projektwoche.docx, 07 Harz.docx and the stages in stufen/')


def garden_empties():
    """count_empties for the garden report, in a process of its own: the
    office service it starts is stopped at its end"""
    out = subprocess.run([sys.executable, '-c', 'import sys; sys.argv = ["x"]; sys.path.insert(0, "."); '
                          'import dokumente as k; print(" ".join(map(str, k.count_empties(k.GARDEN))))'],
                         cwd=HERE, capture_output=True, text=True, check=True)
    return [int(x) for x in out.stdout.split()[-len(GARDEN['chapters']):]]


# Where LibreOffice ends the lines of the sheet without the break by hand
# (to choose HARZ_BREAK).
def lines():
    work = tempfile.mkdtemp()
    try:
        path = os.path.join(work, 'brief.docx')
        harz(path, 0, money_break=False)
        subprocess.run(['soffice', '--headless', f'-env:UserInstallation=file://{work}/profile', '--convert-to', 'pdf', '--outdir', work, path],
                       check=True, capture_output=True)
        print(subprocess.run(['pdftotext', '-layout', os.path.join(work, 'brief.pdf'), '-'], capture_output=True, text=True, check=True).stdout)
    finally:
        shutil.rmtree(work, ignore_errors=True)


if __name__ == '__main__' and sys.argv[1:] == ['extras']:
    extras()
    sys.exit(0)
if __name__ == '__main__' and sys.argv[1:] == ['lines']:
    lines()
    sys.exit(0)


if __name__ == '__main__':
    os.makedirs(STATIC, exist_ok=True)
    os.makedirs(os.path.join(HERE, 'stufen'), exist_ok=True)
    os.makedirs(os.path.join(HERE, 'bilder'), exist_ok=True)
    # lock files left by a LibreOffice that did not close its document
    for folder in (STATIC, os.path.join(HERE, 'stufen')):
        for name in os.listdir(folder):
            if name.startswith('.~lock.'):
                os.remove(os.path.join(folder, name))
    empties = count_empties()
    print('empty paragraphs:', empties)
    starter(os.path.join(STATIC, STARTER), empties)
    solution(os.path.join(HERE, 'stufen', 'reisebericht.docx'))
    solution(os.path.join(HERE, 'stufen', 'verzeichnis-veraltet.docx'), stale=True)
    solution(os.path.join(HERE, 'stufen', 'ohne-umbrueche.docx'), breaks=False)
    work = tempfile.mkdtemp()
    try:
        longer = os.path.join(work, 'laenger.docx')
        starter(longer, empties, longer=True)
        picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'bilder', 'titelseite.svg'), height_cm=10)
        picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'bilder', 'dienstag.svg'), page=3, top_cm=0, height_cm=9)
        picture(longer, os.path.join(HERE, 'bilder', 'dienstag-weiter-unten.svg'), page=3, top_cm=0, height_cm=9)
        picture(os.path.join(HERE, 'stufen', 'reisebericht.docx'), os.path.join(HERE, 'bilder', 'titelseite-fertig.svg'), height_cm=10)
        picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'teaser.webp'), top_cm=1.6, height_cm=9.8)
    finally:
        shutil.rmtree(work, ignore_errors=True)
    with open(os.path.join(HERE, 'stufen', 'leere-absaetze.txt'), 'w') as f:
        f.write(' '.join(map(str, empties)) + '\n')
    print('written: static/pages-and-contents/07 Reisebericht.docx, stufen/*.docx, bilder/*.webp, teaser.webp')
