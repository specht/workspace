# The documents of »Formatvorlagen«: the 7b's diary of the class trip with
# every heading formatted by hand (the starting file) and built with heading
# styles (the solution, task-examples.yaml). Run in the office image:
#
#     src/scripts/office python3 src/content/paragraph-styles/dokumente.py
#     src/scripts/office python3 src/content/paragraph-styles/dokumente.py extras   # the documents of »Üben und knobeln«
#
# The starting file: every paragraph has the style Standard. The days are
# bold, 14 pt and blue, set by hand on each, Wednesday in 13 pt by mistake;
# the parts of Wednesday and Thursday bold and 12 pt. Nothing tells the
# program that these lines are headings: the navigation shows none, and a
# table of contents (worksheet 7) would stay empty.
#
# The solution: the title in the style Titel, the days in Überschrift 1, the
# parts in Überschrift 2, nothing set by hand on them; Überschrift 1 itself
# changed once (green, the 7b's colour).

import os
import sys
import shutil
import subprocess
import tempfile
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.shared import Pt, Cm, RGBColor

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
from page_picture import page_picture as draw_page  # noqa: E402
STATIC = os.path.join(HERE, '..', '..', 'static', 'paragraph-styles')
STARTER = '06 Reisetagebuch.docx'

TITLE = 'Reisetagebuch der 7b'
INTRO = 'Fünf Tage am Kranichsee, aufgeschrieben von allen Tischgruppen.'
BLUE = RGBColor(0x1F, 0x4E, 0x79)
GREEN = RGBColor(0x2E, 0x6B, 0x3A)

# (day, [(part or None, text)])
DAYS = [
    ('Montag: Anreise', [(None,
        'Um zehn Uhr fuhr der Bus am Schultor ab. Nach einer Pause an einer Raststätte kamen wir gegen zwei Uhr '
        'an der Jugendherberge an. Die Zimmer haben vier Betten, und von unserem sieht man den See.')]),
    ('Dienstag: Kletterwald', [(None,
        'Im Kletterwald gab es Strecken in drei Höhen. Fast alle haben die mittlere geschafft, sechs sogar die '
        'höchste. Die Seilrutsche über den Bach wollten alle zweimal fahren.')]),
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
    doc.core_properties.author = 'Klasse 7b'
    doc.core_properties.title = TITLE
    return doc


def by_hand(doc, text, size, color=None, before=12, after=4, center=False):
    p = doc.add_paragraph()
    p.paragraph_format.space_before = Pt(before)
    p.paragraph_format.space_after = Pt(after)
    if center:
        p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    r = p.add_run(text)
    r.bold = True
    r.font.size = Pt(size)
    if color is not None:
        r.font.color.rgb = color
    return p


made_by_hand = by_hand   # (solution has a parameter of the same name)


# two table groups pasted their text from elsewhere: it kept its font
PASTED = {'Dienstag: Kletterwald': ('Arial', 11), 'Donnerstag: Schifffahrt': ('Times New Roman', 12.5)}


def subtitle_by_hand(doc):
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    r = p.add_run(INTRO)
    r.italic = True
    r.font.size = Pt(13)
    return p


def text(doc, day, words, pasted=True):
    p = doc.add_paragraph()
    r = p.add_run(words)
    if pasted and day in PASTED:
        r.font.name, size = PASTED[day]
        r.font.size = Pt(size)
    return p


def starter(path):
    doc = base()
    by_hand(doc, TITLE, 20, before=0, after=6, center=True)
    subtitle_by_hand(doc)
    for day, parts in DAYS:
        size = 13 if day.startswith('Mittwoch') else 14
        by_hand(doc, day, size, BLUE, before=18, after=6)
        for part, words in parts:
            if part:
                by_hand(doc, part, 12, before=8, after=2)
            text(doc, day, words)
    doc.save(path)


def heading1(doc):
    """Überschrift 1 changed once: green, the 7b's colour"""
    h1 = doc.styles['Heading 1']
    h1.font.color.rgb = GREEN
    h1.font.size = Pt(16)
    return h1


def matching(doc):
    """the title and Überschrift 2 in the same green, for the solution's
    look (the check asks only for Überschrift 1 changed)"""
    doc.styles['Heading 2'].font.color.rgb = GREEN
    doc.styles['Title'].font.color.rgb = GREEN


def standard(doc):
    """»Standard« changed once: a font with serifs for all the text"""
    doc.styles['Normal'].font.name = 'Georgia'
    doc.styles['Normal'].font.size = Pt(11)


def solution(path, by_hand=False, part_style='Heading 2', changed=True, titles=True, pasted=False, body=True):
    """the stages for the examples: by_hand: the styles given, but the
    days still bold, blue and 14 pt by hand; part_style: Heading 3 skips a
    level; changed: False leaves Überschrift 1 as it was; titles: False
    leaves title and subtitle made by hand; pasted: the pasted text keeps
    its font; body: False leaves »Standard« as it was"""
    doc = base()
    if changed:
        heading1(doc)
        matching(doc)
    if body:
        standard(doc)
    if titles:
        t = doc.add_paragraph(TITLE, style='Title')
        t.alignment = WD_ALIGN_PARAGRAPH.CENTER
        sub = doc.add_paragraph(INTRO, style='Subtitle')
        sub.alignment = WD_ALIGN_PARAGRAPH.CENTER
    else:
        made_by_hand(doc, TITLE, 20, before=0, after=6, center=True)
        subtitle_by_hand(doc)
    for day, parts in DAYS:
        p = doc.add_paragraph(style='Heading 1')
        r = p.add_run(day)
        if by_hand:
            r.bold = True
            r.font.size = Pt(14)
            r.font.color.rgb = BLUE
        for part, words in parts:
            if part:
                doc.add_paragraph(part, style=part_style)
            text(doc, day, words, pasted)
    doc.save(path)


def picture(docx, target, top_cm=1.6, height_cm=12, left_cm=1.8, width_cm=17.4):
    work = tempfile.mkdtemp()
    try:
        draw_page(docx, target, page=1, top_cm=top_cm, height_cm=height_cm, left_cm=left_cm, width_cm=width_cm)
    finally:
        shutil.rmtree(work, ignore_errors=True)


# ---- The extras (chapters for the quick, extra: true)

from docx.enum.style import WD_STYLE_TYPE  # noqa: E402

# Another round: a small guide to the animals at the lake, every heading
# made by hand like the diary's, one text pasted in Arial.
ANIMALS_TITLE = 'Tiere am Kranichsee'
ANIMALS_SUBTITLE = 'Was wir auf der Klassenfahrt gesehen haben'
ANIMALS = [
    ('Vögel', [
        ('Der Kranich', 'Der größte Vogel am See. Kraniche schlafen im flachen Wasser, weil dort kein Fuchs unbemerkt an sie herankommt.'),
        ('Der Graureiher', 'Er steht stundenlang still am Ufer und wartet auf Fische. Fliegt er, zieht er den Hals ein, der Kranich streckt ihn.'),
    ]),
    ('Im Wasser', [
        ('Der Hecht', 'Ein Raubfisch mit langem Maul, der zwischen den Pflanzen lauert. Der Kapitän hat einen gesehen, der fast einen Meter lang war.'),
    ]),
    ('Im Moor', [
        ('Der Moorfrosch', 'Im Frühling werden die Männchen für ein paar Tage blau. Im Juni waren sie wieder braun, aber laut waren sie trotzdem.'),
        ('Die Libelle', 'Über dem Moorpfad flogen so viele, dass wir aufgehört haben zu zählen. Libellen können sogar rückwärts fliegen.'),
    ]),
]
# the text a group pasted from elsewhere: it kept its font
ANIMALS_PASTED = 'Der Hecht'


def animals(path, solved=False):
    doc = base()
    doc.core_properties.title = ANIMALS_TITLE
    if solved:
        heading1(doc)
        matching(doc)
        standard(doc)
        t = doc.add_paragraph(ANIMALS_TITLE, style='Title')
        t.alignment = WD_ALIGN_PARAGRAPH.CENTER
        sub = doc.add_paragraph(ANIMALS_SUBTITLE, style='Subtitle')
        sub.alignment = WD_ALIGN_PARAGRAPH.CENTER
    else:
        made_by_hand(doc, ANIMALS_TITLE, 20, before=0, after=6, center=True)
        p = doc.add_paragraph()
        p.alignment = WD_ALIGN_PARAGRAPH.CENTER
        r = p.add_run(ANIMALS_SUBTITLE)
        r.italic = True
        r.font.size = Pt(13)
    for i, (group, animals_) in enumerate(ANIMALS):
        if solved:
            doc.add_paragraph(group, style='Heading 1')
        else:
            made_by_hand(doc, group, 13 if i == 1 else 14, BLUE, before=18, after=6)
        for name, words in animals_:
            if solved:
                doc.add_paragraph(name, style='Heading 2')
            else:
                made_by_hand(doc, name, 12, before=8, after=2)
            p = doc.add_paragraph()
            r = p.add_run(words)
            if not solved and name == ANIMALS_PASTED:
                r.font.name = 'Arial'
                r.font.size = Pt(11)
    doc.save(path)


# Two handles: the guide to building a bird house, its headings styles
# already. The steps stand in the wrong order (painting first): moved in the
# navigation pane, each with its text. The three tips are made italic and
# green by hand: one style of their own, »Tipp«.
GUIDE_TITLE = 'Bauanleitung: ein Vogelhaus'
GUIDE_SUBTITLE = 'Für Blaumeisen, aus einem einzigen Brett'
GUIDE_NEED = ('Ein Brett aus Fichtenholz, 2 cm dick und 1,20 m lang, zwölf Schrauben, Holzleim, eine Säge, einen '
              'Akkuschrauber und einen Bohrer für ein Loch von 28 mm.')
GUIDE_TIP_WOOD = 'Tipp: Unbehandeltes Holz ist das beste; Vögel mögen keinen Lack im Nest.'
GUIDE_STEPS = {
    'Sägen': ('Säg das Brett in sechs Teile: Boden, Rückwand, Vorderwand, zwei Seiten und das Dach. Bohr in die '
              'Vorderwand das Einflugloch.', 'Tipp: Miss zweimal, bevor du sägst.'),
    'Zusammenbauen': ('Leim und schraub zuerst die Seiten an den Boden, dann Rückwand und Vorderwand. Das Dach kommt '
                      'zuletzt und ragt vorn etwas über.', None),
    'Streichen': ('Streich das Haus nur außen, mit Farbe auf Wasserbasis. Innen bleibt das Holz, wie es ist.',
                  'Tipp: Lass die Farbe einen Tag trocknen, bevor du das Haus aufhängst.'),
}
GUIDE_RIGHT = ['Sägen', 'Zusammenbauen', 'Streichen']
GUIDE_STARTER = ['Streichen', 'Sägen', 'Zusammenbauen']
GUIDE_WHERE = ('In zwei bis drei Metern Höhe, mit dem Loch nach Osten oder Südosten, weg vom Wind und von der '
               'Mittagssonne.')
TIP_GREEN = RGBColor(0x2E, 0x6B, 0x3A)


def guide(path, ordered=False, tip_style=None):
    """ordered: the steps in their order; tip_style: None (the tips by
    hand), 'Tipp' (a style of their own), 'Quote' (one the program brings)"""
    doc = base()
    doc.core_properties.title = GUIDE_TITLE
    heading1(doc)
    matching(doc)
    if tip_style == 'Tipp':
        tip = doc.styles.add_style('Tipp', WD_STYLE_TYPE.PARAGRAPH)
        tip.base_style = doc.styles['Normal']
        tip.font.italic = True
        tip.font.color.rgb = TIP_GREEN
        tip.paragraph_format.left_indent = Cm(0.5)
    doc.add_paragraph(GUIDE_TITLE, style='Title')
    doc.add_paragraph(GUIDE_SUBTITLE, style='Subtitle')

    def tip(words):
        if tip_style:
            doc.add_paragraph(words, style=tip_style)
            return
        p = doc.add_paragraph()
        p.paragraph_format.left_indent = Cm(0.5)
        r = p.add_run(words)
        r.italic = True
        r.font.color.rgb = TIP_GREEN

    doc.add_paragraph('Was du brauchst', style='Heading 1')
    doc.add_paragraph(GUIDE_NEED)
    tip(GUIDE_TIP_WOOD)
    doc.add_paragraph('So baust du es', style='Heading 1')
    for step in (GUIDE_RIGHT if ordered else GUIDE_STARTER):
        words, hint = GUIDE_STEPS[step]
        doc.add_paragraph(step, style='Heading 2')
        doc.add_paragraph(words)
        if hint:
            tip(hint)
    doc.add_paragraph('Wo es hängen soll', style='Heading 1')
    doc.add_paragraph(GUIDE_WHERE)
    doc.save(path)


def guide_libreoffice(docx_path, odt_path):
    """the steps in order (as python-docx writes them), then the style
    »Tipp« made in LibreOffice through UNO, as a student there makes it, and
    the tips' formatting by hand removed; saved as .docx and as .odt"""
    import time
    import uno
    from com.sun.star.beans import PropertyValue

    def prop(name, value):
        p = PropertyValue()
        p.Name = name
        p.Value = value
        return p

    work = tempfile.mkdtemp()
    pipe = 'paragraph_styles'
    process = subprocess.Popen(['soffice', '--headless', '--invisible', '--norestore', '--nologo', '--nolockcheck',
                                f'-env:UserInstallation=file://{work}/profile', f'--accept=pipe,name={pipe};urp;'],
                               stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    try:
        local = uno.getComponentContext()
        resolver = local.ServiceManager.createInstanceWithContext('com.sun.star.bridge.UnoUrlResolver', local)
        desktop = None
        for _ in range(150):
            try:
                context = resolver.resolve(f'uno:pipe,name={pipe};urp;StarOffice.ComponentContext')
                desktop = context.ServiceManager.createInstanceWithContext('com.sun.star.frame.Desktop', context)
                break
            except Exception:
                time.sleep(0.2)
        source = os.path.join(work, 'ordered.docx')
        guide(source, ordered=True)
        doc = desktop.loadComponentFromURL(uno.systemPathToFileUrl(source), '_blank', 0, (prop('Hidden', True),))
        family = doc.StyleFamilies.getByName('ParagraphStyles')
        tip = doc.createInstance('com.sun.star.style.ParagraphStyle')
        family.insertByName('Tipp', tip)
        tip.ParentStyle = 'Standard'
        tip.CharPosture = uno.Enum('com.sun.star.awt.FontSlant', 'ITALIC')
        tip.CharColor = 0x2E6B3A
        tip.ParaLeftMargin = 500
        e = doc.Text.createEnumeration()
        while e.hasMoreElements():
            p = e.nextElement()
            if p.supportsService('com.sun.star.text.Paragraph') and p.getString().startswith('Tipp:'):
                p.ParaStyleName = 'Tipp'
                cursor = doc.Text.createTextCursorByRange(p)
                cursor.setPropertiesToDefault(('CharPosture', 'CharColor'))
                p.setPropertyToDefault('ParaLeftMargin')
        doc.storeToURL(uno.systemPathToFileUrl(os.path.abspath(docx_path)), (prop('FilterName', 'MS Word 2007 XML'),))
        doc.storeToURL(uno.systemPathToFileUrl(os.path.abspath(odt_path)), (prop('FilterName', 'writer8'),))
        doc.close(True)
    finally:
        process.kill()
        subprocess.run(['pkill', '-f', f'pipe,name={pipe}'], capture_output=True)
        shutil.rmtree(work, ignore_errors=True)


# Eleven traps: an information sheet about next year's trip to the sea, with
# headings. The six of the first sheet, then one of each sheet: Arial,
# a typed point, a row outside the table, a stretched photo, and of this
# sheet: one heading only made to look like the others (bold, 16 pt, the
# style's colour), Standard underneath, missing from the navigation.
SEA_TITLE = 'Klassenfahrt an die Ostsee'
SEA_DATE = '10. September'
SEA_HELLO = 'Liebe Eltern,'
SEA_WHAT = ('im Mai fährt die 7b für fünf Tage an die Ostsee, in die Jugendherberge Dünenhagen. Die wichtigsten '
            'Angaben stehen auf diesem Blatt.')
SEA_MORE = 'Am Elternabend erzählen wir mehr.'
SEA_WHEN = ('Wir fahren am Montag, dem 10. Mai, um 8 Uhr mit dem Zug ab und kommen am Freitag gegen 16 Uhr zurück. '
            'Treffpunkt ist der Bahnhof.')
SEA_COSTS = [('Bahnfahrt', '45,00 €'), ('Jugendherberge', '160,00 €'), ('Ausflüge', '25,00 €'), ('Zusammen', '230,00 €')]
SEA_BRING = ['Schlafsack oder Bettwäsche', 'feste Schuhe', 'Regenjacke', 'Badesachen']
SEA_MONEY = ('Das Geld überweisen Sie bitte bis zum 1. März an den Förderverein. Wer dabei Hilfe braucht, spricht '
             'uns einfach an.')
# the word that ends the first line of SEA_MONEY (»python3 dokumente.py lines«)
SEA_BREAK = 'braucht,'
SEA_QUESTIONS = 'Fragen beantworten wir gern per E-Mail.'
SEA_GREETING = 'Viele Grüße'
SEA_SIGN = 'die Klasse 7b'
SEA_HEADINGS = ['Wann und wohin', 'Was es kostet', 'Was mitkommt']
SEA_BY_HAND = 'Was es kostet'
SEA_COLUMN = Cm(4.0)
CRANE = os.path.join(HERE, '..', 'pictures-in-text', 'bilder', 'kranich-uckermark.jpg')
CRANE_ALT = 'Ein grauer Kranich mit schwarz-weißem Hals und rotem Fleck auf dem Kopf steht in einer hohen Wiese.'


def crane_bytes():
    import io
    from PIL import Image
    image = Image.open(CRANE).convert('RGB').resize((1500, 1000), Image.LANCZOS)
    out = io.BytesIO()
    image.save(out, 'JPEG', quality=86)
    out.seek(0)
    return out


def sea_paragraph(doc, text, after=12, align=None, font=None, size=None, bold=False, style=None):
    p = doc.add_paragraph(style=style) if style else doc.add_paragraph()
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


def sea_table(doc, rows):
    """no borders, no margins in the cells (the column at SEA_COLUMN)"""
    from docx.oxml import OxmlElement
    from docx.oxml.ns import qn
    table = doc.add_table(rows=len(rows), cols=2)
    for i, row in enumerate(rows):
        for j, words in enumerate(row):
            cell = table.rows[i].cells[j]
            cell.paragraphs[0].paragraph_format.space_after = Pt(0)
            cell.paragraphs[0].add_run(words)
    widths = [SEA_COLUMN, Cm(12)]
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


def sea(path, money_break=True, solved=False, heading_left=False):
    """solved: all eleven fixed; heading_left: all fixed but the heading by
    hand (an example that finds ten of eleven)"""
    from docx.oxml import OxmlElement
    from docx.oxml.ns import qn
    from docx.enum.text import WD_TAB_ALIGNMENT
    fixed = solved or heading_left
    doc = base()
    doc.core_properties.title = SEA_TITLE
    doc.styles['Normal'].paragraph_format.space_after = Pt(0)
    # the headings in the text's font: the letter has one font
    h1 = doc.styles['Heading 1']
    h1.font.name = 'Calibri'
    h1.font.size = Pt(14)
    h1.font.color.rgb = BLUE
    h1.paragraph_format.space_before = Pt(12)
    h1.paragraph_format.space_after = Pt(4)
    for el in h1.element.rPr.findall(qn('w:rFonts')):
        for k in ('w:asciiTheme', 'w:hAnsiTheme', 'w:eastAsiaTheme', 'w:cstheme'):
            el.attrib.pop(qn(k), None)
    if fixed:
        sea_paragraph(doc, SEA_TITLE, align=WD_ALIGN_PARAGRAPH.CENTER, size=16, bold=True)
        sea_paragraph(doc, SEA_DATE, align=WD_ALIGN_PARAGRAPH.RIGHT)
    else:
        sea_paragraph(doc, ' ' * 42 + SEA_TITLE, size=16, bold=True)
        sea_paragraph(doc, ' ' * 134 + SEA_DATE)
    sea_paragraph(doc, SEA_HELLO)
    sea_paragraph(doc, SEA_WHAT if fixed else SEA_WHAT.replace('Dünenhagen. ', 'Dünenhagen.  '))
    if fixed:
        sea_paragraph(doc, SEA_MORE, after=6)
    else:
        sea_paragraph(doc, '     ' + SEA_MORE, after=0)
        empty = doc.add_paragraph()
        marks = OxmlElement('w:rPr')
        sz = OxmlElement('w:sz')
        sz.set(qn('w:val'), '12')
        marks.append(sz)
        empty._p.get_or_add_pPr().append(marks)

    def heading(words):
        if words == SEA_BY_HAND and not solved:
            # made by hand to look like Überschrift 1
            p, r = sea_paragraph(doc, words, after=4, size=14, bold=True)
            p.paragraph_format.space_before = Pt(12)
            r.font.color.rgb = BLUE
            p.paragraph_format.keep_with_next = True
        else:
            doc.add_paragraph(words, style='Heading 1')

    heading(SEA_HEADINGS[0])
    sea_paragraph(doc, SEA_WHEN, after=6)
    heading(SEA_HEADINGS[1])
    sea_table(doc, SEA_COSTS if fixed else SEA_COSTS[:-1])
    if not fixed:
        p = doc.add_paragraph()
        p.paragraph_format.tab_stops.add_tab_stop(SEA_COLUMN, WD_TAB_ALIGNMENT.LEFT)
        p.add_run('\t'.join(SEA_COSTS[-1]))
    heading(SEA_HEADINGS[2])
    for i, item in enumerate(SEA_BRING):
        last = i == len(SEA_BRING) - 1
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
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.paragraph_format.space_after = Pt(8)
    shape = p.add_run().add_picture(crane_bytes(), width=Cm(6), height=Cm(4) if fixed else Cm(3.57))
    shape._inline.docPr.set('name', 'Grafik 1')
    shape._inline.docPr.set('descr', CRANE_ALT)
    p, r = sea_paragraph(doc, '')
    if money_break and not fixed:
        head, tail = SEA_MONEY.split(SEA_BREAK + ' ', 1)
        r.text = head + SEA_BREAK
        r.add_break()
        p.add_run(tail)
    else:
        r.text = SEA_MONEY
    if fixed:
        sea_paragraph(doc, SEA_QUESTIONS, after=24)
    else:
        sea_paragraph(doc, SEA_QUESTIONS, after=24, font='Arial', size=11)
    sea_paragraph(doc, SEA_GREETING, after=0)
    sea_paragraph(doc, SEA_SIGN, after=0)
    doc.save(path)


def extras():
    os.makedirs(os.path.join(HERE, 'stufen'), exist_ok=True)
    animals(os.path.join(STATIC, '06 Tiere am See.docx'))
    animals(os.path.join(HERE, 'stufen', 'tiere-am-see.docx'), solved=True)
    guide(os.path.join(STATIC, '06 Bauanleitung.docx'))
    guide(os.path.join(HERE, 'stufen', 'bauanleitung.docx'), ordered=True, tip_style='Tipp')
    # examples halfway: the tips in a style the program brings; the steps
    # still in the wrong order
    guide(os.path.join(HERE, 'stufen', 'bauanleitung-zitat.docx'), ordered=True, tip_style='Quote')
    guide(os.path.join(HERE, 'stufen', 'bauanleitung-reihenfolge.docx'), tip_style='Tipp')
    guide_libreoffice(os.path.join(HERE, 'stufen', 'bauanleitung-libreoffice.docx'), os.path.join(HERE, 'stufen', 'bauanleitung-libreoffice.odt'))
    sea(os.path.join(STATIC, '06 Ostsee.docx'))
    sea(os.path.join(HERE, 'stufen', 'ostsee.docx'), solved=True)
    sea(os.path.join(HERE, 'stufen', 'ostsee-zehn.docx'), heading_left=True)
    print('written: static/paragraph-styles/06 Tiere am See.docx, 06 Bauanleitung.docx, 06 Ostsee.docx and the stages in stufen/')


# Where LibreOffice ends the lines of the sheet without the break by hand
# (to choose SEA_BREAK).
def lines():
    work = tempfile.mkdtemp()
    try:
        path = os.path.join(work, 'brief.docx')
        sea(path, money_break=False)
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
    starter(os.path.join(STATIC, STARTER))
    solution(os.path.join(HERE, 'stufen', 'reisetagebuch.docx'))
    solution(os.path.join(HERE, 'stufen', 'noch-von-hand.docx'), by_hand=True)
    solution(os.path.join(HERE, 'stufen', 'ebene-3.docx'), part_style='Heading 3')
    solution(os.path.join(HERE, 'stufen', 'vorlage-wie-vorher.docx'), changed=False)
    solution(os.path.join(HERE, 'stufen', 'titel-von-hand.docx'), titles=False)
    solution(os.path.join(HERE, 'stufen', 'text-von-hand.docx'), pasted=True, body=False)
    work = tempfile.mkdtemp()
    try:
        picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'bilder', 'tagebuch.svg'), height_cm=14.5)
        picture(os.path.join(HERE, 'stufen', 'reisetagebuch.docx'), os.path.join(HERE, 'bilder', 'tagebuch-fertig.svg'), height_cm=14.5)
    finally:
        shutil.rmtree(work, ignore_errors=True)
    picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'teaser.webp'), top_cm=1.6, height_cm=9.8)
    print('written: static/paragraph-styles/06 Reisetagebuch.docx, stufen/reisetagebuch.docx, bilder/*.webp, teaser.webp')
