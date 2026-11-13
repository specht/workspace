# The documents of »Bilder im Text«: an article about the cranes for the
# 7b's trip newspaper as someone put it together in a hurry (the starting
# file) and as it should be (the solution, task-examples.yaml). Run in the
# office image after bilder/karte.py (drawings image):
#
#     src/scripts/office python3 src/content/pictures-in-text/dokumente.py
#     src/scripts/office python3 src/content/pictures-in-text/dokumente.py extras   # the documents of »Üben und knobeln« (after bilder/vogelhaus.py)
#
# The starting file: the photo of a crane stands in a line of its own and
# is stretched to the width of the text (the crane gets fat), both captions
# are typed (»Abbildung 1«, »Abbildung 2«), the photo has no alt text, and
# the map the one Word guesses by itself.
#
# The solution is made by LibreOffice through UNO, as a student would make
# it there: the photo in its own proportions in a frame at the right of the
# text, with its caption, both captions with a number range (a field that
# counts), both pictures with an alt text. Saved as .docx, so the check
# reads it the way it reads a student's file.

import io
import os
import sys
import shutil
import subprocess
import tempfile
import time
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.shared import Pt, Cm
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
from page_picture import page_picture as draw_page  # noqa: E402
STATIC = os.path.join(HERE, '..', '..', 'static', 'pictures-in-text')
STARTER = '05 Kraniche.docx'
PHOTO = os.path.join(HERE, 'bilder', 'kranich-uckermark.jpg')
MAP = os.path.join(HERE, 'bilder', 'karte.png')

TITLE = 'Kraniche am Kranichsee'
TEXT = [
    'Der Kranichsee hat seinen Namen von einem Vogel, den wir auf der Klassenfahrt jeden Morgen gehört haben: '
    'dem Kranich. Kraniche sind fast so groß wie ein Kind in der ersten Klasse. Sie sind grau, haben einen '
    'schwarz-weißen Hals und einen roten Fleck auf dem Kopf.',
    'Im Herbst ziehen die Kraniche nach Süden, bis nach Spanien oder Nordafrika. Vorher sammeln sie sich zu '
    'Tausenden an Seen wie unserem. Am Abend fliegen sie laut rufend zu ihren Schlafplätzen im flachen Wasser. '
    'Dort stehen sie die ganze Nacht, denn im Wasser kommt kein Fuchs unbemerkt an sie heran.',
    'Vom Aussichtsturm am Nordufer konnten wir sie gut beobachten. Die Karte zeigt, wo der Turm steht und wo '
    'der Moorpfad entlangführt.',
    'Wer Kraniche sehen will, braucht ein Fernglas und Geduld. Und man muss leise sein: Kraniche fliegen schon '
    'auf, wenn man noch ein paar hundert Meter entfernt ist.',
]
CAPTION_PHOTO = 'Ein Kranich in einer Wiese in der Uckermark. Foto: Charles J. Sharp, CC BY-SA 4.0'
CAPTION_MAP = 'Der Kranichsee mit Jugendherberge, Aussichtsturm und Moorpfad'
# what Word writes by itself when it guesses a description
AUTO_ALT = 'Ein Bild, das Karte, Text enthält. Automatisch generierte Beschreibung'
ALT_PHOTO = 'Ein grauer Kranich mit schwarz-weißem Hals und rotem Fleck auf dem Kopf steht in einer hohen Wiese.'
ALT_MAP = ('Karte des Kranichsees: die Jugendherberge am Südufer, der Aussichtsturm am Nordufer, ein Weg um den '
           'See, der Moorpfad im Osten und der Schlafplatz der Kraniche im flachen Wasser im Westen.')


def photo_bytes():
    """the photo at 1500 × 1000 pixels, without its metadata"""
    image = Image.open(PHOTO).convert('RGB').resize((1500, 1000), Image.LANCZOS)
    out = io.BytesIO()
    image.save(out, 'JPEG', quality=86)
    out.seek(0)
    return out


def base():
    doc = Document()
    section = doc.sections[0]
    section.page_width, section.page_height = Cm(21), Cm(29.7)
    section.left_margin = section.right_margin = Cm(2.5)
    section.top_margin, section.bottom_margin = Cm(2.2), Cm(2)
    normal = doc.styles['Normal']
    normal.font.name = 'Calibri'
    normal.font.size = Pt(12)
    normal.paragraph_format.space_before = Pt(0)
    normal.paragraph_format.space_after = Pt(8)
    normal.paragraph_format.line_spacing = 1.08
    doc.core_properties.author = 'Klasse 7b'
    doc.core_properties.title = TITLE
    return doc


def picture(doc, source, width, height, name, alt):
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.paragraph_format.space_after = Pt(2)
    shape = p.add_run().add_picture(source, width=width, height=height)
    pr = shape._inline.docPr
    pr.set('name', name)
    if alt:
        pr.set('descr', alt)
    return p


def caption(doc, number, text):
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.paragraph_format.space_after = Pt(12)
    r = p.add_run(f'Abbildung {number}: {text}')
    r.italic = True
    r.font.size = Pt(10)
    return p


# the article of the page; the extras' article (»Noch eine Runde«) has the
# same build with other text and pictures
ARTICLE = {'title': TITLE, 'text': TEXT, 'first': TEXT[0][:18], 'captions': (CAPTION_PHOTO, CAPTION_MAP),
           'alts': (ALT_PHOTO, ALT_MAP), 'source': STARTER}


def starter(path, map_first=False, article=None):
    """map_first: the map moved up, before the crane (the prediction)"""
    a = article or ARTICLE
    doc = base()
    doc.core_properties.title = a['title']
    t = doc.add_paragraph()
    t.alignment = WD_ALIGN_PARAGRAPH.CENTER
    t.paragraph_format.space_after = Pt(14)
    r = t.add_run(a['title'])
    r.bold = True
    r.font.size = Pt(18)

    def the_map():
        picture(doc, a.get('second', MAP), Cm(10.5), Cm(7), 'Grafik 2', AUTO_ALT)
        caption(doc, 2, a['captions'][1])

    if map_first:
        the_map()
    doc.add_paragraph(a['text'][0])
    # stretched to the width of the text: 15 × 5.3 cm instead of 3:2
    picture(doc, a['first_picture']() if 'first_picture' in a else photo_bytes(), Cm(15), Cm(5.3), 'Grafik 1', None)
    caption(doc, 1, a['captions'][0])
    doc.add_paragraph(a['text'][1])
    doc.add_paragraph(a['text'][2])
    if not map_first:
        the_map()
    doc.add_paragraph(a['text'][3])
    doc.save(path)


# ---- the solution, made by LibreOffice

PIPE = 'pictures_in_text'


def office():
    import uno
    profile = tempfile.mkdtemp(prefix='lo-')
    process = subprocess.Popen(['soffice', '--headless', '--invisible', '--norestore', '--nologo', '--nolockcheck',
                                f'-env:UserInstallation=file://{profile}', f'--accept=pipe,name={PIPE};urp;'],
                               stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    local = uno.getComponentContext()
    resolver = local.ServiceManager.createInstanceWithContext('com.sun.star.bridge.UnoUrlResolver', local)
    for _ in range(150):
        try:
            context = resolver.resolve(f'uno:pipe,name={PIPE};urp;StarOffice.ComponentContext')
            desktop = context.ServiceManager.createInstanceWithContext('com.sun.star.frame.Desktop', context)
            return process, profile, desktop
        except Exception:
            time.sleep(0.2)
    raise RuntimeError('LibreOffice did not start')


def prop(name, value):
    from com.sun.star.beans import PropertyValue
    p = PropertyValue()
    p.Name = name
    p.Value = value
    return p


def paragraphs(doc):
    e = doc.Text.createEnumeration()
    while e.hasMoreElements():
        p = e.nextElement()
        if p.supportsService('com.sun.star.text.Paragraph'):
            yield p


def remove_paragraph(doc, p):
    cursor = doc.Text.createTextCursorByRange(p.getStart())
    cursor.gotoEndOfParagraph(True)
    cursor.goRight(1, True)
    cursor.setString('')


def number_field(doc):
    """a number range »Abbildung« (Word: SEQ Abbildung)"""
    masters = doc.TextFieldMasters
    key = 'com.sun.star.text.fieldmaster.SetExpression.Abbildung'
    if masters.hasByName(key):
        master = masters.getByName(key)
    else:
        master = doc.createInstance('com.sun.star.text.fieldmaster.SetExpression')
        # the name first: a type set before it is lost, and the number is
        # then saved as text (»1.00«)
        master.Name = 'Abbildung'
        master.SubType = 1   # SEQUENCE
    field = doc.createInstance('com.sun.star.text.textfield.SetExpression')
    field.attachTextFieldMaster(master)
    field.SubType = 1         # SEQUENCE
    field.NumberingType = 4   # ARABIC
    field.Content = 'Abbildung+1'
    return field


def caption_paragraph(doc, text, cursor, words):
    """at cursor: »Abbildung «, the number as a field, »: words«"""
    text.insertString(cursor, 'Abbildung ', False)
    text.insertTextContent(cursor, number_field(doc), False)
    text.insertString(cursor, f': {words}', False)


def solution(path, typed=False, article=None):
    """typed: the captions still typed (a stage for the examples: all but
    the captions done)"""
    a = article or ARTICLE
    import uno
    from com.sun.star.awt import Size
    from com.sun.star.table import BorderLine2
    process, profile, desktop = office()
    try:
        source = os.path.join(STATIC, a['source'])
        doc = desktop.loadComponentFromURL(uno.systemPathToFileUrl(os.path.abspath(source)), '_blank', 0,
                                           (prop('Hidden', True),))
        objects = doc.GraphicObjects
        photo = objects.getByName('Grafik 1')
        the_map = objects.getByName('Grafik 2')
        the_map.Description = a['alts'][1]
        ps = list(paragraphs(doc))
        first = next(p for p in ps if p.getString().startswith(a['first']))
        photo_par = ps[ps.index(first) + 1]
        photo_caption = ps[ps.index(first) + 2]
        map_caption = next(p for p in ps if p.getString().startswith('Abbildung 2'))

        # the photo: 7.5 × 5 cm (3:2) in a frame at the right of the first
        # paragraph, its caption below it in the frame
        width, height = 7500, 5000
        frame = doc.createInstance('com.sun.star.text.TextFrame')
        frame.AnchorType = uno.Enum('com.sun.star.text.TextContentAnchorType', 'AT_PARAGRAPH')
        frame.Width = width
        frame.SizeType = 2      # MIN: the height grows with the caption
        frame.Height = height + 500
        frame.TextWrap = uno.Enum('com.sun.star.text.WrapTextMode', 'PARALLEL')
        frame.HoriOrient = 1    # RIGHT
        frame.HoriOrientRelation = 1   # PRINT_AREA
        frame.VertOrient = 1    # TOP
        frame.LeftMargin = 400
        frame.BottomMargin = 200
        none = BorderLine2()
        for side in ('LeftBorder', 'RightBorder', 'TopBorder', 'BottomBorder'):
            setattr(frame, side, none)
        for side in ('LeftBorderDistance', 'RightBorderDistance', 'TopBorderDistance', 'BottomBorderDistance'):
            setattr(frame, side, 0)
        graphic = doc.createInstance('com.sun.star.text.TextGraphicObject')
        graphic.Graphic = photo.Graphic
        graphic.Size = Size(width, height)
        graphic.Name = 'Grafik 1'
        doc.Text.insertTextContent(first.getStart(), frame, False)
        inner = frame.getText()
        cursor = inner.createTextCursor()
        graphic.AnchorType = uno.Enum('com.sun.star.text.TextContentAnchorType', 'AS_CHARACTER')
        inner.insertTextContent(cursor, graphic, False)
        graphic.Description = a['alts'][0]
        inner.insertControlCharacter(cursor, 0, False)   # PARAGRAPH_BREAK
        if typed:
            inner.insertString(cursor, f'Abbildung 1: {a["captions"][0]}', False)
        else:
            caption_paragraph(doc, inner, cursor, a['captions'][0])
        cap = inner.createEnumeration()
        cap.nextElement()
        c = cap.nextElement()
        c.ParaStyleName = 'Caption'
        c.CharPosture = uno.Enum('com.sun.star.awt.FontSlant', 'ITALIC')
        c.CharHeight = 10
        c.ParaBottomMargin = 0
        remove_paragraph(doc, photo_caption)
        remove_paragraph(doc, photo_par)

        # the map's caption: »2« becomes the number range
        if not typed:
            cursor = doc.Text.createTextCursorByRange(map_caption.getStart())
            cursor.goRight(len('Abbildung '), False)
            cursor.goRight(1, True)
            cursor.setString('')
            doc.Text.insertTextContent(cursor, number_field(doc), False)
        map_caption.ParaStyleName = 'Caption'
        map_caption.ParaAdjust = 3   # CENTER
        map_caption.CharPosture = uno.Enum('com.sun.star.awt.FontSlant', 'ITALIC')
        map_caption.CharHeight = 10
        doc.TextFields.refresh()
        doc.storeToURL(uno.systemPathToFileUrl(os.path.abspath(path)), (prop('FilterName', 'MS Word 2007 XML'),))
        doc.close(True)
    finally:
        process.kill()
        subprocess.run(['pkill', '-f', f'pipe,name={PIPE}'], capture_output=True)
        shutil.rmtree(profile, ignore_errors=True)


# ---- pictures of the documents for the page

def page_picture(docx, target, top_cm=0, height_cm=29.7, left_cm=0, width_cm=21, dpi=150):
    work = tempfile.mkdtemp()
    try:
        draw_page(docx, target, page=1, top_cm=top_cm, height_cm=height_cm, left_cm=left_cm, width_cm=width_cm)
    finally:
        shutil.rmtree(work, ignore_errors=True)


# ---- The extras (chapters for the quick, extra: true)

HOUSE = os.path.join(HERE, 'bilder', 'vogelhaus.png')
GARDEN = os.path.join(HERE, 'bilder', 'schulgarten.png')

# Another round: the 7b's article about its bird houses, built like the
# cranes' (the house stretched in a line of its own, both captions typed,
# no alt text, the garden's the one Word guesses).
HOUSES = {
    'title': 'Vogelhäuser für den Schulgarten',
    'text': [
        'In der Projektwoche hat die 7b fünf Vogelhäuser gebaut. Jedes ist aus unbehandeltem Holz, hat ein Dach, '
        'das über die Wände ragt, und ein rundes Einflugloch von 28 Millimetern: groß genug für eine Blaumeise, '
        'zu klein für eine Elster.',
        'Am Montag haben wir die Pläne gezeichnet und das Holz ausgesucht, am Dienstag gesägt und geschliffen. '
        'Am Mittwoch wurde zusammengebaut, am Donnerstag gestrichen, nur außen, denn die Vögel mögen keine Farbe '
        'im Nest.',
        'Am Freitag haben wir die Häuser im Schulgarten aufgehängt, jedes an einem anderen Baum. Der Plan zeigt, wo '
        'sie hängen.',
        'Im Herbst machen wir die Häuser sauber, damit im Frühling wieder Vögel einziehen können. Wer eine Meise '
        'hineinfliegen sieht, sagt der 7b Bescheid.',
    ],
    'captions': ('Eines unserer Vogelhäuser, gemalt von der 7b', 'Der Schulgarten mit den fünf Vogelhäusern'),
    'alts': ('Ein türkisfarbenes Vogelhaus mit rotem Dach und rundem Einflugloch auf einem Pfahl, auf dem Dach '
             'sitzt eine Blaumeise.',
             'Plan des Schulgartens: unten die Schule, ein Weg zu zwei Hochbeeten, ein Teich und fünf Bäume, an jedem '
             'ein Vogelhaus mit einer Nummer: 1 Apfelbaum, 2 Eiche, 3 Kastanie, 4 Linde, 5 Birke.'),
    'source': '05 Vogelhaeuser.docx',
    'first_picture': lambda: HOUSE,
    'second': GARDEN,
}
HOUSES['first'] = HOUSES['text'][0][:18]


# Cropping: the animal of the month in the school newspaper, a square box.
# The crane's photo is whole (3:2); cut to a square around the crane, its
# head and tail still in. KEEP in fractions of the photo: from the tip of
# the beak to the tail, from the red crown to below the body.
MONTH_TITLE = 'Tier des Monats: der Kranich'
MONTH_TEXT = [
    'Für die Schülerzeitung stellt jede Klasse ein Tier vor. Die 7b hat sich den Kranich ausgesucht, den sie auf '
    'der Klassenfahrt am Kranichsee jeden Morgen gehört hat.',
    'Kraniche werden bis zu 1,30 Meter groß und wiegen trotzdem nur etwa sechs Kilogramm. Im Herbst ziehen sie in '
    'großen Keilen nach Süden; ihre Rufe hört man, lange bevor man sie sieht.',
]
MONTH_CAPTION = 'Foto: Charles J. Sharp, CC BY-SA 4.0'
KEEP = (0.37, 0.18, 0.63, 0.74)
# the solution's crop, in fractions: left, top, right, bottom cut away
CROP = (600 / 1920, 200 / 1280, 520 / 1920, 280 / 1280)


def month(path, crop=None, size_cm=None):
    """crop: (left, top, right, bottom) cut away, written as Word does
    (srcRect); size_cm: the size shown"""
    from docx.oxml import OxmlElement
    from docx.oxml.ns import qn
    doc = base()
    doc.core_properties.title = MONTH_TITLE
    t = doc.add_paragraph()
    t.paragraph_format.space_after = Pt(10)
    r = t.add_run(MONTH_TITLE)
    r.bold = True
    r.font.size = Pt(18)
    doc.add_paragraph(MONTH_TEXT[0])
    w, h = size_cm or (9, 6)
    p = picture(doc, photo_bytes(), Cm(w), Cm(h), 'Grafik 1', ALT_PHOTO)
    if crop:
        blip_fill = p._p.xpath('.//pic:blipFill')[0]
        rect = OxmlElement('a:srcRect')
        for key, v in zip(('l', 't', 'r', 'b'), crop):
            rect.set(key, str(round(v * 100000)))
        blip_fill.insert(1, rect)
    c = doc.add_paragraph()
    c.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = c.add_run(MONTH_CAPTION)
    run.italic = True
    run.font.size = Pt(9)
    doc.add_paragraph(MONTH_TEXT[1])
    doc.save(path)


def month_libreoffice(path):
    """the same crop made in LibreOffice (GraphicCrop), as a student there"""
    import uno
    from com.sun.star.awt import Size
    from com.sun.star.text import GraphicCrop
    process, profile, desktop = office()
    try:
        source = os.path.join(STATIC, '05 Tier des Monats.docx')
        doc = desktop.loadComponentFromURL(uno.systemPathToFileUrl(os.path.abspath(source)), '_blank', 0,
                                           (prop('Hidden', True),))
        photo = doc.GraphicObjects.getByName('Grafik 1')
        actual = photo.ActualSize
        crop = GraphicCrop()
        crop.Left = round(CROP[0] * actual.Width)
        crop.Top = round(CROP[1] * actual.Height)
        crop.Right = round(CROP[2] * actual.Width)
        crop.Bottom = round(CROP[3] * actual.Height)
        photo.GraphicCrop = crop
        photo.Size = Size(6000, 6000)
        doc.storeToURL(uno.systemPathToFileUrl(os.path.abspath(path)), (prop('FilterName', 'MS Word 2007 XML'),))
        doc.close(True)
    finally:
        process.kill()
        subprocess.run(['pkill', '-f', f'pipe,name={PIPE}'], capture_output=True)
        shutil.rmtree(profile, ignore_errors=True)


# Ten traps: the 7b's invitation to the school fest. The six kinds of the
# first sheet, the font of the second, the typed point of the third, the
# row outside the table of the fourth, and one of this sheet: the bird
# house stretched by a little more than a tenth (its round hole an oval).
FEST_TITLE = 'Einladung zum Schulfest'
FEST_DATE = '22. Juni'
FEST_HELLO = 'Liebe Eltern, liebe Großeltern,'
FEST_WHAT = ('am Freitag, dem 3. Juli, feiern wir unser Schulfest. Es beginnt um 15 Uhr auf dem Schulhof und endet '
             'gegen 19 Uhr. Alle Klassen zeigen, was sie in der Projektwoche gebaut haben.')
FEST_OURS = 'Die 7b hat Vogelhäuser für den Schulgarten gebaut.'
FEST_PLAN_HEAD = 'Das Programm:'
FEST_PLAN = [('15 Uhr', 'Begrüßung auf dem Schulhof'), ('15:30 Uhr', 'Führung durch den Schulgarten'),
             ('16:30 Uhr', 'Auftritt der Schulband'), ('18 Uhr', 'Tombola')]
FEST_BRING_HEAD = 'Für das Buffet bitten wir um:'
FEST_BRING = ['Kuchen oder Muffins', 'Salate', 'Brot und Brötchen', 'Getränke']
FEST_MONEY = ('Mit dem Erlös der Tombola kaufen wir Samen und Erde für die Hochbeete im Schulgarten. Wir freuen uns '
              'auf Ihren Besuch.')
# the word that ends the first line of FEST_MONEY (»python3 dokumente.py lines«)
FEST_BREAK = 'Schulgarten.'
FEST_RAIN = 'Bei Regen feiern wir in der Aula.'
FEST_GREETING = 'Viele Grüße'
FEST_SIGN = 'die Klasse 7b'
FEST_COLUMN = Cm(3.0)
FEST_ALT = HOUSES['alts'][0]
# the house: 7.5 cm wide, 5 cm high in its own proportions; 4.45 cm stretches it by 12 %
FEST_STRETCHED = Cm(4.45)


def letter_paragraph(doc, text, after=12, align=None, font=None, size=None, bold=False):
    p = doc.add_paragraph()
    p.paragraph_format.space_after = Pt(after)
    if align is not None:
        p.alignment = align
    r = p.add_run(text)
    if font:
        r.font.name = font
    if size:
        r.font.size = Pt(size)
    r.bold = bold
    return p, r


def letter_table(doc, rows):
    """no borders, no margins in the cells: the second column's text begins
    at FEST_COLUMN in Word and LibreOffice alike"""
    from docx.oxml import OxmlElement
    from docx.oxml.ns import qn
    table = doc.add_table(rows=len(rows), cols=2)
    for i, row in enumerate(rows):
        for j, text in enumerate(row):
            cell = table.rows[i].cells[j]
            cell.paragraphs[0].paragraph_format.space_after = Pt(0)
            cell.paragraphs[0].add_run(text)
    widths = [FEST_COLUMN, Cm(13)]
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
    return table


def letter_bullet(doc, text, last=False):
    p = doc.add_paragraph(style='List Bullet')
    p.paragraph_format.space_after = Pt(12 if last else 2)
    p.add_run(text)


def fest(path, money_break=True, solved=False, stretched_left=False):
    """solved: all ten fixed; stretched_left: all fixed but the picture (an
    example that finds nine of ten)"""
    from docx.oxml import OxmlElement
    from docx.oxml.ns import qn
    from docx.enum.text import WD_TAB_ALIGNMENT
    fixed = solved or stretched_left
    doc = base()
    doc.core_properties.title = FEST_TITLE
    if fixed:
        letter_paragraph(doc, FEST_TITLE, align=WD_ALIGN_PARAGRAPH.CENTER, size=16, bold=True)
        letter_paragraph(doc, FEST_DATE, align=WD_ALIGN_PARAGRAPH.RIGHT)
    else:
        letter_paragraph(doc, ' ' * 44 + FEST_TITLE, size=16, bold=True)
        letter_paragraph(doc, ' ' * 150 + FEST_DATE)
    letter_paragraph(doc, FEST_HELLO)
    letter_paragraph(doc, FEST_WHAT if fixed else FEST_WHAT.replace('19 Uhr. ', '19 Uhr.  '))
    if fixed:
        letter_paragraph(doc, FEST_OURS)
    else:
        letter_paragraph(doc, '     ' + FEST_OURS, after=0)
        empty = doc.add_paragraph()
        empty.paragraph_format.space_after = Pt(0)
        marks = OxmlElement('w:rPr')
        sz = OxmlElement('w:sz')
        sz.set(qn('w:val'), '16')
        marks.append(sz)
        empty._p.get_or_add_pPr().append(marks)
    letter_paragraph(doc, FEST_PLAN_HEAD, after=4)
    letter_table(doc, FEST_PLAN if fixed else FEST_PLAN[:-1])
    if not fixed:
        p = doc.add_paragraph()
        p.paragraph_format.space_after = Pt(12)
        p.paragraph_format.tab_stops.add_tab_stop(FEST_COLUMN, WD_TAB_ALIGNMENT.LEFT)
        p.add_run('\t'.join(FEST_PLAN[-1]))
    head, _ = letter_paragraph(doc, FEST_BRING_HEAD, after=2)
    if fixed:
        head.paragraph_format.space_before = Pt(12)
    for item in FEST_BRING[:-1]:
        letter_bullet(doc, item)
    if fixed:
        letter_bullet(doc, FEST_BRING[-1], last=True)
    else:
        ref = doc.styles['List Bullet'].paragraph_format
        p = doc.add_paragraph()
        p.paragraph_format.space_after = Pt(12)
        p.paragraph_format.left_indent = ref.left_indent or Cm(0.63)
        p.paragraph_format.first_line_indent = ref.first_line_indent or Cm(-0.63)
        p.paragraph_format.tab_stops.add_tab_stop(p.paragraph_format.left_indent)
        p.add_run('•\t' + FEST_BRING[-1])
    p = picture(doc, HOUSE, Cm(7.5), Cm(5) if solved else FEST_STRETCHED, 'Grafik 1', FEST_ALT)
    p.paragraph_format.space_after = Pt(12)
    p, r = letter_paragraph(doc, '')
    if money_break and not fixed:
        head, tail = FEST_MONEY.split(FEST_BREAK + ' ', 1)
        r.text = head + FEST_BREAK
        r.add_break()
        p.add_run(tail)
    else:
        r.text = FEST_MONEY
    if fixed:
        letter_paragraph(doc, FEST_RAIN, after=24)
    else:
        letter_paragraph(doc, FEST_RAIN, after=24, font='Arial', size=11)
    letter_paragraph(doc, FEST_GREETING, after=0)
    letter_paragraph(doc, FEST_SIGN, after=0)
    doc.save(path)


def extras():
    os.makedirs(os.path.join(HERE, 'stufen'), exist_ok=True)
    starter(os.path.join(STATIC, HOUSES['source']), article=HOUSES)
    solution(os.path.join(HERE, 'stufen', 'vogelhaeuser.docx'), article=HOUSES)
    month(os.path.join(STATIC, '05 Tier des Monats.docx'))
    month(os.path.join(HERE, 'stufen', 'tier-des-monats.docx'), crop=CROP, size_cm=(6, 6))
    month_libreoffice(os.path.join(HERE, 'stufen', 'tier-des-monats-libreoffice.docx'))
    # examples that fail: square but the head cut off; square by squeezing, not cropped
    month(os.path.join(HERE, 'stufen', 'tier-des-monats-ohne-kopf.docx'), crop=(600 / 1920, 400 / 1280, 520 / 1920, 80 / 1280), size_cm=(6, 6))
    month(os.path.join(HERE, 'stufen', 'tier-des-monats-gestaucht.docx'), size_cm=(6, 6))
    fest(os.path.join(STATIC, '05 Schulfest.docx'))
    fest(os.path.join(HERE, 'stufen', 'schulfest.docx'), solved=True)
    fest(os.path.join(HERE, 'stufen', 'schulfest-neun.docx'), stretched_left=True)
    print('written: static/pictures-in-text/05 Vogelhaeuser.docx, 05 Tier des Monats.docx, 05 Schulfest.docx and the stages in stufen/')


# Where LibreOffice ends the lines of the invitation without the break by
# hand (to choose FEST_BREAK).
def lines():
    work = tempfile.mkdtemp()
    try:
        path = os.path.join(work, 'brief.docx')
        fest(path, money_break=False)
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
    starter(os.path.join(STATIC, STARTER))
    solution(os.path.join(HERE, 'stufen', 'kraniche.docx'))
    solution(os.path.join(HERE, 'stufen', 'kraniche-getippt.docx'), typed=True)
    work = tempfile.mkdtemp()
    try:
        moved = os.path.join(work, 'karte-oben.docx')
        starter(moved, map_first=True)
        page_picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'bilder', 'artikel.svg'), top_cm=1.6, height_cm=12.4, left_cm=1.8, width_cm=17.4)
        page_picture(moved, os.path.join(HERE, 'bilder', 'karte-oben.svg'), top_cm=1.6, height_cm=18.1, left_cm=1.8, width_cm=17.4)
        page_picture(os.path.join(HERE, 'stufen', 'kraniche.docx'), os.path.join(HERE, 'bilder', 'artikel-fertig.svg'), top_cm=1.6, height_cm=26.6, left_cm=1.8, width_cm=17.4)
        page_picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'teaser.webp'), top_cm=1.6, height_cm=9.8, left_cm=1.8, width_cm=17.4)
    finally:
        shutil.rmtree(work, ignore_errors=True)
    print('written: static/pictures-in-text/05 Kraniche.docx, stufen/*.docx, bilder/*.webp, teaser.webp')
