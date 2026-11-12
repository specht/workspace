# The documents of »Bilder im Text«: an article about the cranes for the
# 7b's trip newspaper as someone put it together in a hurry (the starting
# file) and as it should be (the solution, task-examples.yaml). Run in the
# office image after bilder/karte.py (drawings image):
#
#     src/scripts/office python3 src/content/pictures-in-text/dokumente.py
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
import shutil
import subprocess
import tempfile
import time
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.shared import Pt, Cm
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
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


def starter(path, map_first=False):
    """map_first: the map moved up, before the crane (the prediction)"""
    doc = base()
    t = doc.add_paragraph()
    t.alignment = WD_ALIGN_PARAGRAPH.CENTER
    t.paragraph_format.space_after = Pt(14)
    r = t.add_run(TITLE)
    r.bold = True
    r.font.size = Pt(18)

    def the_map():
        picture(doc, MAP, Cm(10.5), Cm(7), 'Grafik 2', AUTO_ALT)
        caption(doc, 2, CAPTION_MAP)

    if map_first:
        the_map()
    doc.add_paragraph(TEXT[0])
    # stretched to the width of the text: 15 × 5.3 cm instead of 3:2
    picture(doc, photo_bytes(), Cm(15), Cm(5.3), 'Grafik 1', None)
    caption(doc, 1, CAPTION_PHOTO)
    doc.add_paragraph(TEXT[1])
    doc.add_paragraph(TEXT[2])
    if not map_first:
        the_map()
    doc.add_paragraph(TEXT[3])
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


def solution(path, typed=False):
    """typed: the captions still typed (a stage for the examples: all but
    the captions done)"""
    import uno
    from com.sun.star.awt import Size
    from com.sun.star.table import BorderLine2
    process, profile, desktop = office()
    try:
        source = os.path.join(STATIC, STARTER)
        doc = desktop.loadComponentFromURL(uno.systemPathToFileUrl(os.path.abspath(source)), '_blank', 0,
                                           (prop('Hidden', True),))
        objects = doc.GraphicObjects
        photo = objects.getByName('Grafik 1')
        the_map = objects.getByName('Grafik 2')
        the_map.Description = ALT_MAP
        ps = list(paragraphs(doc))
        first = next(p for p in ps if p.getString().startswith('Der Kranichsee hat'))
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
        graphic.Description = ALT_PHOTO
        inner.insertControlCharacter(cursor, 0, False)   # PARAGRAPH_BREAK
        if typed:
            inner.insertString(cursor, f'Abbildung 1: {CAPTION_PHOTO}', False)
        else:
            caption_paragraph(doc, inner, cursor, CAPTION_PHOTO)
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
        subprocess.run(['soffice', '--headless', f'-env:UserInstallation=file://{work}/profile',
                        '--convert-to', 'pdf', '--outdir', work, docx], check=True, capture_output=True)
        pdf = os.path.join(work, os.path.splitext(os.path.basename(docx))[0] + '.pdf')
        px = lambda cm: int(round(cm / 2.54 * dpi))
        subprocess.run(['pdftoppm', '-r', str(dpi), '-png', '-f', '1', '-l', '1', '-singlefile',
                        '-x', str(px(left_cm)), '-y', str(px(top_cm)), '-W', str(px(width_cm)), '-H', str(px(height_cm)),
                        pdf, os.path.join(work, 'page')], check=True)
        subprocess.run(['cwebp', '-quiet', '-q', '88', os.path.join(work, 'page.png'), '-o', target], check=True)
    finally:
        shutil.rmtree(work, ignore_errors=True)


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
        page_picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'bilder', 'artikel.webp'), top_cm=1.6, height_cm=12.4, left_cm=1.8, width_cm=17.4)
        page_picture(moved, os.path.join(HERE, 'bilder', 'karte-oben.webp'), top_cm=1.6, height_cm=18.1, left_cm=1.8, width_cm=17.4)
        page_picture(os.path.join(HERE, 'stufen', 'kraniche.docx'), os.path.join(HERE, 'bilder', 'artikel-fertig.webp'), top_cm=1.6, height_cm=26.6, left_cm=1.8, width_cm=17.4)
        page_picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'teaser.webp'), top_cm=1.6, height_cm=9.8, left_cm=1.8, width_cm=17.4)
    finally:
        shutil.rmtree(work, ignore_errors=True)
    print('written: static/pictures-in-text/05 Kraniche.docx, stufen/*.docx, bilder/*.webp, teaser.webp')
