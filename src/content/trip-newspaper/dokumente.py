# The documents of »Die Zeitung der Klassenfahrt«: the starting file (a
# title and one line, the article is the student's own), an article built
# as the series taught (the solution, task-examples.yaml) and the same
# article put together in a hurry (a stage that must fail); the article and
# the empty starting file exported as PDF (the check of the PDF). Run in the
# office image:
#
#     src/scripts/office python3 src/content/trip-newspaper/dokumente.py
#
# The fields are written as Word writes them (fldSimple): the number of a
# caption (SEQ Abbildung) and the page number in the footer (PAGE).

import io
import os
import sys
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Pt, Cm, RGBColor
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
from page_picture import page_picture as draw_page  # noqa: E402

STATIC = os.path.join(HERE, '..', '..', 'static', 'trip-newspaper')
STARTER = '10 Artikel.docx'
PHOTO = os.path.join(HERE, '..', 'pictures-in-text', 'bilder', 'kranich-uckermark.jpg')
GREEN = RGBColor(0x2E, 0x6B, 0x3A)

TITLE = 'Ein Tag im Kletterwald'
INTRO = ('Am Dienstag unserer Klassenfahrt ging es in den Kletterwald am Kranichsee. Viele von uns waren noch nie '
         'geklettert, und manche hatten vorher ziemlich Angst. Am Ende des Tages wollte niemand mehr nach Hause.')
PARTS = [
    ('Die Einweisung', [
        'Bevor es losging, hat uns eine Trainerin gezeigt, wie die Gurte sitzen und wie man sich sichert. Jeder '
        'hatte zwei Karabiner, und einer davon musste immer am Seil hängen. Das haben wir unten am Boden so lange '
        'geübt, bis es alle konnten.',
        'Diese drei Regeln galten den ganzen Tag:']),
    ('Hoch hinaus', [
        'Es gab Strecken in drei Höhen. Fast alle haben die mittlere geschafft, sechs sogar die höchste. Am '
        'schwierigsten war eine Brücke aus einzelnen Brettern, die bei jedem Schritt wackelte. Wer unsicher war, '
        'durfte jederzeit an einer Leiter absteigen, und niemand hat darüber gelacht.',
        'Das Beste kam zum Schluss: eine lange Seilrutsche über den Bach. Die wollten alle mindestens zweimal '
        'fahren. Vom Turm aus konnten wir sogar einen Kranich im Schilf sehen.']),
]
RULES = ['Immer ein Karabiner am Seil.', 'Nie beide Karabiner zugleich lösen.', 'Auf einer Plattform höchstens drei Personen.']
CAPTION = 'Ein Kranich, wie wir ihn vom Turm aus gesehen haben. Foto: Charles J. Sharp, CC BY-SA 4.0'
ALT = 'Ein grauer Kranich mit schwarz-weißem Hals und rotem Fleck auf dem Kopf steht in einer hohen Wiese.'
OUTRO = ('Am Abend waren wir müde, aber stolz. Beim Abendessen hat jeder erzählt, welche Strecke er geschafft hat, '
         'und die Klassenleitung hat versprochen, dass wir im nächsten Jahr wiederkommen.')


def base(newspaper=False):
    """newspaper: set as »Die Zeitungsseite« taught, justified with
    automatic hyphenation (the finished article)"""
    doc = Document()
    section = doc.sections[0]
    section.page_width, section.page_height = Cm(21), Cm(29.7)
    section.left_margin = section.right_margin = Cm(2.5)
    section.top_margin, section.bottom_margin = Cm(2.5), Cm(2)
    normal = doc.styles['Normal']
    # German, as a German Word or LibreOffice writes it (the hyphenation
    # follows the language of the text)
    lang = OxmlElement('w:lang')
    lang.set(qn('w:val'), 'de-DE')
    normal.element.get_or_add_rPr().append(lang)
    normal.font.name = 'Calibri'
    normal.font.size = Pt(12)
    normal.paragraph_format.space_before = Pt(0)
    normal.paragraph_format.space_after = Pt(8)
    normal.paragraph_format.line_spacing = 1.08
    for name in ('Title', 'Heading 1'):
        doc.styles[name].font.color.rgb = GREEN
        doc.styles[name].paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
    if newspaper:
        normal.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.JUSTIFY
        doc.settings.element.insert(0, OxmlElement('w:autoHyphenation'))
    doc.core_properties.author = 'Klasse 7b'
    return doc


def field(paragraph, instr, shown):
    """a field as Word writes it, with the text it shows now"""
    simple = OxmlElement('w:fldSimple')
    simple.set(qn('w:instr'), f' {instr} ')
    run = OxmlElement('w:r')
    text = OxmlElement('w:t')
    text.text = shown
    run.append(text)
    simple.append(run)
    paragraph._p.append(simple)


def photo():
    image = Image.open(PHOTO).convert('RGB').resize((1500, 1000), Image.LANCZOS)
    out = io.BytesIO()
    image.save(out, 'JPEG', quality=86)
    out.seek(0)
    return out


def starter(path):
    doc = base()
    doc.add_paragraph('Mein Artikel', style='Title')
    doc.add_paragraph('Hier schreibst du deinen Artikel.')
    doc.save(path)


def box_word(paragraph, shade=True):
    """a box around the paragraph as Word writes it (Rahmen und
    Schattierung): a line on each side, a light green behind it; paragraphs
    in a row with the same box are one box"""
    pr = paragraph._p.get_or_add_pPr()
    later = ('w:shd', 'w:tabs', 'w:suppressAutoHyphens', 'w:spacing', 'w:ind', 'w:contextualSpacing', 'w:jc', 'w:outlineLvl', 'w:rPr')
    borders = OxmlElement('w:pBdr')
    for side in ('top', 'left', 'bottom', 'right'):
        line = OxmlElement(f'w:{side}')
        line.set(qn('w:val'), 'single')
        line.set(qn('w:sz'), '8')
        line.set(qn('w:space'), '4')
        line.set(qn('w:color'), '2E6B3A')
        borders.append(line)
    pr.insert_element_before(borders, *later)
    if shade:
        fill = OxmlElement('w:shd')
        fill.set(qn('w:val'), 'clear')
        fill.set(qn('w:color'), 'auto')
        fill.set(qn('w:fill'), 'E6F0E6')
        pr.insert_element_before(fill, *later[1:])


def article(path, hurried=False, more=False, box=False, shade=True):
    """hurried: the same article put together as at the start of the
    series: headings bold by hand, a list with typed dashes, the caption's
    number typed, no alt text, »Seite 1« typed in the footer, a double space;
    more: also an empty paragraph, a heading in capitals by format, a word
    underlined, a line ended by hand (»Ein Artikel in Eile«); box: the rules
    in a box with a line around and (shade) a colour behind (»Ein Infokasten«)"""
    doc = base(newspaper=not hurried)
    doc.add_paragraph(TITLE, style='Title')
    doc.add_paragraph(INTRO.replace('. Viele', '.  Viele') if hurried else INTRO)
    if more:
        doc.add_paragraph()
    for heading, texts in PARTS:
        if hurried:
            p = doc.add_paragraph()
            r = p.add_run(heading)
            r.bold = True
            r.font.size = Pt(14)
            if more and heading == PARTS[0][0]:
                r.font.all_caps = True
        else:
            doc.add_paragraph(heading, style='Heading 1')
        for text in texts:
            doc.add_paragraph(text)
            if text.startswith('Diese drei Regeln'):
                for rule in RULES:
                    if hurried:
                        doc.add_paragraph('- ' + rule)
                    else:
                        item = doc.add_paragraph(rule, style='List Bullet')
                        if box:
                            box_word(item, shade)
    # the picture: 3:2 at 9 cm, centred, its caption below
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.paragraph_format.space_after = Pt(2)
    shape = p.add_run().add_picture(photo(), width=Cm(9), height=Cm(6))
    shape._inline.docPr.set('name', 'Grafik 1')
    if not hurried:
        shape._inline.docPr.set('descr', ALT)
    c = doc.add_paragraph(style='Caption')
    c.alignment = WD_ALIGN_PARAGRAPH.CENTER
    # no wider than the picture above it: 9 cm in the middle of 16 cm
    c.paragraph_format.left_indent = c.paragraph_format.right_indent = Cm(3.5)
    if hurried:
        c.add_run(f'Abbildung 1: {CAPTION}')
    else:
        c.add_run('Abbildung ')
        field(c, 'SEQ Abbildung \\* ARABIC', '1')
        c.add_run(f': {CAPTION}')
    if more:
        # »stolz« underlined, a line ended by hand after »Abendessen«
        head, tail = OUTRO.split('stolz', 1)
        p = doc.add_paragraph(head)
        p.add_run('stolz').underline = True
        first, rest = tail.split(' Abendessen ', 1)
        r = p.add_run(first + ' Abendessen')
        r.add_break()
        p.add_run(rest)
    else:
        doc.add_paragraph(OUTRO)
    footer = doc.sections[0].footer.paragraphs[0]
    footer.alignment = WD_ALIGN_PARAGRAPH.CENTER
    if hurried:
        footer.add_run('Seite 1')
    else:
        footer.add_run('Seite ')
        field(footer, 'PAGE', '1')
    doc.save(path)


def to_pdf(docx, target):
    """the document exported as PDF, as Word or LibreOffice export it
    (the fonts embedded): an example for the check of the PDF"""
    import shutil
    import subprocess
    import tempfile
    work = tempfile.mkdtemp()
    try:
        subprocess.run(['soffice', '--headless', f'-env:UserInstallation=file://{work}/profile',
                        '--convert-to', 'pdf', '--outdir', work, docx], check=True, capture_output=True)
        shutil.copy(os.path.join(work, os.path.splitext(os.path.basename(docx))[0] + '.pdf'), target)
    finally:
        shutil.rmtree(work, ignore_errors=True)


# ---- The extras (chapters for the quick, extra: true)
#
#     src/scripts/office python3 src/content/trip-newspaper/dokumente.py extras
#
# An article in a hurry: the Kletterwald put together as at the start of the
# series, with what the hand-in checks; an info box: the finished article,
# its three rules to go in a box with a line around and a colour behind.
HURRIED_FILE = '10 Artikel in Eile.docx'
BOX_FILE = '10 Kletterwald.docx'


def box_libreoffice(path):
    """the box as LibreOffice makes it: Format › Absatz, Umrandung (all four
    lines) and Fläche (a colour), for the three rules marked together"""
    import shutil
    import subprocess
    import uno
    sys.path.insert(0, os.path.join(HERE, '..', 'pictures-in-text'))
    import dokumente as d
    process, profile, desktop = d.office()
    try:
        doc = desktop.loadComponentFromURL(uno.systemPathToFileUrl(os.path.abspath(os.path.join(STATIC, BOX_FILE))),
                                           '_blank', 0, (d.prop('Hidden', True),))
        line = uno.createUnoStruct('com.sun.star.table.BorderLine2')
        line.Color = 0x2E6B3A
        line.LineWidth = 26
        line.OuterLineWidth = 26
        for p in d.paragraphs(doc):
            if p.getString() in RULES:
                for side in ('Top', 'Bottom', 'Left', 'Right'):
                    setattr(p, side + 'Border', line)
                    setattr(p, side + 'BorderDistance', 140)
                p.ParaBackColor = 0xE6F0E6
                p.ParaBackTransparent = False
        doc.storeToURL(uno.systemPathToFileUrl(os.path.abspath(path)), (d.prop('FilterName', 'MS Word 2007 XML'),))
        doc.close(True)
    finally:
        process.kill()
        subprocess.run(['pkill', '-f', 'pipe,name=pictures_in_text'], capture_output=True)
        shutil.rmtree(profile, ignore_errors=True)


def extras():
    os.makedirs(os.path.join(HERE, 'stufen'), exist_ok=True)
    for folder in (STATIC, os.path.join(HERE, 'stufen')):
        for name in os.listdir(folder):
            if name.startswith('.~lock.'):
                os.remove(os.path.join(folder, name))
    article(os.path.join(STATIC, HURRIED_FILE), hurried=True, more=True)
    article(os.path.join(STATIC, BOX_FILE))
    article(os.path.join(HERE, 'stufen', 'kasten-word.docx'), box=True)
    article(os.path.join(HERE, 'stufen', 'kasten-ohne-farbe.docx'), box=True, shade=False)
    box_libreoffice(os.path.join(HERE, 'stufen', 'kasten-libreoffice.docx'))
    print('written: static/trip-newspaper/10 Artikel in Eile.docx, 10 Kletterwald.docx and the stages in stufen/')


if __name__ == '__main__' and sys.argv[1:] == ['extras']:
    extras()
    sys.exit(0)


if __name__ == '__main__':
    os.makedirs(STATIC, exist_ok=True)
    os.makedirs(os.path.join(HERE, 'stufen'), exist_ok=True)
    os.makedirs(os.path.join(HERE, 'bilder'), exist_ok=True)
    starter(os.path.join(STATIC, STARTER))
    article(os.path.join(HERE, 'stufen', 'artikel.docx'))
    article(os.path.join(HERE, 'stufen', 'artikel-eilig.docx'), hurried=True)
    to_pdf(os.path.join(HERE, 'stufen', 'artikel.docx'), os.path.join(HERE, 'stufen', 'artikel.pdf'))
    to_pdf(os.path.join(STATIC, STARTER), os.path.join(HERE, 'stufen', 'vorlage.pdf'))
    draw_page(os.path.join(HERE, 'stufen', 'artikel.docx'), os.path.join(HERE, 'bilder', 'artikel.svg'),
              top_cm=1.6, height_cm=27.6, left_cm=1.8, width_cm=17.4)
    draw_page(os.path.join(HERE, 'stufen', 'artikel.docx'), os.path.join(HERE, 'teaser.webp'),
              top_cm=1.6, height_cm=9.8, left_cm=1.8, width_cm=17.4)
    print('written: static/trip-newspaper/10 Artikel.docx, stufen/*.docx, bilder/artikel.svg, teaser.webp')
