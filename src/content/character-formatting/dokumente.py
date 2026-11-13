# The documents of »Schrift mit Absicht«: the 7b's flyer for its cake sale
# as someone very keen made it (the starting file students download) and
# the same flyer set with intent (the solution the check must accept,
# task-examples.yaml). Made with python-docx; run in the office image:
#
#     src/scripts/office python3 src/content/character-formatting/dokumente.py
#
# The flyer is built right (alignment, spacing; that was worksheet 1), but
# set in seven fonts, sizes from 9 to 36 pt, whole paragraphs bold, one
# underlined in capitals.

import os
import sys
import shutil
import subprocess
import tempfile
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.shared import Pt, Cm, RGBColor
from docx.oxml import OxmlElement
from docx.oxml.ns import qn

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
from page_picture import page_picture as draw_page  # noqa: E402
STATIC = os.path.join(HERE, '..', '..', 'static', 'character-formatting')
STARTER = '02 Aushang.docx'

TITLE = 'Kuchenverkauf!'
SUBTITLE = 'Die 7b sammelt für ihre Klassenfahrt an den Kranichsee'
WHEN = 'Am Donnerstag, dem 14. März, verkaufen wir in der großen Pause Kuchen, Muffins und Waffeln.'
WHERE = 'Wo: vor der Mensa'
PRICE = 'Ein Stück kostet 1 Euro. Alles, was wir einnehmen, kommt in die Kasse für die Klassenfahrt.'
BAKE = 'Wer einen Kuchen backen möchte, meldet sich bis Montag bei der Klassenleitung.'
THANKS = 'Danke!'
# what someone passing by must read: when and where
BOLD = ['14. März', 'großen Pause', 'vor der Mensa']


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
    normal.paragraph_format.space_after = Pt(12)
    normal.paragraph_format.line_spacing = 1.15
    doc.core_properties.author = 'Klasse 7b'
    doc.core_properties.title = TITLE
    return doc


def paragraph(doc, align=None, after=None):
    p = doc.add_paragraph()
    if align:
        p.alignment = align
    if after is not None:
        p.paragraph_format.space_after = Pt(after)
    return p


def run(p, text, font, size, bold=False, italic=False, underline=False, caps=False):
    r = p.add_run(text)
    r.font.name = font
    r.font.size = Pt(size)
    if bold:
        r.bold = True
    if italic:
        r.italic = True
    if underline:
        r.underline = True
    if caps:
        r.font.all_caps = True
    return r


def runs_with_bold(p, text, font, size):
    """text in runs, the pieces of BOLD in it bold"""
    at = 0
    while at < len(text):
        hits = [(text.find(b, at), b) for b in BOLD if text.find(b, at) >= 0]
        if not hits:
            run(p, text[at:], font, size)
            break
        start, piece = min(hits)
        if start > at:
            run(p, text[at:start], font, size)
        run(p, piece, font, size, bold=True)
        at = start + len(piece)


def starter(path):
    doc = base()
    run(paragraph(doc, WD_ALIGN_PARAGRAPH.CENTER, 6), TITLE, 'Impact', 36)
    run(paragraph(doc, WD_ALIGN_PARAGRAPH.CENTER, 24), SUBTITLE, 'Comic Sans MS', 16, italic=True, underline=True)
    run(paragraph(doc), WHEN, 'Times New Roman', 12, bold=True)
    run(paragraph(doc), WHERE, 'Courier New', 9)
    run(paragraph(doc), PRICE, 'Verdana', 10, bold=True)
    run(paragraph(doc, after=24), BAKE, 'Arial', 11, bold=True, underline=True, caps=True)
    run(paragraph(doc, WD_ALIGN_PARAGRAPH.CENTER), THANKS, 'Georgia', 20, italic=True)
    doc.save(path)


def solution(path):
    doc = base()
    run(paragraph(doc, WD_ALIGN_PARAGRAPH.CENTER, 6), TITLE, 'Verdana', 32, bold=True)
    run(paragraph(doc, WD_ALIGN_PARAGRAPH.CENTER, 24), SUBTITLE, 'Verdana', 14)
    for text, after in [(WHEN, 12), (WHERE, 12), (PRICE, 12), (BAKE, 24)]:
        runs_with_bold(paragraph(doc, after=after), text, 'Georgia', 12)
    run(paragraph(doc, WD_ALIGN_PARAGRAPH.CENTER), THANKS, 'Verdana', 16, bold=True)
    doc.save(path)


# The top of a document's first page as a picture for the page (as in
# formatting-marks/dokumente.py).
def picture(docx, target, top_cm=1.6, height_cm=10.4, left_cm=0, width_cm=21):
    work = tempfile.mkdtemp()
    try:
        draw_page(docx, target, page=1, top_cm=top_cm, height_cm=height_cm, left_cm=left_cm, width_cm=width_cm)
    finally:
        shutil.rmtree(work, ignore_errors=True)


# ---- The extras (chapters for the quick, extra: true)

# Another round: the poster for a games evening, as keen as the cake sale's.
GAMES_TITLE = 'Spieleabend!'
GAMES_SUBTITLE = 'Die 7b lädt die sechsten Klassen ein'
GAMES_WHEN = 'Am Freitag nach den Ferien spielen wir ab 17 Uhr Brettspiele, Kartenspiele und Werwolf.'
GAMES_WHERE = 'Wo: in der Aula'
GAMES_BRING = 'Bringt euer Lieblingsspiel mit. Für Getränke und Popcorn sorgen wir.'
GAMES_SIGN_UP = 'Wer mitspielen will, trägt sich bis Mittwoch in die Liste an der Tür von Raum 104 ein.'
GAMES_BYE = 'Bis Freitag!'
GAMES_BOLD = ['Freitag nach den Ferien', 'ab 17 Uhr', 'in der Aula']


def games(path):
    doc = base()
    run(paragraph(doc, WD_ALIGN_PARAGRAPH.CENTER, 6), GAMES_TITLE, 'Impact', 40)
    run(paragraph(doc, WD_ALIGN_PARAGRAPH.CENTER, 24), GAMES_SUBTITLE, 'Comic Sans MS', 15, bold=True, underline=True)
    run(paragraph(doc), GAMES_WHEN, 'Georgia', 13, bold=True)
    run(paragraph(doc), GAMES_WHERE, 'Arial', 16, bold=True, caps=True)
    run(paragraph(doc), GAMES_BRING, 'Courier New', 10, italic=True)
    run(paragraph(doc, after=24), GAMES_SIGN_UP, 'Trebuchet MS', 9, underline=True)
    run(paragraph(doc, WD_ALIGN_PARAGRAPH.CENTER), GAMES_BYE, 'Comic Sans MS', 22, bold=True)
    doc.save(path)


def games_solution(path):
    global BOLD
    doc = base()
    run(paragraph(doc, WD_ALIGN_PARAGRAPH.CENTER, 6), GAMES_TITLE, 'Verdana', 32, bold=True)
    run(paragraph(doc, WD_ALIGN_PARAGRAPH.CENTER, 24), GAMES_SUBTITLE, 'Verdana', 14)
    kept, BOLD = BOLD, GAMES_BOLD
    try:
        for text, after in [(GAMES_WHEN, 12), (GAMES_WHERE, 12), (GAMES_BRING, 12), (GAMES_SIGN_UP, 24)]:
            runs_with_bold(paragraph(doc, after=after), text, 'Georgia', 12)
    finally:
        BOLD = kept
    run(paragraph(doc, WD_ALIGN_PARAGRAPH.CENTER), GAMES_BYE, 'Verdana', 16, bold=True)
    doc.save(path)


# Two handles: the 7b's record of measurements at the lake. Monday's heading
# is set as the others should be (Format übertragen); the units and
# formulas need raised and lowered digits (km², CO₂).
RECORD_TITLE = 'Messprotokoll Kranichsee'
RECORD = [
    ('Montag', 'Der Kranichsee ist 2,4 km2 groß und an der tiefsten Stelle 8 m tief. Das Wasser hat 18 °C.'),
    ('Dienstag', 'Im Wasser gelöst sind O2 und CO2. Am Morgen messen wir 9 mg O2 in einem Liter Wasser.'),
    ('Mittwoch', 'Auf einer Fläche von 1 m2 am Ufer zählen wir 31 Wasserschnecken.'),
    ('Donnerstag', 'Der Förster erzählt: Ein Hektar Wald bindet in einem Jahr etwa 10 t CO2.'),
    ('Freitag', 'Zum Schluss rechnen wir: 2,4 km2 sind 240 Hektar.'),
]
RECORD_BLUE = RGBColor(0x2E, 0x5C, 0x8A)
RECORD_RAISED = ['km2', 'm2']
RECORD_LOWERED = ['CO2', 'O2']


def scripted(p, text, font='Calibri', size=12):
    """text in runs, the digit of each unit (km2, m2) raised, of each
    formula (O2, CO2) lowered"""
    import re
    pattern = re.compile(r'(k?m)(2)|(C?O)(2)')
    at = 0
    for m in pattern.finditer(text):
        run(p, text[at:m.start()] + (m.group(1) or m.group(3)), font, size)
        r = run(p, '2', font, size)
        if m.group(1):
            r.font.superscript = True
        else:
            r.font.subscript = True
        at = m.end()
    run(p, text[at:], font, size)


def record(path, solved=False):
    doc = base()
    run(paragraph(doc, after=12), RECORD_TITLE, 'Calibri', 18, bold=True)
    for i, (day, text) in enumerate(RECORD):
        heading = paragraph(doc, after=3)
        if i == 0 or solved:
            r = run(heading, day, 'Georgia', 14, bold=True)
            r.font.color.rgb = RECORD_BLUE
        else:
            run(heading, day, 'Calibri', 12)
        body = paragraph(doc, after=12)
        if solved:
            scripted(body, text)
        else:
            run(body, text, 'Calibri', 12)
    doc.save(path)


# Seven traps: the 7b's thank-you letter to the bakers after the cake sale.
# The six kinds of »Was das Auge nicht sieht« and one of this sheet: a
# sentence in Arial between the Calibri ones.
THANKS_TITLE = 'Danke fürs Backen!'
THANKS_DATE = '24. März'
THANKS_HELLO = 'Liebe Eltern,'
THANKS_RESULT = ('unser Kuchenverkauf war ein voller Erfolg: In der großen Pause waren nach zwanzig Minuten alle 64 Stücke '
                 'verkauft. Zusammen mit den Spenden haben wir 214 Euro eingenommen.')
THANKS_MONEY = ('Das Geld kommt in die Kasse für die Klassenfahrt. Damit wird die Fahrt für jede und jeden von uns um gut '
                'acht Euro günstiger.')
THANKS_BAKERS = ('Ein besonderer Dank geht an alle, die gebacken haben: Der Zitronenkuchen war als Erstes weg, die Waffeln '
                 'gleich danach.')
# the word that ends the first line of THANKS_BAKERS (»python3 dokumente.py lines«)
THANKS_BREAK = 'Erstes'
THANKS_MAY = 'Im Mai erzählen wir Ihnen dann, wie es am Kranichsee war.'
THANKS_GREETING = 'Viele Grüße'
THANKS_SIGN = 'die Klasse 7b'


def thanks(path, bakers_break=True):
    doc = base()
    run(paragraph(doc, after=12), ' ' * 46 + THANKS_TITLE, 'Calibri', 16, bold=True)
    run(paragraph(doc, after=12), ' ' * 151 + THANKS_DATE, 'Calibri', 12)
    run(paragraph(doc, after=12), THANKS_HELLO, 'Calibri', 12)
    run(paragraph(doc, after=12), THANKS_RESULT.replace('verkauft. ', 'verkauft.  '), 'Calibri', 12)
    run(paragraph(doc, after=0), '     ' + THANKS_MONEY, 'Calibri', 12)
    # an empty paragraph instead of a gap, its mark small enough to look
    # like the 12 pt of the others
    empty = paragraph(doc, after=0)
    marks = OxmlElement('w:rPr')
    size = OxmlElement('w:sz')
    size.set(qn('w:val'), '16')
    marks.append(size)
    empty._p.get_or_add_pPr().append(marks)
    p = paragraph(doc, after=12)
    if bakers_break:
        head, tail = THANKS_BAKERS.split(THANKS_BREAK + ' ', 1)
        run(p, head + THANKS_BREAK, 'Calibri', 12).add_break()
        run(p, tail, 'Calibri', 12)
    else:
        run(p, THANKS_BAKERS, 'Calibri', 12)
    run(paragraph(doc, after=24), THANKS_MAY, 'Arial', 11)
    run(paragraph(doc, after=0), THANKS_GREETING, 'Calibri', 12)
    run(paragraph(doc, after=0), THANKS_SIGN, 'Calibri', 12)
    doc.save(path)


def thanks_solution(path):
    doc = base()
    run(paragraph(doc, WD_ALIGN_PARAGRAPH.CENTER, 12), THANKS_TITLE, 'Calibri', 16, bold=True)
    run(paragraph(doc, WD_ALIGN_PARAGRAPH.RIGHT, 12), THANKS_DATE, 'Calibri', 12)
    for text, after in [(THANKS_HELLO, 12), (THANKS_RESULT, 12), (THANKS_MONEY, 12), (THANKS_BAKERS, 12), (THANKS_MAY, 24),
                        (THANKS_GREETING, 0), (THANKS_SIGN, 0)]:
        run(paragraph(doc, after=after), text, 'Calibri', 12)
    doc.save(path)


def caps_typed(path):
    """the solution with »vor der Mensa« typed in capitals to stand out,
    not formatted (an example: the check says the word is written
    differently, not that it is missing)"""
    solution(path)
    doc = Document(path)
    for p in doc.paragraphs:
        for r in p.runs:
            if 'vor der Mensa' in r.text:
                r.text = r.text.replace('vor der Mensa', 'VOR DER MENSA')
    doc.save(path)


def extras():
    caps_typed(os.path.join(HERE, 'stufen', 'aushang-grossgetippt.docx'))
    games(os.path.join(STATIC, '02 Spieleabend.docx'))
    games_solution(os.path.join(HERE, 'stufen', 'spieleabend.docx'))
    record(os.path.join(STATIC, '02 Messprotokoll.docx'))
    record(os.path.join(HERE, 'stufen', 'messprotokoll.docx'), solved=True)
    thanks(os.path.join(STATIC, '02 Dankesbrief.docx'))
    thanks_solution(os.path.join(HERE, 'stufen', 'dankesbrief.docx'))
    print('written: static/character-formatting/02 Spieleabend.docx, 02 Messprotokoll.docx, 02 Dankesbrief.docx and their solutions in stufen/')


# Where LibreOffice ends the lines of the letter without the break by hand
# (to choose THANKS_BREAK).
def lines():
    work = tempfile.mkdtemp()
    try:
        path = os.path.join(work, 'brief.docx')
        thanks(path, bakers_break=False)
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
    solution(os.path.join(HERE, 'stufen', 'aushang.docx'))
    picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'bilder', 'aushang.svg'), top_cm=1.8, height_cm=11.6)
    picture(os.path.join(HERE, 'stufen', 'aushang.docx'), os.path.join(HERE, 'bilder', 'aushang-fertig.svg'), top_cm=1.8, height_cm=11.6)
    # the card in the catalog: 16:9
    picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'teaser.webp'), top_cm=1.6, height_cm=9.9, left_cm=1.7, width_cm=17.6)
    print('written: static/character-formatting/02 Aushang.docx, stufen/aushang.docx, bilder/*.webp, teaser.webp')
