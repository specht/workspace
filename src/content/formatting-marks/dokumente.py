# The documents of »Was das Auge nicht sieht«: the invitation as someone
# typed it in a hurry (the starting file students download) and the same
# invitation built as it should be (the solution the check must accept,
# task-examples.yaml). Made with python-docx, so both have Word's default
# template; run in the office image:
#
#     src/scripts/office python3 src/content/formatting-marks/dokumente.py
#
# The starting file looks right when printed at 12 pt: the title and the
# second line centred with spaces, the date pushed right with spaces, the
# gaps made of empty paragraphs, the letter's lines ended by hand (Shift+
# Enter) like on a typewriter, two spaces after each full stop.

import os
import re
import sys
import shutil
import subprocess
import tempfile
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Pt, Cm, RGBColor

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
from page_picture import page_picture as draw_page  # noqa: E402
STATIC = os.path.join(HERE, '..', '..', 'static', 'formatting-marks')

TITLE = 'Einladung zum Elternabend'
SUBTITLE = 'Klassenfahrt der 7b an den Kranichsee'
DATE = '2. März'
LETTER = [
    'im Mai fährt die 7b für fünf Tage in die Jugendherberge am Kranichsee. Auf dem',
    'Elternabend stellen wir das Programm vor, sprechen über die Kosten und beantworten',
    'Ihre Fragen. Wer nicht kommen kann, bekommt die Unterlagen über die Kinder.',
]
WHEN = 'Der Elternabend ist am Dienstag, dem 9. März, um 19 Uhr in Raum 104.'
REPLY = 'Bitte sagen Sie uns bis Freitag, ob Sie kommen.'
GREETING = 'Mit freundlichen Grüßen'
SIGNATURE = 'die Klassenleitung der 7b'


def base(size=12):
    doc = Document()
    section = doc.sections[0]
    section.page_width, section.page_height = Cm(21), Cm(29.7)
    section.left_margin = section.right_margin = Cm(2.5)
    section.top_margin, section.bottom_margin = Cm(2.5), Cm(2)
    normal = doc.styles['Normal']
    normal.font.name = 'Calibri'
    normal.font.size = Pt(size)
    normal.paragraph_format.space_before = Pt(0)
    normal.paragraph_format.space_after = Pt(0)
    normal.paragraph_format.line_spacing = 1.0
    doc.core_properties.author = 'Klassenleitung 7b'
    doc.core_properties.title = TITLE
    return doc


def run(paragraph, text, bold=False, size=None):
    r = paragraph.add_run(text)
    if bold:
        r.bold = True
    if size:
        r.font.size = Pt(size)
    return r


def starter(path, bigger=0):
    doc = base(12 + bigger)
    # the spaces were typed until it looked centred
    run(doc.add_paragraph(), ' ' * 38 + TITLE, bold=True, size=16 + bigger)
    run(doc.add_paragraph(), ' ' * 49 + SUBTITLE)
    doc.add_paragraph()
    run(doc.add_paragraph(), ' ' * 148 + DATE)
    doc.add_paragraph()
    run(doc.add_paragraph(), 'Liebe Eltern,')
    doc.add_paragraph()
    p = doc.add_paragraph()
    for i, line in enumerate(LETTER):
        r = run(p, line.replace('. ', '.  ') + (' ' if i < len(LETTER) - 1 else ''))
        if i < len(LETTER) - 1:
            r.add_break()
    doc.add_paragraph()
    run(doc.add_paragraph(), WHEN)
    doc.add_paragraph()
    run(doc.add_paragraph(), REPLY)
    doc.add_paragraph()
    doc.add_paragraph()
    run(doc.add_paragraph(), GREETING)
    run(doc.add_paragraph(), SIGNATURE)
    doc.save(path)


# The top of the starting file as Word or LibreOffice shows it with the
# formatting marks on: each space a blue »·«, each paragraph's end a blue
# »¶«. A »·« is a little wider than a space; each one is condensed by the
# difference (Carlito, the font LibreOffice uses for Calibri), so that the
# lines stay as long as in the file.
MARK_BLUE = RGBColor(0x24, 0x57, 0xA6)


def condense(run, twips):
    props = run._r.get_or_add_rPr()
    spacing = OxmlElement('w:spacing')
    spacing.set(qn('w:val'), str(twips))
    props.append(spacing)


def dot_correction(size, bold):
    from PIL import ImageFont
    path = subprocess.run(['fc-match', '-f', '%{file}', 'Carlito:bold' if bold else 'Carlito'],
                          capture_output=True, text=True, check=True).stdout
    font = ImageFont.truetype(path, 1000)
    # in twentieths of a point
    return round((font.getlength(' ') - font.getlength('·')) / 1000 * size * 20)


def marked(paragraph, text, bold=False, size=12):
    for piece in re.split(r'( +)', text):
        if not piece:
            continue
        if piece.startswith(' '):
            r = run(paragraph, '·' * len(piece), bold, size)
            r.font.color.rgb = MARK_BLUE
            condense(r, dot_correction(size, bold))
        else:
            run(paragraph, piece, bold, size)
    r = run(paragraph, '¶', bold, size)
    r.font.color.rgb = MARK_BLUE


def marks_top(path):
    doc = base()
    marked(doc.add_paragraph(), ' ' * 38 + TITLE, bold=True, size=16)
    marked(doc.add_paragraph(), ' ' * 49 + SUBTITLE)
    marked(doc.add_paragraph(), '')
    marked(doc.add_paragraph(), ' ' * 148 + DATE)
    marked(doc.add_paragraph(), '')
    marked(doc.add_paragraph(), 'Liebe Eltern,')
    doc.save(path)


def solution(path):
    doc = base()
    title = doc.add_paragraph()
    title.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run(title, TITLE, bold=True, size=16)
    sub = doc.add_paragraph()
    sub.alignment = WD_ALIGN_PARAGRAPH.CENTER
    sub.paragraph_format.space_after = Pt(24)
    run(sub, SUBTITLE)
    date = doc.add_paragraph()
    date.alignment = WD_ALIGN_PARAGRAPH.RIGHT
    date.paragraph_format.space_after = Pt(12)
    run(date, DATE)
    for text, after in [('Liebe Eltern,', 12), (' '.join(LETTER), 12), (WHEN, 12), (REPLY, 24), (GREETING, 0), (SIGNATURE, 0)]:
        p = doc.add_paragraph()
        p.paragraph_format.space_after = Pt(after)
        run(p, text)
    doc.save(path)


# The top of a document's first page as a picture for the page: LibreOffice
# makes a PDF, poppler a PNG at 2.5 times the size the page shows it, cwebp
# a lossless WebP (text stays sharp, the paper compresses to nothing).
def picture(docx, target, top_cm=1.6, height_cm=10.4, left_cm=0, width_cm=21):
    work = tempfile.mkdtemp()
    try:
        draw_page(docx, target, page=1, top_cm=top_cm, height_cm=height_cm, left_cm=left_cm, width_cm=width_cm)
    finally:
        shutil.rmtree(work, ignore_errors=True)


# ---- The extras (chapters for the quick, extra: true)

# Another round: the notice for the day of departure, typed like the
# invitation, shorter; the same rules.
NOTICE_TITLE = 'Abfahrt an den Kranichsee'
NOTICE_MEET = [
    'Wir treffen uns am Montag um 7.30 Uhr vor dem Haupteingang. Der Bus',
    'fährt pünktlich um 7.45 Uhr ab; wer später kommt, fährt nicht mit.',
]
NOTICE_BRING = 'Denkt an eure Krankenkassenkarte und an etwas zu essen für die Fahrt.'
NOTICE_SIGN = 'Eure Klassenleitung'


def notice(path):
    doc = base()
    run(doc.add_paragraph(), ' ' * 39 + NOTICE_TITLE, bold=True, size=16)
    doc.add_paragraph()
    doc.add_paragraph()
    p = doc.add_paragraph()
    for i, line in enumerate(NOTICE_MEET):
        r = run(p, line.replace('. ', '.  ') + (' ' if i < len(NOTICE_MEET) - 1 else ''))
        if i < len(NOTICE_MEET) - 1:
            r.add_break()
    doc.add_paragraph()
    run(doc.add_paragraph(), NOTICE_BRING)
    doc.add_paragraph()
    doc.add_paragraph()
    run(doc.add_paragraph(), ' ' * 131 + NOTICE_SIGN)
    doc.save(path)


def notice_solution(path):
    doc = base()
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.paragraph_format.space_after = Pt(24)
    run(p, NOTICE_TITLE, bold=True, size=16)
    for text, after in [(' '.join(NOTICE_MEET), 12), (NOTICE_BRING, 24)]:
        q = doc.add_paragraph()
        q.paragraph_format.space_after = Pt(after)
        run(q, text)
    q = doc.add_paragraph()
    q.alignment = WD_ALIGN_PARAGRAPH.RIGHT
    run(q, NOTICE_SIGN)
    doc.save(path)


# Six traps: the 7b's letter to the youth hostel. It looks finished, on
# paper and at a glance on screen; six things are built wrong, each one of
# the kinds of the invitation, and the check does not say where.
LETTER_TITLE = 'Ankunft am Kranichsee'
LETTER_DATE = '12. März'
LETTER_HELLO = 'Liebe Mitarbeiterinnen und Mitarbeiter der Jugendherberge,'
LETTER_WHO = ('wir sind die Klasse 7b und kommen im Mai für fünf Tage zu Ihnen. Wir sind 26 Schülerinnen und Schüler '
              'und zwei Lehrkräfte. Unser Bus kommt am Montag gegen 11 Uhr an.')
LETTER_ASK = ('Können wir unser Gepäck schon vor dem Mittagessen in die Zimmer bringen? Und gibt es am '
              'Abend einen Raum, in dem wir zusammen spielen können?')
LETTER_FOOD = ('Zwei von uns essen kein Fleisch, und eine Schülerin verträgt keine Nüsse. Sie bringt ihr eigenes '
               'Müsli mit, wenn das in Ordnung ist.')
# the word that ends the first line of LETTER_FOOD at 12 pt in LibreOffice
# and Word (looked at with »python3 dokumente.py lines«): the line break by
# hand goes there, where the line would end anyway
LETTER_FOOD_BREAK = 'ihr'
LETTER_THANKS = 'Vielen Dank für Ihre Antwort! Wir freuen uns schon sehr.'
LETTER_GREETING = 'Viele Grüße'
LETTER_SIGN = 'die Klasse 7b'


def letter_paragraph(doc, text, after=12):
    p = doc.add_paragraph()
    p.paragraph_format.space_after = Pt(after)
    run(p, text)
    return p


def letter(path, food_break=True):
    doc = base()
    run(doc.add_paragraph(), ' ' * 41 + LETTER_TITLE, bold=True, size=16)
    doc.paragraphs[-1].paragraph_format.space_after = Pt(12)
    date = doc.add_paragraph()
    date.paragraph_format.space_after = Pt(12)
    run(date, ' ' * 150 + LETTER_DATE)
    letter_paragraph(doc, LETTER_HELLO)
    letter_paragraph(doc, LETTER_WHO.replace('Lehrkräfte. ', 'Lehrkräfte.  '))
    # an indent of spaces, and an empty paragraph instead of a gap
    letter_paragraph(doc, '     ' + LETTER_ASK, after=0)
    # its mark in 9 pt: as high as the 12 pt gap of the others
    empty = doc.add_paragraph()
    marks = OxmlElement('w:rPr')
    size = OxmlElement('w:sz')
    size.set(qn('w:val'), '18')
    marks.append(size)
    empty._p.get_or_add_pPr().append(marks)
    p = doc.add_paragraph()
    p.paragraph_format.space_after = Pt(12)
    if food_break:
        head, tail = LETTER_FOOD.split(LETTER_FOOD_BREAK + ' ', 1)
        run(p, head + LETTER_FOOD_BREAK).add_break()
        run(p, tail)
    else:
        run(p, LETTER_FOOD)
    letter_paragraph(doc, LETTER_THANKS, after=24)
    letter_paragraph(doc, LETTER_GREETING, after=0)
    letter_paragraph(doc, LETTER_SIGN, after=0)
    doc.save(path)


def letter_solution(path):
    doc = base()
    title = letter_paragraph(doc, '')
    title.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run(title, LETTER_TITLE, bold=True, size=16)
    date = letter_paragraph(doc, '')
    date.alignment = WD_ALIGN_PARAGRAPH.RIGHT
    run(date, LETTER_DATE)
    for text, after in [(LETTER_HELLO, 12), (LETTER_WHO, 12), (LETTER_ASK, 12), (LETTER_FOOD, 12), (LETTER_THANKS, 24),
                        (LETTER_GREETING, 0), (LETTER_SIGN, 0)]:
        letter_paragraph(doc, text, after)
    doc.save(path)


# A secret message from the class trip: a and e swapped, i and o too. No
# word begins with a capital A, E, I or O, so that replacing with or without
# »Groß-/Kleinschreibung beachten« gives the same.
SECRET = [
    'Treffpunkt für die Nachtwanderung: um neun Uhr am Steg hinter der Jugendherberge.',
    'Bringt eine Taschenlampe mit, aber verratet niemandem etwas.',
    'Das Losungswort heißt Kranichfeder.',
]
SECRET_TITLE = 'Streng geheim'


def swapped(text):
    return text.translate(str.maketrans('aeio', 'eaoi'))


def secret(path, decoded=False):
    doc = base()
    p = doc.add_paragraph()
    p.paragraph_format.space_after = Pt(12)
    run(p, SECRET_TITLE, bold=True, size=16)
    for line in SECRET:
        q = doc.add_paragraph()
        q.paragraph_format.space_after = Pt(6)
        run(q, line if decoded else swapped(line))
    doc.save(path)


def joined(path):
    """the solution with the space after the first sentence's full stop
    gone, as when two spaces are replaced by nothing instead of one (an
    example: the check names the missing space, not two missing words)"""
    solution(path)
    doc = Document(path)
    import re
    end = re.compile(r'([a-zäöüß])\. ([A-ZÄÖÜ])')
    for p in doc.paragraphs:
        for r in p.runs:
            if end.search(r.text):
                r.text = end.sub(r'\1.\2', r.text, count=1)
                doc.save(path)
                return
    raise RuntimeError('no sentence to join')


def extras():
    notice(os.path.join(STATIC, '01 Abfahrt.docx'))
    notice_solution(os.path.join(HERE, 'stufen', 'abfahrt.docx'))
    joined(os.path.join(HERE, 'stufen', 'einladung-zusammen.docx'))
    letter(os.path.join(STATIC, '01 Brief an die Jugendherberge.docx'))
    letter_solution(os.path.join(HERE, 'stufen', 'brief.docx'))
    secret(os.path.join(STATIC, '01 Geheimbotschaft.docx'))
    secret(os.path.join(HERE, 'stufen', 'botschaft.docx'), decoded=True)
    print('written: static/formatting-marks/01 Abfahrt.docx, 01 Brief an die Jugendherberge.docx, 01 Geheimbotschaft.docx, stufen/abfahrt.docx, brief.docx, botschaft.docx')


# Where LibreOffice ends the lines of the letter without the break by hand
# (to choose LETTER_FOOD_BREAK): its text as a PDF, line by line.
def lines():
    work = tempfile.mkdtemp()
    try:
        path = os.path.join(work, 'brief.docx')
        letter(path, food_break=False)
        subprocess.run(['soffice', '--headless', f'-env:UserInstallation=file://{work}/profile', '--convert-to', 'pdf', '--outdir', work, path], check=True, capture_output=True)
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
    starter(os.path.join(STATIC, '01 Einladung.docx'))
    solution(os.path.join(HERE, 'stufen', 'einladung.docx'))
    work = tempfile.mkdtemp()
    try:
        # the invitation as printed, and after »one size larger« (prediction)
        picture(os.path.join(STATIC, '01 Einladung.docx'), os.path.join(HERE, 'bilder', 'einladung.svg'), height_cm=10.9)
        # the card in the catalog: 16:9, the text area of the invitation
        picture(os.path.join(STATIC, '01 Einladung.docx'), os.path.join(HERE, 'teaser.webp'), top_cm=1.2, height_cm=9.9, left_cm=1.7, width_cm=17.6)
        starter(os.path.join(work, 'groesser.docx'), bigger=2)
        picture(os.path.join(work, 'groesser.docx'), os.path.join(HERE, 'bilder', 'einladung-groesser.svg'), height_cm=14.6)
        # the top with the formatting marks on
        marks_top(os.path.join(work, 'zeichen.docx'))
        picture(os.path.join(work, 'zeichen.docx'), os.path.join(HERE, 'bilder', 'zeichen.svg'), top_cm=2.0, height_cm=4.4, left_cm=2.1, width_cm=16.9)
        # and the solution after the same step
        bigger = os.path.join(work, 'fertig-groesser.docx')
        doc = Document(os.path.join(HERE, 'stufen', 'einladung.docx'))
        doc.styles['Normal'].font.size = Pt(14)
        for p in doc.paragraphs:
            for r in p.runs:
                if r.font.size:
                    r.font.size = Pt(r.font.size.pt + 2)
        doc.save(bigger)
        picture(bigger, os.path.join(HERE, 'bilder', 'fertig-groesser.svg'), height_cm=12.4)
    finally:
        shutil.rmtree(work, ignore_errors=True)
    print('written: static/formatting-marks/01 Einladung.docx, stufen/einladung.docx, bilder/*.webp, teaser.webp')
