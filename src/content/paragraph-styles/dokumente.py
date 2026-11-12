# The documents of »Formatvorlagen«: the 7b's diary of the class trip with
# every heading formatted by hand (the starting file) and built with heading
# styles (the solution, task-examples.yaml). Run in the office image:
#
#     src/scripts/office python3 src/content/paragraph-styles/dokumente.py
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
import shutil
import subprocess
import tempfile
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.shared import Pt, Cm, RGBColor

HERE = os.path.dirname(os.path.abspath(__file__))
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


def starter(path):
    doc = base()
    by_hand(doc, TITLE, 20, before=0, after=6, center=True)
    doc.add_paragraph(INTRO)
    for day, parts in DAYS:
        size = 13 if day.startswith('Mittwoch') else 14
        by_hand(doc, day, size, BLUE, before=18, after=6)
        for part, text in parts:
            if part:
                by_hand(doc, part, 12, before=8, after=2)
            doc.add_paragraph(text)
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


def solution(path, by_hand=False, part_style='Heading 2', changed=True):
    """the stages for the examples: by_hand: the styles given, but the
    days still bold, blue and 14 pt by hand; part_style: Heading 3 skips a
    level; changed: False leaves Überschrift 1 as it was"""
    doc = base()
    if changed:
        heading1(doc)
        matching(doc)
    t = doc.add_paragraph(TITLE, style='Title')
    t.alignment = WD_ALIGN_PARAGRAPH.CENTER
    doc.add_paragraph(INTRO)
    for day, parts in DAYS:
        p = doc.add_paragraph(style='Heading 1')
        r = p.add_run(day)
        if by_hand:
            r.bold = True
            r.font.size = Pt(14)
            r.font.color.rgb = BLUE
        for part, text in parts:
            if part:
                doc.add_paragraph(part, style=part_style)
            doc.add_paragraph(text)
    doc.save(path)


def picture(docx, target, top_cm=1.6, height_cm=12, left_cm=1.8, width_cm=17.4):
    work = tempfile.mkdtemp()
    try:
        subprocess.run(['soffice', '--headless', f'-env:UserInstallation=file://{work}/profile',
                        '--convert-to', 'pdf', '--outdir', work, docx], check=True, capture_output=True)
        pdf = os.path.join(work, os.path.splitext(os.path.basename(docx))[0] + '.pdf')
        dpi = 150
        px = lambda cm: int(round(cm / 2.54 * dpi))
        subprocess.run(['pdftoppm', '-r', str(dpi), '-png', '-f', '1', '-l', '1', '-singlefile',
                        '-x', str(px(left_cm)), '-y', str(px(top_cm)), '-W', str(px(width_cm)), '-H', str(px(height_cm)),
                        pdf, os.path.join(work, 'page')], check=True)
        subprocess.run(['cwebp', '-quiet', '-lossless', os.path.join(work, 'page.png'), '-o', target], check=True)
    finally:
        shutil.rmtree(work, ignore_errors=True)


if __name__ == '__main__':
    os.makedirs(STATIC, exist_ok=True)
    os.makedirs(os.path.join(HERE, 'stufen'), exist_ok=True)
    os.makedirs(os.path.join(HERE, 'bilder'), exist_ok=True)
    starter(os.path.join(STATIC, STARTER))
    solution(os.path.join(HERE, 'stufen', 'reisetagebuch.docx'))
    solution(os.path.join(HERE, 'stufen', 'noch-von-hand.docx'), by_hand=True)
    solution(os.path.join(HERE, 'stufen', 'ebene-3.docx'), part_style='Heading 3')
    solution(os.path.join(HERE, 'stufen', 'vorlage-wie-vorher.docx'), changed=False)
    work = tempfile.mkdtemp()
    try:
        picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'bilder', 'tagebuch.webp'), height_cm=14.5)
        picture(os.path.join(HERE, 'stufen', 'reisetagebuch.docx'), os.path.join(HERE, 'bilder', 'tagebuch-fertig.webp'), height_cm=14.5)
    finally:
        shutil.rmtree(work, ignore_errors=True)
    picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'teaser.webp'), top_cm=1.6, height_cm=9.8)
    print('written: static/paragraph-styles/06 Reisetagebuch.docx, stufen/reisetagebuch.docx, bilder/*.webp, teaser.webp')
