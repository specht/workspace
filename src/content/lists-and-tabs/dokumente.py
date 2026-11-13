# The documents of »Listen und Tabstopps«: the packing list and the costs
# of the class trip as someone typed them (the starting file) and built as
# they should be (the solution, task-examples.yaml). Run in the office image:
#
#     src/scripts/office python3 src/content/lists-and-tabs/dokumente.py
#     src/scripts/office python3 src/content/lists-and-tabs/dokumente.py extras   # the documents of »Üben und knobeln«
#
# The starting file: every item of the packing list starts with »- « typed
# by hand, and the prices are pushed to the right with spaces, as many as it
# took to look lined up in Calibri at 12 pt (measured with Carlito, which
# has Calibri's widths). In any other font they no longer are.

import os
import sys
import shutil
import subprocess
import tempfile
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH, WD_TAB_ALIGNMENT, WD_TAB_LEADER
from docx.shared import Pt, Cm
from PIL import ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
from page_picture import page_picture as draw_page  # noqa: E402
STATIC = os.path.join(HERE, '..', '..', 'static', 'lists-and-tabs')
STARTER = '03 Packliste.docx'
CARLITO = '/usr/share/fonts/truetype/crosextra/Carlito-Regular.ttf'

TITLE = 'Klassenfahrt an den Kranichsee'
PACK_HEAD = 'Was du mitnimmst'
PACK = ['Schlafsack oder Bettwäsche', 'Hausschuhe', 'Regenjacke und feste Schuhe', 'Taschenlampe',
        'Badesachen und Handtuch', 'Krankenkassenkarte (eine Kopie reicht)']
COST_HEAD = 'Was die Fahrt kostet'
COSTS = [('Busfahrt hin und zurück', '48,00 €'), ('Jugendherberge, 4 Nächte mit Vollpension', '156,00 €'),
         ('Kletterwald', '22,50 €'), ('Schifffahrt auf dem Kranichsee', '9,50 €'), ('Zusammen', '236,00 €')]
# where the prices end: the right edge of the text (16 cm)
RIGHT_CM = 16.0


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
    normal.paragraph_format.space_after = Pt(6)
    doc.core_properties.author = 'Klassenleitung 7b'
    doc.core_properties.title = TITLE
    return doc


def heading(doc, text, size=13, after=6, before=12, center=False):
    p = doc.add_paragraph()
    p.paragraph_format.space_before = Pt(before)
    p.paragraph_format.space_after = Pt(after)
    if center:
        p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    r = p.add_run(text)
    r.bold = True
    r.font.size = Pt(size)
    return p


def padded(label, price):
    """label, spaces, price: as many spaces as push the price's end to the
    right edge in Calibri 12 pt"""
    font = ImageFont.truetype(CARLITO, 120)    # 10 px per pt
    width = lambda s: font.getlength(s) / 10.0
    target = RIGHT_CM / 2.54 * 72
    space = width(' ')
    n = max(2, int(round((target - width(label) - width(price)) / space)) - 1)
    return label + ' ' * n + price


def starter(path):
    doc = base()
    heading(doc, TITLE, 16, after=18, before=0, center=True)
    heading(doc, PACK_HEAD)
    for item in PACK:
        p = doc.add_paragraph()
        p.paragraph_format.space_after = Pt(2)
        p.add_run('- ' + item)
    heading(doc, COST_HEAD, before=18)
    for label, price in COSTS:
        p = doc.add_paragraph()
        p.paragraph_format.space_after = Pt(2)
        r = p.add_run(padded(label, price))
        if label == 'Zusammen':
            r.bold = True
    doc.save(path)


def solution(path):
    doc = base()
    heading(doc, TITLE, 16, after=18, before=0, center=True)
    heading(doc, PACK_HEAD)
    for item in PACK:
        p = doc.add_paragraph(style='List Bullet')
        p.paragraph_format.space_after = Pt(2)
        p.add_run(item)
    heading(doc, COST_HEAD, before=18)
    for label, price in COSTS:
        p = doc.add_paragraph()
        p.paragraph_format.space_after = Pt(2)
        p.paragraph_format.tab_stops.add_tab_stop(Cm(RIGHT_CM), WD_TAB_ALIGNMENT.RIGHT, WD_TAB_LEADER.DOTS)
        r = p.add_run(label + '\t' + price)
        if label == 'Zusammen':
            r.bold = True
    doc.save(path)


def picture(docx, target, top_cm=1.6, height_cm=10.4, left_cm=0, width_cm=21, font=None):
    """the top of the first page as a lossless WebP; font: every run in this
    font first (the prediction: what happens in another font)"""
    work = tempfile.mkdtemp()
    try:
        if font:
            doc = Document(docx)
            doc.styles['Normal'].font.name = font
            for p in doc.paragraphs:
                for r in p.runs:
                    r.font.name = font
            docx = os.path.join(work, 'other.docx')
            doc.save(docx)
        draw_page(docx, target, page=1, top_cm=top_cm, height_cm=height_cm, left_cm=left_cm, width_cm=width_cm)
    finally:
        shutil.rmtree(work, ignore_errors=True)


# ---- The extras (chapters for the quick, extra: true)

def typed_paragraph(doc, text, after=2, bold=False, font=None, size=None):
    p = doc.add_paragraph()
    p.paragraph_format.space_after = Pt(after)
    r = p.add_run(text)
    r.bold = bold
    if font:
        r.font.name = font
    if size:
        r.font.size = Pt(size)
    return p


def tabbed(doc, text, stop_cm, kind, leader=None, bold=False):
    p = doc.add_paragraph()
    p.paragraph_format.space_after = Pt(2)
    if leader:
        p.paragraph_format.tab_stops.add_tab_stop(Cm(stop_cm), kind, leader)
    else:
        p.paragraph_format.tab_stops.add_tab_stop(Cm(stop_cm), kind)
    r = p.add_run(text)
    r.bold = bold
    return p


def width_pt(text, size=12):
    font = ImageFont.truetype(CARLITO, size * 10)
    return font.getlength(text) / 10.0


def padded_to(label, at_cm):
    """label and as many spaces as let the next text begin at at_cm in
    Calibri 12 pt"""
    n = max(2, int(round((at_cm / 2.54 * 72 - width_pt(label)) / width_pt(' '))))
    return label + ' ' * n


# Another round: the 7b's flea market, the list typed with dashes and the
# prices pushed right with spaces, like the packing list.
FLEA_TITLE = 'Flohmarkt der 7b'
FLEA_INTRO = 'Am Samstag vor den Sommerferien verkaufen wir auf dem Schulhof, was zu Hause keiner mehr braucht. Der Erlös geht an das Tierheim am Stadtrand.'
FLEA_BRING_HEAD = 'Was ihr mitbringen könnt'
FLEA_BRING = ['Bücher und Comics', 'Spiele und Puzzles, bei denen kein Teil fehlt', 'Kuscheltiere',
              'Kleidung, die euch nicht mehr passt', 'Selbst gebackenen Kuchen']
FLEA_PRICE_HEAD = 'Was es kostet'
FLEA_PRICES = [('Ein Buch oder Comic', '1,00 €'), ('Ein Spiel oder Puzzle', '2,50 €'), ('Ein Kuscheltier', '0,50 €'),
               ('Ein Stück Kuchen', '1,20 €'), ('Ein Becher Kakao', '0,80 €')]


def flea(path, solved=False):
    doc = base()
    doc.core_properties.title = FLEA_TITLE
    heading(doc, FLEA_TITLE, 16, after=12, before=0, center=True)
    typed_paragraph(doc, FLEA_INTRO, after=6)
    heading(doc, FLEA_BRING_HEAD)
    for item in FLEA_BRING:
        if solved:
            p = doc.add_paragraph(style='List Bullet')
            p.paragraph_format.space_after = Pt(2)
            p.add_run(item)
        else:
            typed_paragraph(doc, '– ' + item)
    heading(doc, FLEA_PRICE_HEAD, before=18)
    for label, price in FLEA_PRICES:
        if solved:
            tabbed(doc, label + '\t' + price, RIGHT_CM, WD_TAB_ALIGNMENT.RIGHT, WD_TAB_LEADER.DOTS)
        else:
            typed_paragraph(doc, padded(label, price))
    doc.save(path)


# Two handles: the plan of the hiking day. The facts are a column of text
# at an own left tab stop (one tab lands »Ziel« at 1.25 cm and the longer
# words at 2.5 cm, on the default stops); the rally is a numbered list with
# points one level down (Tab at the start of a point).
HIKE_TITLE = 'Wandertag auf den Hohen Stein'
HIKE_FACTS_HEAD = 'Das Wichtigste'
HIKE_FACTS = [('Ziel', 'der Hohe Stein, 612 m'), ('Treffpunkt', 'Parkplatz am Freibad'), ('Abfahrt', '8:15 Uhr mit dem Bus'),
              ('Zurück', 'gegen 16:30 Uhr'), ('Mitbringen', 'Proviant, einen Liter zu trinken, feste Schuhe'),
              ('Bei Regen', 'Besuch im Heimatmuseum')]
HIKE_STOP_CM = 3.5
HIKE_RALLY_HEAD = 'So läuft die Rallye am Gipfel'
HIKE_RALLY = [
    ('Jede Gruppe holt sich am Gipfelkreuz eine Karte mit fünf Stationen.',
     ['Sucht die Stationen in einer beliebigen Reihenfolge.', 'An jeder Station steht eine Frage; schreibt die Antwort auf die Karte.']),
    ('Nach einer Stunde treffen sich alle wieder am Gipfelkreuz.', []),
    ('Die Lehrkräfte sehen die Karten nach.',
     ['Jede richtige Antwort bringt einen Punkt.', 'Bei gleich vielen Punkten entscheidet die Schätzfrage.']),
    ('Die Gruppe mit den meisten Punkten sucht das erste Lied für die Rückfahrt aus.', []),
]


def numbered(doc, text, level=0):
    from docx.oxml import OxmlElement
    from docx.oxml.ns import qn
    p = doc.add_paragraph(style='List Number')
    p.paragraph_format.space_after = Pt(2)
    if level:
        num_id = doc.styles['List Number'].element.pPr.numPr.numId.val
        ppr = p._p.get_or_add_pPr()
        numpr = OxmlElement('w:numPr')
        ilvl = OxmlElement('w:ilvl')
        ilvl.set(qn('w:val'), str(level))
        nid = OxmlElement('w:numId')
        nid.set(qn('w:val'), str(num_id))
        numpr.append(ilvl)
        numpr.append(nid)
        ppr.append(numpr)
    p.add_run(text)
    return p


def hike(path, solved=False, flat=False):
    doc = base()
    doc.core_properties.title = HIKE_TITLE
    heading(doc, HIKE_TITLE, 16, after=12, before=0, center=True)
    heading(doc, HIKE_FACTS_HEAD)
    for label, value in HIKE_FACTS:
        if solved:
            tabbed(doc, label + '\t' + value, HIKE_STOP_CM, WD_TAB_ALIGNMENT.LEFT)
        else:
            typed_paragraph(doc, padded_to(label, HIKE_STOP_CM) + value)
    heading(doc, HIKE_RALLY_HEAD, before=18)
    for i, (step, points) in enumerate(HIKE_RALLY, 1):
        if solved:
            numbered(doc, step)
        else:
            typed_paragraph(doc, f'{i}. ' + step)
        for point in points:
            if solved:
                numbered(doc, point, level=0 if flat else 1)
            else:
                typed_paragraph(doc, '      - ' + point)
    doc.save(path)


# Eight traps: the 7b's letter to the parents about the sports day. The six
# kinds of the first sheet, the font of the second (one sentence in
# Arial) and one of this sheet: the last point of the list typed, a »•«
# and a tab, indented like the others.
SPORT_TITLE = 'Unser Sportfest'
SPORT_DATE = '2. Juni'
SPORT_HELLO = 'Liebe Eltern,'
SPORT_WHEN = ('am Freitag, dem 12. Juni, ist unser Sportfest. Es beginnt um 9 Uhr auf dem Sportplatz hinter der Schule und '
              'endet gegen 13 Uhr. Danach gibt es für alle Würstchen und Salat.')
SPORT_HELP = 'Die 7b hilft beim Aufbau und betreut den Stand mit den Getränken.'
SPORT_BRING_HEAD = 'Bitte geben Sie Ihrem Kind mit:'
SPORT_BRING = ['Sportsachen und Turnschuhe', 'eine Mütze oder Kappe gegen die Sonne', 'genug zu trinken', 'Sonnencreme']
SPORT_EXCUSE = ('Wer bei einer Disziplin nicht mitmachen kann, bringt bitte eine Entschuldigung mit. Für alle anderen gibt '
                'es am Ende eine Urkunde.')
# the word that ends the first line of SPORT_EXCUSE (»python3 dokumente.py lines«)
SPORT_BREAK = 'alle'
SPORT_STORM = 'Bei Gewitter fällt das Sportfest aus; dann ist Unterricht nach Plan.'
SPORT_GREETING = 'Viele Grüße'
SPORT_SIGN = 'die Klasse 7b'


def plain(doc, text, after=12, align=None, font='Calibri', size=12, bold=False):
    p = doc.add_paragraph()
    p.paragraph_format.space_after = Pt(after)
    if align is not None:
        p.alignment = align
    r = p.add_run(text)
    r.font.name = font
    r.font.size = Pt(size)
    r.bold = bold
    return p, r


def bullet(doc, text, last=False):
    p = doc.add_paragraph(style='List Bullet')
    p.paragraph_format.space_after = Pt(12 if last else 2)
    p.add_run(text)
    return p


def sport(path, excuse_break=True):
    from docx.oxml import OxmlElement
    from docx.oxml.ns import qn
    doc = base()
    doc.core_properties.title = SPORT_TITLE
    plain(doc, ' ' * 52 + SPORT_TITLE, size=16, bold=True)
    plain(doc, ' ' * 151 + SPORT_DATE)
    plain(doc, SPORT_HELLO)
    plain(doc, SPORT_WHEN.replace('13 Uhr. ', '13 Uhr.  '))
    plain(doc, '     ' + SPORT_HELP, after=0)
    # an empty paragraph instead of a gap, its mark small enough to look
    # like the 12 pt of the others
    empty = doc.add_paragraph()
    empty.paragraph_format.space_after = Pt(0)
    marks = OxmlElement('w:rPr')
    sz = OxmlElement('w:sz')
    sz.set(qn('w:val'), '16')
    marks.append(sz)
    empty._p.get_or_add_pPr().append(marks)
    plain(doc, SPORT_BRING_HEAD, after=2)
    for item in SPORT_BRING[:-1]:
        bullet(doc, item)
    # the last point typed: a bullet and a tab, indented like the list
    ref = doc.styles['List Bullet'].paragraph_format
    p = doc.add_paragraph()
    p.paragraph_format.space_after = Pt(12)
    p.paragraph_format.left_indent = ref.left_indent or Cm(0.63)
    p.paragraph_format.first_line_indent = ref.first_line_indent or Cm(-0.63)
    p.paragraph_format.tab_stops.add_tab_stop(p.paragraph_format.left_indent)
    p.add_run('•\t' + SPORT_BRING[-1])
    p, r = plain(doc, '')
    if excuse_break:
        head, tail = SPORT_EXCUSE.split(SPORT_BREAK + ' ', 1)
        r.text = head + SPORT_BREAK
        r.add_break()
        r2 = p.add_run(tail)
        r2.font.name = 'Calibri'
        r2.font.size = Pt(12)
    else:
        r.text = SPORT_EXCUSE
    plain(doc, SPORT_STORM, after=24, font='Arial', size=11)
    plain(doc, SPORT_GREETING, after=0)
    plain(doc, SPORT_SIGN, after=0)
    doc.save(path)


def sport_solution(path, typed_last=False):
    """typed_last: all fixed but the typed point of the list (an example
    that finds seven of eight)"""
    doc = base()
    plain(doc, SPORT_TITLE, align=WD_ALIGN_PARAGRAPH.CENTER, size=16, bold=True)
    plain(doc, SPORT_DATE, align=WD_ALIGN_PARAGRAPH.RIGHT)
    plain(doc, SPORT_HELLO)
    plain(doc, SPORT_WHEN)
    plain(doc, SPORT_HELP)
    plain(doc, SPORT_BRING_HEAD, after=2)
    for i, item in enumerate(SPORT_BRING):
        if typed_last and i == len(SPORT_BRING) - 1:
            p = doc.add_paragraph()
            p.paragraph_format.space_after = Pt(12)
            p.add_run('•\t' + item)
        else:
            bullet(doc, item, last=i == len(SPORT_BRING) - 1)
    plain(doc, SPORT_EXCUSE)
    plain(doc, SPORT_STORM, after=24)
    plain(doc, SPORT_GREETING, after=0)
    plain(doc, SPORT_SIGN, after=0)
    doc.save(path)


def list_spilled(path):
    """the solution with the heading after the packing list a point of
    the list too (too much selected)"""
    solution(path)
    doc = Document(path)
    p = next(p for p in doc.paragraphs if p.text == COST_HEAD)
    p.style = doc.styles['List Bullet']
    doc.save(path)


def extras():
    os.makedirs(os.path.join(HERE, 'stufen'), exist_ok=True)
    list_spilled(os.path.join(HERE, 'stufen', 'packliste-zu-viel.docx'))
    flea(os.path.join(STATIC, '03 Flohmarkt.docx'))
    flea(os.path.join(HERE, 'stufen', 'flohmarkt.docx'), solved=True)
    hike(os.path.join(STATIC, '03 Wandertag.docx'))
    hike(os.path.join(HERE, 'stufen', 'wandertag.docx'), solved=True)
    sport(os.path.join(STATIC, '03 Sportfest.docx'))
    sport_solution(os.path.join(HERE, 'stufen', 'sportfest.docx'))
    # examples halfway: the points of the rally left on the top level; the
    # letter with only the typed point left
    hike(os.path.join(HERE, 'stufen', 'wandertag-eine-ebene.docx'), solved=True, flat=True)
    sport_solution(os.path.join(HERE, 'stufen', 'sportfest-sieben.docx'), typed_last=True)
    print('written: static/lists-and-tabs/03 Flohmarkt.docx, 03 Wandertag.docx, 03 Sportfest.docx and their solutions in stufen/')


# Where LibreOffice ends the lines of the letter without the break by hand
# (to choose SPORT_BREAK).
def lines():
    work = tempfile.mkdtemp()
    try:
        path = os.path.join(work, 'brief.docx')
        sport(path, excuse_break=False)
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
    solution(os.path.join(HERE, 'stufen', 'packliste.docx'))
    # the costs: as typed, in another font, and built with a tab stop
    costs = dict(top_cm=8.25, height_cm=6.3, left_cm=2.0, width_cm=17.0)
    picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'bilder', 'kosten.svg'), **costs)
    picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'bilder', 'kosten-andere-schrift.svg'), font='Georgia', **costs)
    picture(os.path.join(HERE, 'stufen', 'packliste.docx'), os.path.join(HERE, 'bilder', 'kosten-tabstopp.svg'), font='Georgia', **costs)
    picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'teaser.webp'), top_cm=1.6, height_cm=9.9, left_cm=1.7, width_cm=17.6)
    print('written: static/lists-and-tabs/03 Packliste.docx, stufen/packliste.docx, bilder/*.webp, teaser.webp')
