# The documents of »Tabellen«: the programme of the class trip set with tabs
# (the starting file) and as a table (the solution, task-examples.yaml).
# Run in the office image:
#
#     src/scripts/office python3 src/content/word-tables/dokumente.py
#     src/scripts/office python3 src/content/word-tables/dokumente.py extras   # the documents of »Üben und knobeln«
#
# The starting file looks like a table: day, time and programme stand in
# columns at two tab stops. As soon as an entry gets longer than its line,
# its rest starts again at the left margin, under the day.

import os
import sys
import shutil
import subprocess
import tempfile
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH, WD_TAB_ALIGNMENT
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Pt, Cm

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
from page_picture import page_picture as draw_page  # noqa: E402
STATIC = os.path.join(HERE, '..', '..', 'static', 'word-tables')
STARTER = '04 Programm.docx'

TITLE = 'Programm der Klassenfahrt'
HEAD = ('Tag', 'Zeit', 'Programm')
ROWS = [('Montag', '10:00', 'Abfahrt am Schultor'),
        ('Montag', '14:00', 'Ankunft, Zimmer beziehen'),
        ('Dienstag', '9:00', 'Kletterwald'),
        ('Mittwoch', '9:00', 'Wanderung um den Kranichsee'),
        ('Donnerstag', '10:00', 'Schifffahrt auf dem Kranichsee'),
        ('Freitag', '11:00', 'Rückfahrt, Ankunft an der Schule gegen 15 Uhr')]
LONGER = 'Wanderung um den Kranichsee mit Picknick am Aussichtsturm und Rückweg über den Moorpfad'


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
    normal.paragraph_format.space_after = Pt(4)
    doc.core_properties.author = 'Klassenleitung 7b'
    doc.core_properties.title = TITLE
    return doc


def title(doc):
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.paragraph_format.space_after = Pt(18)
    r = p.add_run(TITLE)
    r.bold = True
    r.font.size = Pt(16)


def starter(path, longer=False):
    doc = base()
    title(doc)
    for i, row in enumerate([HEAD] + ROWS):
        if longer and row[0] == 'Mittwoch':
            row = (row[0], row[1], LONGER)
        p = doc.add_paragraph()
        p.paragraph_format.tab_stops.add_tab_stop(Cm(3.2), WD_TAB_ALIGNMENT.LEFT)
        p.paragraph_format.tab_stops.add_tab_stop(Cm(5.0), WD_TAB_ALIGNMENT.LEFT)
        r = p.add_run('\t'.join(row))
        if i == 0:
            r.bold = True
    doc.save(path)


def solution(path, longer=False):
    doc = base()
    title(doc)
    table = doc.add_table(rows=1 + len(ROWS), cols=3)
    table.style = 'Table Grid'
    for i, row in enumerate([HEAD] + ROWS):
        if longer and row[0] == 'Mittwoch':
            row = (row[0], row[1], LONGER)
        for j, text in enumerate(row):
            cell = table.rows[i].cells[j]
            cell.paragraphs[0].paragraph_format.space_after = Pt(0)
            r = cell.paragraphs[0].add_run(text)
            if i == 0:
                r.bold = True
    # the first row is the header: repeated on every page
    flag = OxmlElement('w:tblHeader')
    flag.set(qn('w:val'), 'true')
    table.rows[0]._tr.get_or_add_trPr().append(flag)
    # the widths in the grid too, which LibreOffice reads
    table.autofit = False
    widths = [Cm(3.0), Cm(2.0), Cm(11.0)]
    for column, width in zip(table.columns, widths):
        column.width = width
    for col, width in zip(table._tbl.tblGrid.findall(qn('w:gridCol')), widths):
        col.set(qn('w:w'), str(int(width.twips)))
    for row in table.rows:
        for cell, width in zip(row.cells, widths):
            cell.width = width
    doc.save(path)


def picture(docx, target, top_cm=1.6, height_cm=10.4, left_cm=0, width_cm=21):
    work = tempfile.mkdtemp()
    try:
        draw_page(docx, target, page=1, top_cm=top_cm, height_cm=height_cm, left_cm=left_cm, width_cm=width_cm)
    finally:
        shutil.rmtree(work, ignore_errors=True)


# ---- The extras (chapters for the quick, extra: true)

def widths_of(table, widths):
    """the columns' widths in the grid and in every cell (Word and
    LibreOffice read different ones)"""
    table.autofit = False
    for column, width in zip(table.columns, widths):
        column.width = width
    for col, width in zip(table._tbl.tblGrid.findall(qn('w:gridCol')), widths):
        col.set(qn('w:w'), str(int(width.twips)))
    for row in table.rows:
        for cell, width in zip(row.cells, widths):
            cell.width = width


def repeat_header(table):
    flag = OxmlElement('w:tblHeader')
    flag.set(qn('w:val'), 'true')
    table.rows[0]._tr.get_or_add_trPr().append(flag)


def fill(table, rows, bold_first=True):
    for i, row in enumerate(rows):
        for j, text in enumerate(row):
            cell = table.rows[i].cells[j]
            cell.paragraphs[0].paragraph_format.space_after = Pt(0)
            r = cell.paragraphs[0].add_run(text)
            if i == 0 and bold_first:
                r.bold = True


def plain(doc, text, after=12, align=None, font=None, size=None, bold=False):
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


def heading_line(doc, text):
    plain(doc, text, after=12, align=WD_ALIGN_PARAGRAPH.CENTER, size=16, bold=True)


# Another round: the sports day's results, in columns at three tab stops.
SPORT_TITLE = 'Ergebnisse vom Sportfest'
SPORT_INTRO = 'Die besten Ergebnisse jeder siebten Klasse:'
SPORT_ROWS = [('Disziplin', '7a', '7b', '7c'),
              ('Sprint 50 m', '7,4 s', '7,1 s', '7,6 s'),
              ('Weitsprung', '4,10 m', '4,35 m', '3,95 m'),
              ('Ballwurf', '42 m', '39 m', '45 m'),
              ('800-m-Lauf', '2:58 min', '3:05 min', '2:51 min'),
              ('Staffel', 'Platz 2', 'Platz 1', 'Platz 3')]
SPORT_END = 'Herzlichen Glückwunsch an alle, die mitgelaufen, gesprungen und geworfen sind!'


def results(path, solved=False):
    doc = base()
    doc.core_properties.title = SPORT_TITLE
    heading_line(doc, SPORT_TITLE)
    plain(doc, SPORT_INTRO, after=6)
    if solved:
        table = doc.add_table(rows=len(SPORT_ROWS), cols=4)
        table.style = 'Table Grid'
        fill(table, SPORT_ROWS)
        repeat_header(table)
        widths_of(table, [Cm(4.0), Cm(4.0), Cm(4.0), Cm(4.0)])
    else:
        for i, row in enumerate(SPORT_ROWS):
            p = doc.add_paragraph()
            for at in (4.0, 7.5, 11.0):
                p.paragraph_format.tab_stops.add_tab_stop(Cm(at), WD_TAB_ALIGNMENT.LEFT)
            r = p.add_run('\t'.join(row))
            r.bold = i == 0
    plain(doc, '', after=0)
    plain(doc, SPORT_END)
    doc.save(path)


# Two handles: the points after the sports day, a table already. The 7d's
# row is missing (the page gives it: a new row with Tab in the last cell);
# then sorted by points, the most first. The points have two and three
# digits: sorted like words, 98 comes first.
POINTS_TITLE = 'Punktestand der siebten Klassen'
POINTS_INTRO = 'Nach dem Sportfest zählen wir für jede Klasse die Medaillen und die Punkte aller Disziplinen.'
POINTS_ROWS = [('Klasse', 'Medaillen', 'Punkte'), ('7a', '13', '112'), ('7b', '15', '98'), ('7c', '13', '125')]
POINTS_ADDED = ('7d', '11', '104')


def points(path, solved=False):
    doc = base()
    doc.core_properties.title = POINTS_TITLE
    heading_line(doc, POINTS_TITLE)
    plain(doc, POINTS_INTRO, after=6)
    rows = POINTS_ROWS
    if solved == 'as words':
        # sorted like words, digit by digit: 98, 125, 112, 104
        rows = [rows[0]] + sorted(rows[1:] + [POINTS_ADDED], key=lambda r: r[2], reverse=True)
    elif solved:
        rows = [rows[0]] + sorted(rows[1:] + [POINTS_ADDED], key=lambda r: -int(r[2]))
    table = doc.add_table(rows=len(rows), cols=3)
    table.style = 'Table Grid'
    fill(table, rows)
    widths_of(table, [Cm(4.0), Cm(4.0), Cm(4.0)])
    doc.save(path)


# Nine traps: the 7b's letter to the parents about the project week. The
# six of the first sheet, the font of the second, the typed point of the
# third and one of this sheet: a table without borders whose last row is
# no row but a paragraph below it, day and work parted by a tab at the
# column's place.
WEEK_TITLE = 'Projektwoche der 7b'
WEEK_DATE = '15. Juni'
WEEK_HELLO = 'Liebe Eltern,'
WEEK_WHAT = ('vom 22. bis 26. Juni ist an unserer Schule Projektwoche. Die 7b baut in dieser Woche Vogelhäuser für den '
             'Schulgarten. Zum Schulfest am Ende der Woche hängen wir sie auf.')
WEEK_TIMES = 'Es geht jeden Tag um 8 Uhr los und endet um 13 Uhr.'
WEEK_PLAN_HEAD = 'So sieht die Woche aus:'
WEEK_PLAN = [('Montag', 'Pläne zeichnen und Holz aussuchen'), ('Dienstag', 'Sägen und schleifen'),
             ('Mittwoch', 'Zusammenbauen'), ('Donnerstag', 'Streichen'), ('Freitag', 'Aufhängen und Schulfest')]
WEEK_BRING_HEAD = 'Bitte geben Sie Ihrem Kind mit:'
WEEK_BRING = ['Kleidung, die schmutzig werden darf', 'eine Schürze oder ein altes Hemd', 'ein Pausenbrot', 'etwas zu trinken']
WEEK_GIFTS = ('Wer zu Hause Holzreste, Schrauben oder Farbe übrig hat, kann sie gern mitgeben. Wir freuen uns über '
              'jede Spende.')
# the word that ends the first line of WEEK_GIFTS (»python3 dokumente.py lines«)
WEEK_BREAK = 'Wir'
WEEK_FEST = 'Das Schulfest beginnt am Freitag um 15 Uhr.'
WEEK_GREETING = 'Viele Grüße'
WEEK_SIGN = 'die Klasse 7b'
WEEK_COLUMN = Cm(3.5)


def bullet(doc, text, last=False):
    p = doc.add_paragraph(style='List Bullet')
    p.paragraph_format.space_after = Pt(12 if last else 2)
    p.add_run(text)
    return p


def plan_table(doc, rows):
    """no borders, no margins in the cells: the second column's text
    begins at WEEK_COLUMN in Word and LibreOffice alike"""
    table = doc.add_table(rows=len(rows), cols=2)
    fill(table, rows, bold_first=False)
    widths_of(table, [WEEK_COLUMN, Cm(12.5)])
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


def week(path, gifts_break=True):
    doc = base()
    doc.core_properties.title = WEEK_TITLE
    plain(doc, ' ' * 49 + WEEK_TITLE, size=16, bold=True)
    plain(doc, ' ' * 150 + WEEK_DATE)
    plain(doc, WEEK_HELLO)
    plain(doc, WEEK_WHAT.replace('Schulgarten. ', 'Schulgarten.  '))
    plain(doc, '     ' + WEEK_TIMES, after=0)
    # an empty paragraph instead of a gap, its mark small enough to look
    # like the 12 pt of the others
    empty = doc.add_paragraph()
    empty.paragraph_format.space_after = Pt(0)
    marks = OxmlElement('w:rPr')
    sz = OxmlElement('w:sz')
    sz.set(qn('w:val'), '16')
    marks.append(sz)
    empty._p.get_or_add_pPr().append(marks)
    plain(doc, WEEK_PLAN_HEAD, after=4)
    plan_table(doc, WEEK_PLAN[:-1])
    # the last row: a paragraph, the work at a tab stop where the column is
    p = doc.add_paragraph()
    p.paragraph_format.space_after = Pt(12)
    p.paragraph_format.tab_stops.add_tab_stop(WEEK_COLUMN, WD_TAB_ALIGNMENT.LEFT)
    p.add_run('\t'.join(WEEK_PLAN[-1]))
    plain(doc, WEEK_BRING_HEAD, after=2)
    for item in WEEK_BRING[:-1]:
        bullet(doc, item)
    ref = doc.styles['List Bullet'].paragraph_format
    p = doc.add_paragraph()
    p.paragraph_format.space_after = Pt(12)
    p.paragraph_format.left_indent = ref.left_indent or Cm(0.63)
    p.paragraph_format.first_line_indent = ref.first_line_indent or Cm(-0.63)
    p.paragraph_format.tab_stops.add_tab_stop(p.paragraph_format.left_indent)
    p.add_run('•\t' + WEEK_BRING[-1])
    p, r = plain(doc, '')
    if gifts_break:
        head, tail = WEEK_GIFTS.split(WEEK_BREAK + ' ', 1)
        r.text = head + WEEK_BREAK
        r.add_break()
        p.add_run(tail)
    else:
        r.text = WEEK_GIFTS
    plain(doc, WEEK_FEST, after=24, font='Arial', size=11)
    plain(doc, WEEK_GREETING, after=0)
    plain(doc, WEEK_SIGN, after=0)
    doc.save(path)


def week_solution(path, outside=False):
    """outside: all fixed but the last row of the plan (an example that
    finds eight of nine)"""
    doc = base()
    plain(doc, WEEK_TITLE, align=WD_ALIGN_PARAGRAPH.CENTER, size=16, bold=True)
    plain(doc, WEEK_DATE, align=WD_ALIGN_PARAGRAPH.RIGHT)
    plain(doc, WEEK_HELLO)
    plain(doc, WEEK_WHAT)
    plain(doc, WEEK_TIMES)
    plain(doc, WEEK_PLAN_HEAD, after=4)
    plan_table(doc, WEEK_PLAN[:-1] if outside else WEEK_PLAN)
    if outside:
        p = doc.add_paragraph()
        p.paragraph_format.tab_stops.add_tab_stop(WEEK_COLUMN, WD_TAB_ALIGNMENT.LEFT)
        p.add_run('\t'.join(WEEK_PLAN[-1]))
    p, _ = plain(doc, WEEK_BRING_HEAD, after=2)
    p.paragraph_format.space_before = Pt(12)
    for i, item in enumerate(WEEK_BRING):
        bullet(doc, item, last=i == len(WEEK_BRING) - 1)
    plain(doc, WEEK_GIFTS)
    plain(doc, WEEK_FEST, after=24)
    plain(doc, WEEK_GREETING, after=0)
    plain(doc, WEEK_SIGN, after=0)
    doc.save(path)


def extras():
    os.makedirs(os.path.join(HERE, 'stufen'), exist_ok=True)
    results(os.path.join(STATIC, '04 Sportfest.docx'))
    results(os.path.join(HERE, 'stufen', 'sportfest.docx'), solved=True)
    points(os.path.join(STATIC, '04 Punktestand.docx'))
    points(os.path.join(HERE, 'stufen', 'punktestand.docx'), solved=True)
    week(os.path.join(STATIC, '04 Projektwoche.docx'))
    week_solution(os.path.join(HERE, 'stufen', 'projektwoche.docx'))
    # examples halfway: the points sorted like words; the letter with only
    # the row outside the table left
    points(os.path.join(HERE, 'stufen', 'punktestand-wie-woerter.docx'), solved='as words')
    week_solution(os.path.join(HERE, 'stufen', 'projektwoche-acht.docx'), outside=True)
    print('written: static/word-tables/04 Sportfest.docx, 04 Punktestand.docx, 04 Projektwoche.docx and their solutions in stufen/')


# Where LibreOffice ends the lines of the letter without the break by hand
# (to choose WEEK_BREAK).
def lines():
    work = tempfile.mkdtemp()
    try:
        path = os.path.join(work, 'brief.docx')
        week(path, gifts_break=False)
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
    solution(os.path.join(HERE, 'stufen', 'programm.docx'))
    work = tempfile.mkdtemp()
    try:
        crop = dict(top_cm=2.0, left_cm=2.0, width_cm=17.0)
        picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'bilder', 'programm.svg'), height_cm=7.3, **crop)
        starter(os.path.join(work, 'laenger.docx'), longer=True)
        picture(os.path.join(work, 'laenger.docx'), os.path.join(HERE, 'bilder', 'programm-laenger.svg'), height_cm=8.0, **crop)
        solution(os.path.join(work, 'tabelle.docx'), longer=True)
        picture(os.path.join(work, 'tabelle.docx'), os.path.join(HERE, 'bilder', 'programm-tabelle.svg'), top_cm=2.0, height_cm=7.0, left_cm=2.0, width_cm=17.0)
    finally:
        shutil.rmtree(work, ignore_errors=True)
    picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'teaser.webp'), top_cm=1.4, height_cm=9.9, left_cm=1.7, width_cm=17.6)
    print('written: static/word-tables/04 Programm.docx, stufen/programm.docx, bilder/*.webp, teaser.webp')
