# The documents of »Tabellen«: the programme of the class trip set with tabs
# (the starting file) and as a table (the solution, task-examples.yaml).
# Run in the office image:
#
#     src/scripts/office python3 src/content/word-tables/dokumente.py
#
# The starting file looks like a table: day, time and programme stand in
# columns at two tab stops. As soon as an entry gets longer than its line,
# its rest starts again at the left margin, under the day.

import os
import shutil
import subprocess
import tempfile
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH, WD_TAB_ALIGNMENT
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Pt, Cm

HERE = os.path.dirname(os.path.abspath(__file__))
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
    solution(os.path.join(HERE, 'stufen', 'programm.docx'))
    work = tempfile.mkdtemp()
    try:
        crop = dict(top_cm=2.0, left_cm=2.0, width_cm=17.0)
        picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'bilder', 'programm.webp'), height_cm=7.3, **crop)
        starter(os.path.join(work, 'laenger.docx'), longer=True)
        picture(os.path.join(work, 'laenger.docx'), os.path.join(HERE, 'bilder', 'programm-laenger.webp'), height_cm=8.0, **crop)
        solution(os.path.join(work, 'tabelle.docx'), longer=True)
        picture(os.path.join(work, 'tabelle.docx'), os.path.join(HERE, 'bilder', 'programm-tabelle.webp'), top_cm=2.0, height_cm=7.0, left_cm=2.0, width_cm=17.0)
    finally:
        shutil.rmtree(work, ignore_errors=True)
    picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'teaser.webp'), top_cm=1.4, height_cm=9.9, left_cm=1.7, width_cm=17.6)
    print('written: static/word-tables/04 Programm.docx, stufen/programm.docx, bilder/*.webp, teaser.webp')
