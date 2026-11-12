# The documents of »Listen und Tabstopps«: the packing list and the costs
# of the class trip as someone typed them (the starting file) and built as
# they should be (the solution, task-examples.yaml). Run in the office image:
#
#     src/scripts/office python3 src/content/lists-and-tabs/dokumente.py
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
