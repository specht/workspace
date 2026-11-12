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
import shutil
import subprocess
import tempfile
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.shared import Pt, Cm

HERE = os.path.dirname(os.path.abspath(__file__))
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
    solution(os.path.join(HERE, 'stufen', 'aushang.docx'))
    picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'bilder', 'aushang.webp'), top_cm=1.8, height_cm=11.6)
    picture(os.path.join(HERE, 'stufen', 'aushang.docx'), os.path.join(HERE, 'bilder', 'aushang-fertig.webp'), top_cm=1.8, height_cm=11.6)
    # the card in the catalog: 16:9
    picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'teaser.webp'), top_cm=1.6, height_cm=9.9, left_cm=1.7, width_cm=17.6)
    print('written: static/character-formatting/02 Aushang.docx, stufen/aushang.docx, bilder/*.webp, teaser.webp')
