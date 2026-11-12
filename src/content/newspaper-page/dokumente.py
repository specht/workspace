# The documents of »Die Zeitungsseite«: an article for the 7b's newspaper as
# it was written, the way an office program starts a document (the starting
# file), and set as a newspaper sets it (the solution and a stage of
# task-examples.yaml). Run in the office image:
#
#     src/scripts/office python3 src/content/newspaper-page/dokumente.py
#
# The starting file: one column over the whole width, 2,5 cm margins,
# ragged right, paragraphs apart by spacing after them, line spacing 1,08,
# no hyphenation. The solution: two columns, 1,5 cm margins, and in the
# style »Standard« (Word: Normal) justified text, automatic hyphenation, a
# first-line indent instead of spacing, single line spacing. The stage
# »ohne-trennung«: the same without hyphenation (the gaps the page shows).

import os
import sys
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Pt, Cm, RGBColor

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
from page_picture import page_picture as draw_page  # noqa: E402

STATIC = os.path.join(HERE, '..', '..', 'static', 'newspaper-page')
STARTER = '09 Kraniche.docx'
GREEN = RGBColor(0x2E, 0x6B, 0x3A)

TITLE = 'Tausende Kraniche über dem See'
INTRO = ('Am Donnerstag sind wir mit dem Ausflugsschiff über den Kranichsee gefahren. Der Kapitän fährt seit über '
         'zwanzig Jahren auf dem See und kennt jeden Vogel, der dort lebt. Unterwegs hat er uns erzählt, warum der '
         'See seinen Namen trägt.')
PARTS = [
    ('Rast auf dem Weg nach Süden', [
        'Im Herbst machen hier Tausende Kraniche Rast. Sie kommen aus Skandinavien und aus dem Baltikum und fliegen '
        'weiter nach Spanien oder Nordafrika. Tagsüber suchen sie auf den abgeernteten Feldern nach Körnern, abends '
        'kommen sie zum Schlafen ins flache Wasser am Ufer. Dort sind sie sicher, weil kein Fuchs sie im Wasser '
        'ungesehen erreicht.',
        'Wenn die Kraniche am Abend über den See fliegen, hört man sie schon von Weitem. Sie rufen laut und '
        'trompetend, und die Schwärme ziehen in langen Ketten und Keilen über den Himmel. Der Kapitän sagt, an '
        'manchen Abenden seien es mehr als zehntausend Vögel.']),
    ('Ein großer Vogel', [
        'Ein Kranich steht gut einen Meter hoch, und seine Flügel sind von Spitze zu Spitze etwa zwei Meter breit. '
        'Erkennen kann man ihn am roten Fleck auf dem Kopf und am schwarz-weißen Hals. Im Flug streckt er den Hals '
        'lang nach vorn, ganz anders als ein Reiher.',
        'Früher brüteten in Deutschland nur noch wenige Kraniche. Seit Moore und Feuchtwiesen geschützt werden, '
        'sind es wieder viele Tausend Paare. Der Kapitän meint, ohne die Schutzgebiete gäbe es den Namen des Sees '
        'heute vielleicht nur noch auf alten Karten.']),
    ('Was wir selbst gesehen haben', [
        'Im Mai sind nur wenige Kraniche am See, denn die meisten brüten weiter im Norden. Trotzdem haben wir ein '
        'Paar entdeckt, das am Schilfrand nach Futter suchte. Der Kapitän hat den Motor ausgemacht, damit wir die '
        'Vögel nicht stören, und alle haben ganz leise durch die Ferngläser geschaut.',
        'Auf der Rückfahrt hat er uns noch gezeigt, wo im Herbst die Beobachtungsstände stehen. Einige von uns '
        'wollen im Oktober mit ihren Familien wiederkommen und sich den großen Kranichzug ansehen.']),
]


def document(newspaper=False, hyphenation=True):
    doc = Document()
    section = doc.sections[0]
    section.page_width, section.page_height = Cm(21), Cm(29.7)
    margin = Cm(1.5) if newspaper else Cm(2.5)
    section.left_margin = section.right_margin = section.top_margin = margin
    section.bottom_margin = Cm(1.5) if newspaper else Cm(2)
    normal = doc.styles['Normal']
    # German, as a German Word or LibreOffice writes it: the hyphenation
    # follows the language of the text
    lang = OxmlElement('w:lang')
    lang.set(qn('w:val'), 'de-DE')
    normal.element.get_or_add_rPr().append(lang)
    normal.font.name = 'Calibri'
    normal.font.size = Pt(11 if newspaper else 12)
    pf = normal.paragraph_format
    pf.space_before = Pt(0)
    if newspaper:
        # the newspaper's look, once, in the style »Standard«
        pf.alignment = WD_ALIGN_PARAGRAPH.JUSTIFY
        pf.first_line_indent = Cm(0.5)
        pf.space_after = Pt(0)
        pf.line_spacing = 1.0
    else:
        pf.space_after = Pt(8)
        pf.line_spacing = 1.08
    for name in ('Title', 'Heading 1'):
        style = doc.styles[name]
        style.font.color.rgb = GREEN
        # headings stay ragged and without an indent
        style.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
        style.paragraph_format.first_line_indent = Cm(0)
    doc.styles['Heading 1'].paragraph_format.space_before = Pt(10)
    doc.styles['Heading 1'].paragraph_format.space_after = Pt(3)
    if newspaper:
        # two columns, 0,8 cm apart (Word: Layout › Spalten)
        cols = section._sectPr.find(qn('w:cols'))
        if cols is None:
            cols = OxmlElement('w:cols')
            section._sectPr.append(cols)
        cols.set(qn('w:num'), '2')
        cols.set(qn('w:space'), str(int(0.8 / 2.54 * 1440)))
        if hyphenation:
            # Word: Layout › Silbentrennung › Automatisch
            settings = doc.settings.element
            auto = OxmlElement('w:autoHyphenation')
            settings.insert(0, auto)
    doc.core_properties.author = 'Klasse 7b'
    doc.core_properties.title = TITLE
    doc.add_paragraph(TITLE, style='Title')
    doc.add_paragraph(INTRO)
    for heading, texts in PARTS:
        doc.add_paragraph(heading, style='Heading 1')
        for text in texts:
            doc.add_paragraph(text)
    return doc


if __name__ == '__main__':
    os.makedirs(STATIC, exist_ok=True)
    for folder in ('stufen', 'bilder'):
        os.makedirs(os.path.join(HERE, folder), exist_ok=True)
    starter = os.path.join(STATIC, STARTER)
    document().save(starter)
    solution = os.path.join(HERE, 'stufen', 'zeitungsseite.docx')
    document(newspaper=True).save(solution)
    gaps = os.path.join(HERE, 'stufen', 'ohne-trennung.docx')
    document(newspaper=True, hyphenation=False).save(gaps)
    # the article as written, the columns with gaps, the finished page
    draw_page(starter, os.path.join(HERE, 'bilder', 'artikel.svg'), top_cm=2.0, height_cm=13.5, left_cm=2.1, width_cm=16.8)
    draw_page(gaps, os.path.join(HERE, 'bilder', 'ohne-trennung.svg'), top_cm=1.0, height_cm=10.5, left_cm=1.1, width_cm=18.8)
    draw_page(solution, os.path.join(HERE, 'bilder', 'zeitungsseite.svg'), top_cm=1.0, height_cm=27.7, left_cm=1.1, width_cm=18.8)
    draw_page(solution, os.path.join(HERE, 'teaser.webp'), top_cm=1.0, height_cm=10.6, left_cm=1.1, width_cm=18.8)
    print('written: static/newspaper-page/09 Kraniche.docx, stufen/*.docx, bilder/*.svg, teaser.webp')
