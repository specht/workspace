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


def document(newspaper=False, hyphenation=True, title=TITLE, intro=INTRO, parts=PARTS):
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
    doc.core_properties.title = title
    doc.add_paragraph(title, style='Title')
    doc.add_paragraph(intro)
    for heading, texts in parts:
        doc.add_paragraph(heading, style='Heading 1')
        for text in texts:
            doc.add_paragraph(text)
    return doc


# ---- The extras (chapters for the quick, extra: true)
#
#     src/scripts/office python3 src/content/newspaper-page/dokumente.py extras
#
# Another round: the 7b's article on the toads, written as the cranes were.
TOADS_TITLE = 'Eimer für die Erdkröten'
TOADS_INTRO = ('Jedes Frühjahr wandern Hunderte Erdkröten über die Straße hinter unserer Schule zum Teich im '
               'Schulgarten. Damit sie dort sicher ankommen, hat die 7b im März zwei Wochen lang jeden Morgen geholfen.')
TOADS = [
    ('Der Weg zum Teich', [
        'Erdkröten verbringen den Winter eingegraben im Wald. Sobald die Nächte wärmer als etwa fünf Grad sind, machen '
        'sie sich auf den Weg zu dem Gewässer, in dem sie selbst geschlüpft sind. Manche wandern dafür mehr als einen '
        'Kilometer weit.',
        'Die meisten Kröten wandern in der Dämmerung und nachts, oft bei Regen. Kommt ein Auto, bleiben sie auf der '
        'Straße sitzen, statt wegzulaufen. Darum werden in manchen Nächten viele von ihnen überfahren.']),
    ('Der Zaun und die Eimer', [
        'Mit dem Naturschutzverein haben wir einen niedrigen Zaun aus Folie neben der Straße aufgestellt. Die Kröten '
        'laufen an ihm entlang, bis sie in einen der Eimer fallen, die alle zehn Meter in die Erde eingegraben sind.',
        'Jeden Morgen vor der ersten Stunde haben wir die Eimer geleert, die Kröten gezählt und auf der anderen '
        'Straßenseite am Teich wieder ausgesetzt. Am Ende waren es 412 Erdkröten und 9 Molche.']),
    ('Was im Teich passiert', [
        'Im Teich legen die Weibchen ihre Eier in langen Laichschnüren ab. Nach etwa zwei Wochen schlüpfen die '
        'Kaulquappen, und im Juni wandern Tausende winzige Kröten zurück in den Wald.',
        'Im nächsten Frühjahr wollen wir wieder helfen. Wer mitmachen möchte, meldet sich bei der Klassenleitung der 7b.']),
]
TOADS_FILE = '09 Erdkroeten.docx'
# The title over both columns: the cranes as a newspaper page (the page's
# solution), the title still in the left column.
SPAN_FILE = '09 Kraniche Zeitung.docx'


def span_word(path):
    """the title over both columns as Word makes it: the title in a section
    of one column, a continuous section break, the text in two columns"""
    import copy
    from docx.enum.section import WD_SECTION
    doc = document(newspaper=True)
    body = doc.sections[-1]._sectPr
    first = copy.deepcopy(body)
    first.find(qn('w:cols')).set(qn('w:num'), '1')
    title = doc.paragraphs[0]
    title._p.get_or_add_pPr().append(first)
    doc.sections[-1].start_type = WD_SECTION.CONTINUOUS
    doc.save(path)


def span_libreoffice(path):
    """the title over both columns as LibreOffice makes it: LibreOffice reads
    Word's columns of the page as a section (»Bereich«) of two columns around
    everything; that one set to one column (Format › Bereiche…), the text
    below the title in a new section of two (marked, Format › Spalten…)"""
    import shutil
    import subprocess
    import uno
    sys.path.insert(0, os.path.join(HERE, '..', 'pictures-in-text'))
    import dokumente as d
    process, profile, desktop = d.office()
    try:
        doc = desktop.loadComponentFromURL(uno.systemPathToFileUrl(os.path.abspath(os.path.join(STATIC, SPAN_FILE))),
                                           '_blank', 0, (d.prop('Hidden', True),))
        family = doc.StyleFamilies.getByName('PageStyles')
        for name in family.getElementNames():
            st = family.getByName(name)
            if st.isInUse():
                one = st.TextColumns
                one.setColumnCount(1)
                st.TextColumns = one
        sections = doc.getTextSections()
        for i in range(sections.getCount()):
            outer = sections.getByIndex(i)
            one = outer.TextColumns
            one.setColumnCount(1)
            outer.TextColumns = one
        text = doc.Text
        ps = list(d.paragraphs(doc))
        cursor = text.createTextCursorByRange(ps[1].getStart())
        cursor.gotoRange(ps[-1].getEnd(), True)
        section = doc.createInstance('com.sun.star.text.TextSection')
        columns = doc.createInstance('com.sun.star.text.TextColumns')
        columns.setColumnCount(2)
        section.TextColumns = columns
        text.insertTextContent(cursor, section, True)
        doc.storeToURL(uno.systemPathToFileUrl(os.path.abspath(path)), (d.prop('FilterName', 'MS Word 2007 XML'),))
        doc.close(True)
    finally:
        process.kill()
        subprocess.run(['pkill', '-f', 'pipe,name=pictures_in_text'], capture_output=True)
        shutil.rmtree(profile, ignore_errors=True)


# Fourteen traps: a letter to the families for the crane evening at the lake
# in October, built as the letter of »Kopfzeile, Fußzeile, Fußnoten« (its
# open_day): six traps of the first sheet, one of each sheet from the
# second to the eighth, and one of this one, a paragraph with a wider line
# spacing set by hand.
def sheet8():
    import importlib.util
    spec = importlib.util.spec_from_file_location('headers_and_footnotes', os.path.join(HERE, '..', 'headers-and-footnotes', 'dokumente.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


EVENING = {
    'title': 'Kranichabend am See',
    'date': '15. September',
    'hello': 'Liebe Familien,',
    'what': ('im Oktober rasten wieder Tausende Kraniche am Kranichsee. Am Samstag, dem 17. Oktober, sehen wir am '
             'Ufer zu, wie sie abends auf den See kommen.'),
    'two_spaces': 'Kranichsee. ',
    'more': 'Die Anmeldung finden Sie auf der zweiten Seite.',
    'headings': ['Der Ablauf', 'Was mitkommt', 'Der Weg zum Ufer'],
    'by_hand': 'Was mitkommt',
    'rooms': [('16:00 Uhr', 'Abfahrt mit dem Bus am Schultor'), ('17:00 Uhr', 'Führung mit dem Kapitän'),
              ('18:30 Uhr', 'Die Kraniche kommen zum See'), ('20:00 Uhr', 'Ankunft am Schultor')],
    'need': ['warme Kleidung', 'ein Fernglas', 'etwas zu trinken', 'eine Taschenlampe'],
    'picture_line': 'So sieht ein Kranich aus der Nähe aus:',
    'picture': os.path.join(HERE, '..', 'pictures-in-text', 'bilder', 'kranich-uckermark.jpg'),
    'picture_alt': 'Ein grauer Kranich mit schwarz-weißem Hals und rotem Fleck auf dem Kopf steht in einer hohen Wiese.',
    'credit': 'Foto: Charles J. Sharp, CC BY-SA 4.0',
    'wider': ('Am Ufer gibt es Ferngläser zum Ausleihen, und der Kapitän erzählt, woher die Kraniche kommen und wohin '
              'sie fliegen.'),
    'money': ('Vom Parkplatz gehen wir zehn Minuten zu Fuß zum Beobachtungsstand am Ufer. Dort warten wir leise, bis '
              'die ersten Kraniche über den See kommen; manchmal dauert das eine Stunde.'),
    'note_word': 'Beobachtungsstand',
    'note': 'Ein Beobachtungsstand ist ein Holzhaus mit Sehschlitzen, aus dem man Vögel beobachtet, ohne sie zu stören.',
    # the word that ends the first line of »money« (»python3 dokumente.py lines«)
    'break': 'Dort',
    'questions': 'Fragen beantwortet die Klassenleitung gern.',
    'slip': 'Anmeldung zum Kranichabend',
    'slip_lines': ['Name des Kindes: ______________________________',
                   'Wir kommen mit ______ Personen.',
                   'Ort, Datum und Unterschrift: ______________________________'],
    'title_pad': 50,
}
EVENING_FILE = '09 Kranichabend.docx'


def extras(only=None):
    os.makedirs(os.path.join(HERE, 'stufen'), exist_ok=True)
    toads = dict(title=TOADS_TITLE, intro=TOADS_INTRO, parts=TOADS)
    document(**toads).save(os.path.join(STATIC, TOADS_FILE))
    document(newspaper=True, **toads).save(os.path.join(HERE, 'stufen', 'erdkroeten.docx'))
    document(newspaper=True).save(os.path.join(STATIC, SPAN_FILE))
    span_word(os.path.join(HERE, 'stufen', 'titel-word.docx'))
    span_libreoffice(os.path.join(HERE, 'stufen', 'titel-libreoffice.docx'))
    s8 = sheet8()
    n = s8.open_day_empties(EVENING)
    print('empty paragraphs before the slip:', n)
    s8.open_day(os.path.join(STATIC, EVENING_FILE), n, c=EVENING)
    thirteen = os.path.join(HERE, 'stufen', 'kranichabend-dreizehn.docx')
    s8.open_day(thirteen, solved=True, c=EVENING)
    s8.open_day_footnote(thirteen, os.path.join(HERE, 'stufen', 'kranichabend.docx'), c=EVENING)
    print('written: static/newspaper-page/09 Erdkroeten.docx, 09 Kraniche Zeitung.docx, 09 Kranichabend.docx and the stages in stufen/')


if __name__ == '__main__' and sys.argv[1:] == ['lines']:
    sheet8().open_day_lines(EVENING)
    sys.exit(0)
if __name__ == '__main__' and sys.argv[1:] == ['extras']:
    extras()
    sys.exit(0)


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
