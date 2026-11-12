# The documents of »Seiten und Verzeichnisse«: the 7b's travel report, six
# pages, put together in a hurry (the starting file), and built with page
# breaks, a table of contents and page numbers (the solution and the stages
# of task-examples.yaml). Run in the office image:
#
#     src/scripts/office python3 src/content/pages-and-contents/dokumente.py
#
# The starting file: a title page with a table of contents typed by hand
# (dots and page numbers), each day pushed onto a page of its own with empty
# paragraphs (as many as it took: the office service lays the document out
# and one more is added until the heading jumps), »Seite 1« typed into the
# footer, the same on every page. The headings already have their styles
# (worksheet 6).
#
# The solution is made by LibreOffice through UNO: no empty paragraphs,
# »Seitenumbruch oberhalb« in the style Überschrift 1, a table of contents
# from the headings, the page number as a field in the footer.

import os
import shutil
import subprocess
import sys
import tempfile
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.shared import Pt, Cm, RGBColor

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
from page_picture import page_picture as draw_page  # noqa: E402
STATIC = os.path.join(HERE, '..', '..', 'static', 'pages-and-contents')
STARTER = '07 Reisebericht.docx'
sys.path.insert(0, os.path.join(HERE, '..', '..', 'office'))
sys.path.insert(0, os.path.join(HERE, '..', 'pictures-in-text'))

GREEN = RGBColor(0x2E, 0x6B, 0x3A)
TITLE = 'Reisebericht der 7b'
SUBTITLE = 'Unsere Klassenfahrt an den Kranichsee'
# (day, [(part or None, text)]), as in »Formatvorlagen«, each day longer
DAYS = [
    ('Montag: Anreise', [(None,
        'Um zehn Uhr fuhr der Bus am Schultor ab. Nach einer Pause an einer Raststätte kamen wir gegen zwei Uhr '
        'an der Jugendherberge an. Die Zimmer haben vier Betten, und von unserem sieht man den See.'),
        (None, 'Nach dem Abendessen haben wir die Gegend erkundet. Hinter der Jugendherberge beginnt der Wald, '
         'und ein schmaler Weg führt hinunter zum Bootssteg.')]),
    ('Dienstag: Kletterwald', [(None,
        'Im Kletterwald gab es Strecken in drei Höhen. Fast alle haben die mittlere geschafft, sechs sogar die '
        'höchste. Die Seilrutsche über den Bach wollten alle zweimal fahren.'),
        (None, 'Vorher gab es eine Einweisung: Immer ein Karabiner am Seil, beim Umhängen nie beide zugleich '
         'lösen. Wer unsicher war, durfte jederzeit an einer Leiter absteigen.')]),
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
MORE = (' Auf der Fahrt haben wir Spiele gespielt und Musik gehört. Zwei von uns ist im Bus schlecht geworden, '
        'aber nach der Pause ging es wieder. Am Ziel hat uns die Herbergsmutter begrüßt und alles gezeigt.')
# a heading pushed onto a new page leaves this much room at most below
PAGE_BOTTOM = 29.7 - 2.0


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
    for name, size in (('Heading 1', 16), ('Heading 2', 13)):
        doc.styles[name].font.color.rgb = GREEN
        doc.styles[name].font.size = Pt(size)
    doc.styles['Title'].font.color.rgb = GREEN
    doc.core_properties.author = 'Klasse 7b'
    doc.core_properties.title = TITLE
    return doc


def starter(path, empties, longer=False):
    """empties: how many empty paragraphs end the title page and each day
    (six numbers); longer: Monday with four sentences more (the prediction)"""
    doc = base()
    t = doc.add_paragraph(TITLE, style='Title')
    t.alignment = WD_ALIGN_PARAGRAPH.CENTER
    sub = doc.add_paragraph(SUBTITLE)
    sub.alignment = WD_ALIGN_PARAGRAPH.CENTER
    sub.paragraph_format.space_after = Pt(30)
    head = doc.add_paragraph()
    r = head.add_run('Inhalt')
    r.bold = True
    r.font.size = Pt(14)
    # typed: the name, dots until it looked about right, the page
    for i, (day, _) in enumerate(DAYS):
        doc.add_paragraph(day + ' ' + '.' * (60 - 2 * len(day) // 2 * 1 + (i % 2) * 3) + ' ' + str(i + 2))
    for _ in range(empties[0]):
        doc.add_paragraph()
    for k, (day, parts) in enumerate(DAYS):
        doc.add_paragraph(day, style='Heading 1')
        for j, (part, text) in enumerate(parts):
            if part:
                doc.add_paragraph(part, style='Heading 2')
            doc.add_paragraph(text + (MORE * 2 if longer and k == 0 and j == len(parts) - 1 else ''))
        if k + 1 < len(DAYS):
            for _ in range(empties[k + 1]):
                doc.add_paragraph()
    footer = doc.sections[0].footer.paragraphs[0]
    footer.alignment = WD_ALIGN_PARAGRAPH.CENTER
    footer.add_run('Seite 1')
    doc.save(path)


def count_empties():
    """as many empty paragraphs as push each next heading onto the next
    page, found with the office service's layout"""
    import server
    work = tempfile.mkdtemp()
    empties = [0] * len(DAYS)
    try:
        path = os.path.join(work, 'try.docx')
        for k in range(len(DAYS)):
            for _ in range(80):
                starter(path, empties)
                with open(path, 'rb') as f:
                    model = server.analyze(f.read(), 'try.docx')
                heading = [p for p in model['paragraphs'] if p['text'] == DAYS[k][0]][0]
                if heading['box']['page'] >= k + 2:
                    break
                # room left below the heading on its page, in empty lines
                room = (PAGE_BOTTOM / 2.54 * 72 - heading['box']['top']) / 22.0
                empties[k] += max(1, int(room * 0.7))
        return empties
    finally:
        server.OFFICE.stop()
        shutil.rmtree(work, ignore_errors=True)


# ---- the solution, made by LibreOffice

def solution(path, stale=False, breaks=True):
    """stale: a heading renamed after the table of contents was made (it
    shows the old name); breaks: False leaves the days without page breaks
    (a stage: no empty paragraphs, but no new pages either)"""
    import uno
    import dokumente as d
    process, profile, desktop = d.office()
    try:
        doc = desktop.loadComponentFromURL(uno.systemPathToFileUrl(os.path.abspath(os.path.join(STATIC, STARTER))),
                                           '_blank', 0, (d.prop('Hidden', True),))
        # the empty paragraphs and the typed table of contents go, one at a
        # time (a removal disposes the paragraph after it)
        gone = lambda text: text == '' or any(text.startswith(day + ' .') for day, _ in DAYS)
        for _ in range(500):
            p = next((p for p in d.paragraphs(doc) if gone(p.getString())), None)
            if p is None:
                break
            d.remove_paragraph(doc, p)
        head = next(p for p in d.paragraphs(doc) if p.getString() == 'Inhalt')
        # the table of contents where »Inhalt« was
        index = doc.createInstance('com.sun.star.text.ContentIndex')
        index.CreateFromOutline = True
        index.Level = 2
        index.Title = 'Inhalt'
        doc.Text.insertTextContent(head.getStart(), index, False)
        d.remove_paragraph(doc, head)
        # removing a paragraph joins the next one into it, with its format:
        # the days and their parts get their styles back
        parts = {part for _, ps in DAYS for part, _ in ps if part}
        for p in d.paragraphs(doc):
            text = p.getString()
            if any(text == day for day, _ in DAYS):
                p.ParaStyleName = 'Heading 1'
                # the break on each day, not in the style: Word bases the
                # heading of the table of contents on Überschrift 1, which
                # would push »Inhalt« onto a page of its own too
                if breaks:
                    p.BreakType = uno.Enum('com.sun.star.style.BreakType', 'PAGE_BEFORE')
            elif text in parts:
                p.ParaStyleName = 'Heading 2'
        # the page number as a field in the footer
        for name in doc.StyleFamilies.getByName('PageStyles').getElementNames():
            st = doc.StyleFamilies.getByName('PageStyles').getByName(name)
            if not (st.isInUse() and st.FooterIsOn):
                continue
            text = st.FooterText
            text.setString('')
            cursor = text.createTextCursor()
            text.insertString(cursor, 'Seite ', False)
            field = doc.createInstance('com.sun.star.text.textfield.PageNumber')
            field.NumberingType = 4
            field.SubType = uno.Enum('com.sun.star.text.PageNumberType', 'CURRENT')
            text.insertTextContent(cursor, field, False)
        index.update()
        if stale:
            for p in d.paragraphs(doc):
                if p.getString() == 'Freitag: Rückfahrt':
                    p.setString('Freitag: Heimfahrt')
        doc.storeToURL(uno.systemPathToFileUrl(os.path.abspath(path)), (d.prop('FilterName', 'MS Word 2007 XML'),))
        doc.close(True)
    finally:
        process.kill()
        subprocess.run(['pkill', '-f', 'pipe,name=pictures_in_text'], capture_output=True)
        shutil.rmtree(profile, ignore_errors=True)


def picture(docx, target, page=1, top_cm=1.6, height_cm=12, left_cm=1.8, width_cm=17.4):
    work = tempfile.mkdtemp()
    try:
        draw_page(docx, target, page=page, top_cm=top_cm, height_cm=height_cm, left_cm=left_cm, width_cm=width_cm)
    finally:
        shutil.rmtree(work, ignore_errors=True)


if __name__ == '__main__':
    os.makedirs(STATIC, exist_ok=True)
    os.makedirs(os.path.join(HERE, 'stufen'), exist_ok=True)
    os.makedirs(os.path.join(HERE, 'bilder'), exist_ok=True)
    # lock files left by a LibreOffice that did not close its document
    for folder in (STATIC, os.path.join(HERE, 'stufen')):
        for name in os.listdir(folder):
            if name.startswith('.~lock.'):
                os.remove(os.path.join(folder, name))
    empties = count_empties()
    print('empty paragraphs:', empties)
    starter(os.path.join(STATIC, STARTER), empties)
    solution(os.path.join(HERE, 'stufen', 'reisebericht.docx'))
    solution(os.path.join(HERE, 'stufen', 'verzeichnis-veraltet.docx'), stale=True)
    solution(os.path.join(HERE, 'stufen', 'ohne-umbrueche.docx'), breaks=False)
    work = tempfile.mkdtemp()
    try:
        longer = os.path.join(work, 'laenger.docx')
        starter(longer, empties, longer=True)
        picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'bilder', 'titelseite.svg'), height_cm=10)
        picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'bilder', 'dienstag.svg'), page=3, top_cm=0, height_cm=9)
        picture(longer, os.path.join(HERE, 'bilder', 'dienstag-weiter-unten.svg'), page=3, top_cm=0, height_cm=9)
        picture(os.path.join(HERE, 'stufen', 'reisebericht.docx'), os.path.join(HERE, 'bilder', 'titelseite-fertig.svg'), height_cm=10)
        picture(os.path.join(STATIC, STARTER), os.path.join(HERE, 'teaser.webp'), top_cm=1.6, height_cm=9.8)
    finally:
        shutil.rmtree(work, ignore_errors=True)
    with open(os.path.join(HERE, 'stufen', 'leere-absaetze.txt'), 'w') as f:
        f.write(' '.join(map(str, empties)) + '\n')
    print('written: static/pages-and-contents/07 Reisebericht.docx, stufen/*.docx, bilder/*.webp, teaser.webp')
