# The starting files of the sheets on presentations (docs/plan-praesentationen.md),
# described once and written twice: as .pptx by python-pptx, the way
# PowerPoint writes one (the master's text styles in the master, letters
# without formatting of their own unless set by hand), and as .odp by
# LibreOffice itself, for LibreOffice and Nextcloud Office. A .pptx opened in
# LibreOffice has every letter formatted by hand, so its slides would not
# follow a change of the master there (tried with LibreOffice 25.2); an .odp
# keeps the styles. Run in the office image (src/scripts/office).
#
#     deck = Deck()
#     s = deck.slide('content', 'Wie Kraniche ziehen', ['Im Herbst', ('nach Spanien', 1)], notes='…')
#     s.box(2, 5, 20, 3, 'Text in einem Textfeld', size=18)
#     s.picture('bilder/kranich.jpg', 20, 6, 12, 8, alt='Ein Kranich …')
#     deck.save('…/01 Kraniche')        # writes 01 Kraniche.pptx and 01 Kraniche.odp
#     slide_pictures('…/01 Kraniche.odp', {1: 'bilder/folie-1.svg'})

import copy
import os
import shutil
import subprocess
import tempfile

# PowerPoint's 16:9 slide: 33,867 × 19,05 cm
WIDTH_CM, HEIGHT_CM = 33.867, 19.05
# the master's sizes (pt): title, subtitle, the body's levels
TITLE_SIZE, SUBTITLE_SIZE, BODY_SIZES = 44, 24, [32, 28, 24, 20, 20]


class Slide:
    def __init__(self, layout, title, body, notes, subtitle):
        self.layout, self.title, self.body, self.notes, self.subtitle = layout, title, body, notes, subtitle
        self.boxes, self.pictures, self.bg = [], [], None
        self.title_format, self.body_format = {}, {}

    def box(self, x, y, w, h, text, size=18, font=None, colour=None, bold=False):
        """a text box put on by hand (cm); its letters formatted on the slide"""
        self.boxes.append(dict(x=x, y=y, w=w, h=h, text=text, size=size, font=font, colour=colour, bold=bold))
        return self

    def picture(self, path, x, y, w, h, alt=None):
        """a picture (cm), at this size: squeezed when its proportions differ"""
        self.pictures.append(dict(path=os.path.abspath(path), x=x, y=y, w=w, h=h, alt=alt))
        return self

    def background(self, colour):
        self.bg = colour
        return self

    def format_title(self, **kw):
        """the title's letters formatted by hand: size, font, colour, bold"""
        self.title_format = kw
        return self

    def format_body(self, **kw):
        self.body_format = kw
        return self


class Deck:
    def __init__(self, font='Calibri', title_colour=0x000000, body_colour=0x000000, background=None):
        self.font, self.title_colour, self.body_colour, self.bg = font, title_colour, body_colour, background
        self.slides = []

    def slide(self, layout, title=None, body=None, notes=None, subtitle=None):
        """layout: title, content, title_only or blank; body: lines, each a
        text or (text, level)"""
        s = Slide(layout, title, [b if isinstance(b, tuple) else (b, 0) for b in (body or [])], notes, subtitle)
        self.slides.append(s)
        return s

    def save(self, base):
        """base.pptx and base.odp"""
        write_pptx(self, base + '.pptx')
        from calc_build import Calc
        with Calc() as lo:
            write_odp(self, lo, base + '.odp')


# ---- .pptx (python-pptx)

PPTX_LAYOUTS = {'title': 0, 'content': 1, 'title_only': 5, 'blank': 6}


def _rgb(c):
    from pptx.dml.color import RGBColor
    return RGBColor((c >> 16) & 255, (c >> 8) & 255, c & 255)


def _pptx_template():
    """python-pptx's own template (4:3) widened to 16:9: every placeholder
    of the master and the layouts moved and stretched to the new width"""
    from pptx import Presentation
    from pptx.util import Emu
    prs = Presentation()
    f = 12192000 / prs.slide_width
    # the layouts first: a placeholder that inherits its place from the
    # master gets it written out, from the master's place before it widens
    for shapes in [layout.shapes for layout in prs.slide_layouts] + [prs.slide_master.shapes]:
        for sh in shapes:
            if sh.left is None:
                continue
            left, top, width, height = sh.left, sh.top, sh.width, sh.height
            sh.left, sh.top, sh.width, sh.height = Emu(int(left * f)), Emu(top), Emu(int(width * f)), Emu(height)
    prs.slide_width, prs.slide_height = Emu(12192000), Emu(6858000)
    return prs


def _pptx_master(prs, deck):
    """the theme's fonts and the master's sizes and colours"""
    from lxml import etree
    ns = {'a': 'http://schemas.openxmlformats.org/drawingml/2006/main',
          'p': 'http://schemas.openxmlformats.org/presentationml/2006/main'}
    master = prs.slide_master
    theme_part = next(r.target_part for r in master.part.rels.values() if r.reltype.endswith('/theme'))
    theme = etree.fromstring(theme_part.blob)
    for latin in theme.iterfind('.//a:fontScheme/*/a:latin', ns):
        latin.set('typeface', deck.font)
    theme_part._blob = etree.tostring(theme, xml_declaration=True, encoding='UTF-8', standalone=True)
    styles = master._element.find('p:txStyles', ns)

    def level(style, n, size, colour):
        p = styles.find(f'p:{style}/a:lvl{n}pPr', ns)
        if p is None:
            return
        d = p.find('a:defRPr', ns)
        d.set('sz', str(int(size * 100)))
        for old in d.findall('a:solidFill', ns):
            d.remove(old)
        fill = etree.SubElement(d, f'{{{ns["a"]}}}solidFill')
        etree.SubElement(fill, f'{{{ns["a"]}}}srgbClr', val='%06X' % colour)
        # solidFill must come before the fonts
        for tag in ('latin', 'ea', 'cs'):
            for el in d.findall(f'a:{tag}', ns):
                d.remove(el)
                d.append(el)
    level('titleStyle', 1, TITLE_SIZE, deck.title_colour)
    for n, size in enumerate(BODY_SIZES, 1):
        level('bodyStyle', n, size, deck.body_colour)
    if deck.bg is not None:
        master.background.fill.solid()
        master.background.fill.fore_color.rgb = _rgb(deck.bg)


def _pptx_runs(shape, fmt):
    from pptx.util import Pt
    for para in shape.text_frame.paragraphs:
        for run in para.runs:
            if 'size' in fmt:
                run.font.size = Pt(fmt['size'])
            if 'font' in fmt:
                run.font.name = fmt['font']
            if 'colour' in fmt:
                run.font.color.rgb = _rgb(fmt['colour'])
            if 'bold' in fmt:
                run.font.bold = fmt['bold']


def write_pptx(deck, path):
    from pptx.util import Cm, Pt
    from pptx.enum.text import MSO_AUTO_SIZE
    prs = _pptx_template()
    _pptx_master(prs, deck)
    for s in deck.slides:
        slide = prs.slides.add_slide(prs.slide_layouts[PPTX_LAYOUTS[s.layout]])
        for ph in list(slide.placeholders):
            kind = ph.placeholder_format.type
            idx = ph.placeholder_format.idx
            if idx == 0 and s.title is not None:
                ph.text_frame.text = s.title
                _pptx_runs(ph, s.title_format)
            elif idx == 1 and s.layout == 'title' and s.subtitle is not None:
                ph.text_frame.text = s.subtitle
            elif idx == 1 and s.body:
                tf = ph.text_frame
                for i, (text, lvl) in enumerate(s.body):
                    para = tf.paragraphs[0] if i == 0 else tf.add_paragraph()
                    para.text, para.level = text, lvl
                _pptx_runs(ph, s.body_format)
            elif kind is not None:
                # an empty placeholder is not shown, but it would still be
                # an empty frame to click on: off the slide
                ph._element.getparent().remove(ph._element)
        for b in s.boxes:
            shape = slide.shapes.add_textbox(Cm(b['x']), Cm(b['y']), Cm(b['w']), Cm(b['h']))
            tf = shape.text_frame
            tf.word_wrap = True
            tf.auto_size = MSO_AUTO_SIZE.NONE
            lines = b['text'].split('\n')
            for i, line in enumerate(lines):
                para = tf.paragraphs[0] if i == 0 else tf.add_paragraph()
                run = para.add_run()
                run.text = line
                run.font.size = Pt(b['size'])
                if b['font']:
                    run.font.name = b['font']
                if b['colour'] is not None:
                    run.font.color.rgb = _rgb(b['colour'])
                if b['bold']:
                    run.font.bold = True
        for p in s.pictures:
            pic = slide.shapes.add_picture(p['path'], Cm(p['x']), Cm(p['y']), Cm(p['w']), Cm(p['h']))
            # python-pptx names the file as the alt text: a picture without one has none
            pic._element.nvPicPr.cNvPr.set('descr', p['alt'] or '')
        if s.bg is not None:
            slide.background.fill.solid()
            slide.background.fill.fore_color.rgb = _rgb(s.bg)
        if s.notes:
            slide.notes_slide.notes_text_frame.text = s.notes
    prs.save(path)


# ---- .odp (LibreOffice through UNO)

ODP_LAYOUTS = {'title': 0, 'content': 1, 'title_only': 19, 'blank': 20}


def _hmm(cm):
    return int(round(cm * 1000))


def _cursor_format(text, fmt):
    c = text.createTextCursor()
    c.gotoStart(False)
    c.gotoEnd(True)
    if 'size' in fmt:
        c.CharHeight = fmt['size']
    if 'font' in fmt:
        c.CharFontName = fmt['font']
    if 'colour' in fmt:
        c.CharColor = fmt['colour']
    if 'bold' in fmt:
        c.CharWeight = 150 if fmt['bold'] else 100


def _odp_background(doc, page, colour):
    import uno
    bg = doc.createInstance('com.sun.star.drawing.Background')
    bg.FillStyle = uno.Enum('com.sun.star.drawing.FillStyle', 'SOLID')
    bg.FillColor = colour
    page.Background = bg


def write_odp(deck, lo, path):
    import uno
    from com.sun.star.awt import Point, Size
    from calc_build import prop
    doc = lo.desktop.loadComponentFromURL('private:factory/simpress', '_blank', 0, (prop('Hidden', True),))
    width, height = _hmm(WIDTH_CM), _hmm(HEIGHT_CM)
    # the master and its placeholders to the new size
    for i in range(doc.MasterPages.getCount()):
        m = doc.MasterPages.getByIndex(i)
        fx, fy = width / m.Width, height / m.Height
        m.Width, m.Height = width, height
        for k in range(m.getCount()):
            sh = m.getByIndex(k)
            pos, size = sh.Position, sh.Size
            sh.Position = Point(int(pos.X * fx), int(pos.Y * fy))
            sh.Size = Size(int(size.Width * fx), int(size.Height * fy))
        if deck.bg is not None:
            _odp_background(doc, m, deck.bg)
    pages = doc.DrawPages
    for i in range(pages.getCount()):
        p = pages.getByIndex(i)
        p.Width, p.Height = width, height
    family = doc.StyleFamilies.getByName(doc.MasterPages.getByIndex(0).Name)
    for name in ['title', 'subtitle'] + [f'outline{n}' for n in range(1, 10)]:
        family.getByName(name).CharFontName = deck.font
    family.getByName('title').CharHeight = TITLE_SIZE
    family.getByName('title').CharColor = deck.title_colour
    family.getByName('subtitle').CharHeight = SUBTITLE_SIZE
    for n in range(1, 10):
        st = family.getByName(f'outline{n}')
        st.CharHeight = BODY_SIZES[min(n, len(BODY_SIZES)) - 1]
        st.CharColor = deck.body_colour
    provider = lo.smgr.createInstanceWithContext('com.sun.star.graphic.GraphicProvider', lo.ctx)
    for n, s in enumerate(deck.slides):
        page = pages.getByIndex(0) if n == 0 else pages.insertNewByIndex(pages.getCount() - 1)
        page.Layout = ODP_LAYOUTS[s.layout]
        for k in range(page.getCount()):
            sh = page.getByIndex(k)
            t = sh.ShapeType
            if t.endswith('TitleTextShape') and s.title is not None:
                sh.String = s.title
                _cursor_format(sh.Text, s.title_format)
            elif t.endswith('SubtitleShape') and s.subtitle is not None:
                sh.String = s.subtitle
            elif t.endswith('OutlinerShape') and s.body:
                sh.String = '\n'.join(text for text, _ in s.body)
                paras = sh.Text.createEnumeration()
                for _, lvl in s.body:
                    paras.nextElement().NumberingLevel = lvl
                _cursor_format(sh.Text, s.body_format)
        for b in s.boxes:
            shape = doc.createInstance('com.sun.star.drawing.TextShape')
            page.add(shape)
            shape.Position = Point(_hmm(b['x']), _hmm(b['y']))
            shape.TextAutoGrowHeight = False
            shape.Size = Size(_hmm(b['w']), _hmm(b['h']))
            shape.String = b['text']
            fmt = {'size': b['size']}
            if b['font']:
                fmt['font'] = b['font']
            if b['colour'] is not None:
                fmt['colour'] = b['colour']
            if b['bold']:
                fmt['bold'] = True
            _cursor_format(shape.Text, fmt)
            shape.Size = Size(_hmm(b['w']), _hmm(b['h']))
        for p in s.pictures:
            graphic = provider.queryGraphic((prop('URL', uno.systemPathToFileUrl(p['path'])),))
            shape = doc.createInstance('com.sun.star.drawing.GraphicObjectShape')
            shape.Graphic = graphic
            page.add(shape)
            shape.Position = Point(_hmm(p['x']), _hmm(p['y']))
            shape.Size = Size(_hmm(p['w']), _hmm(p['h']))
            shape.Description = p['alt'] or ''
        if s.bg is not None:
            _odp_background(doc, page, s.bg)
        if s.notes:
            notes = page.NotesPage
            for k in range(notes.getCount()):
                if notes.getByIndex(k).ShapeType.endswith('NotesShape'):
                    notes.getByIndex(k).String = s.notes
    doc.storeToURL(uno.systemPathToFileUrl(os.path.abspath(path)), (prop('FilterName', 'impress8'),))
    doc.close(True)


def slide_pictures(path, targets):
    """slides of a saved presentation as SVG for the page, drawn by
    LibreOffice as the check draws them (PDF, then pdftocairo); targets:
    {slide number from 1: svg path}"""
    import uno
    from calc_build import Calc, prop
    folder = tempfile.mkdtemp(prefix='deck-')
    try:
        with Calc() as lo:
            doc = lo.desktop.loadComponentFromURL(uno.systemPathToFileUrl(os.path.abspath(path)), '_blank', 0,
                                                  (prop('Hidden', True),))
            pdf = os.path.join(folder, 'out.pdf')
            doc.storeToURL(uno.systemPathToFileUrl(pdf), (prop('FilterName', 'impress_pdf_Export'),))
            doc.close(True)
        for n, target in targets.items():
            subprocess.run(['pdftocairo', '-svg', '-f', str(n), '-l', str(n), pdf, target], check=True)
    finally:
        shutil.rmtree(folder, ignore_errors=True)
