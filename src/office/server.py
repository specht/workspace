#!/usr/bin/env python3
# The office service (docs/plan-textverarbeitung.md, »Der Dienst«).
#
# POST /analyze with a document as the body (X-Filename: its name, for the
# format) answers with JSON: the paragraphs as LibreOffice reads them
# (text, style, alignment, spacing, the formatting set by hand, fields), the
# pages as SVG, and for each paragraph where it landed on them.
#
# One LibreOffice runs the whole time (a pipe, no network), one document at
# a time. A document is loaded with macros off and without updating links,
# changed only in memory (a bookmark at the start and the end of each
# paragraph, which the PDF keeps as named destinations: that is how the
# service knows where a paragraph is), exported as PDF and closed. Nothing
# is stored: each job works in a folder of its own in /tmp, removed after.
#
# Checks live in the backend (src/ruby/include/document_check.rb); this
# service only reads and draws.

import base64
import json
import os
import re
import shutil
import subprocess
import tempfile
import threading
import time
import traceback
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

import uno
from com.sun.star.beans import PropertyValue
from com.sun.star.beans.PropertyState import DIRECT_VALUE

PORT = int(os.environ.get('OFFICE_PORT', '8000'))
MAX_BYTES = 10 * 1024 * 1024
MAX_PAGES = 12
JOB_SECONDS = 40
PIPE = 'workspace_office'
FORMATS = {'docx', 'odt', 'doc', 'rtf'}
# spreadsheets (sheet.py)
SHEETS = {'xlsx', 'ods', 'xls'}
# presentations (slides.py)
PRESENTATIONS = {'pptx', 'odp', 'ppt'}

# LibreOffice in German: a spreadsheet's numbers and dates shown as a
# German program shows them (an .xlsx stores its formats without language)
GERMAN_ENV = dict(os.environ, LANG='de_DE.UTF-8', LC_ALL='de_DE.UTF-8', LANGUAGE='de')

# 1/100 mm (UNO) to PDF points
def pt(hmm):
    return round(hmm / 2540.0 * 72.0, 2)


def prop(name, value):
    p = PropertyValue()
    p.Name = name
    p.Value = value
    return p


class Office:
    """The LibreOffice process and the connection to it."""

    def __init__(self):
        self.process = None
        self.desktop = None
        self.profile = tempfile.mkdtemp(prefix='lo-profile-')

    def start(self):
        self.stop()
        self.process = subprocess.Popen([
            'soffice', '--headless', '--invisible', '--nologo', '--norestore',
            '--nodefault', '--nolockcheck',
            f'-env:UserInstallation=file://{self.profile}',
            f'--accept=pipe,name={PIPE};urp;StarOffice.ComponentContext',
        ], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, env=GERMAN_ENV)
        local = uno.getComponentContext()
        resolver = local.ServiceManager.createInstanceWithContext('com.sun.star.bridge.UnoUrlResolver', local)
        for _ in range(100):
            try:
                context = resolver.resolve(f'uno:pipe,name={PIPE};urp;StarOffice.ComponentContext')
                self.ctx, self.smgr = context, context.ServiceManager
                self.desktop = context.ServiceManager.createInstanceWithContext('com.sun.star.frame.Desktop', context)
                return
            except Exception:
                time.sleep(0.2)
        raise RuntimeError('LibreOffice did not start')

    def stop(self):
        self.desktop = None
        if self.process and self.process.poll() is None:
            self.process.kill()
            self.process.wait()
        self.process = None

    def alive(self):
        return self.process is not None and self.process.poll() is None and self.desktop is not None


OFFICE = Office()
LOCK = threading.Lock()


def enum_value(value):
    return getattr(value, 'value', value)


def is_direct(obj, name):
    try:
        return obj.getPropertyState(name) == DIRECT_VALUE
    except Exception:
        return False


CHAR_PROPS = ['CharWeight', 'CharHeight', 'CharFontName', 'CharPosture', 'CharUnderline', 'CharColor', 'CharCaseMap']


def read_portion(portion):
    kind = portion.TextPortionType
    item = {'type': kind}
    if kind == 'Text':
        item['text'] = portion.getString()
    elif kind == 'TextField':
        field = portion.TextField
        names = field.getSupportedServiceNames() if field else []
        item['field'] = next((n.split('.')[-1] for n in names if n.startswith('com.sun.star.text.textfield.')
                              or n.startswith('com.sun.star.text.TextField.')), 'Unknown')
        item['text'] = portion.getString()
        # a number range (Word: SEQ): the number of a caption, counted by
        # the program; its name is the label (»Abbildung«)
        if item['field'] == 'SetExpression':
            try:
                if field.SubType == 1:
                    item['sequence'] = field.TextFieldMaster.Name
            except Exception:
                pass
        # a date (Word: DATE): fixed, or today's whenever the document opens
        if item['field'] == 'DateTime':
            try:
                item['date'] = bool(field.IsDate)
                item['fixed'] = bool(field.IsFixed)
            except Exception:
                pass
    elif kind == 'Frame':
        # a picture in the line (»Mit Text in Zeile«): no text, but the
        # paragraph is not empty
        item['text'] = ''
    elif kind == 'Footnote':
        # the mark of a footnote the program numbers; its text is in
        # footnotes (read_footnotes)
        item['text'] = ''
    else:
        return None
    item['bold'] = portion.CharWeight >= 150
    item['italic'] = enum_value(portion.CharPosture) not in ('NONE', None)
    item['underline'] = portion.CharUnderline != 0
    item['size'] = round(portion.CharHeight, 1)
    item['font'] = portion.CharFontName
    item['color'] = portion.CharColor
    item['caps'] = portion.CharCaseMap == 1
    # raised (a footnote's number typed and made superscript by hand)
    item['superscript'] = (portion.CharEscapement or 0) > 0
    # lowered (CO2 with its 2 set below the line)
    item['subscript'] = (portion.CharEscapement or 0) < 0
    item['direct'] = [n for n in CHAR_PROPS if is_direct(portion, n)]
    return item


# the text columns of each page style, by name, for paragraph_columns
PAGE_COLUMNS = {}


def page_columns(doc):
    out = {}
    try:
        family = doc.StyleFamilies.getByName('PageStyles')
        for name in family.getElementNames():
            st = family.getByName(name)
            out[name] = st.TextColumns.ColumnCount if st.TextColumns is not None else 1
    except Exception:
        traceback.print_exc()
    return out


def paragraph_columns(para):
    """the text columns a paragraph stands in: those of its sections (a part
    in columns: LibreOffice »Bereich«, Word a section after a continuous
    break) or of its page style, the most of them; 1 for a heading over the
    columns of the text below"""
    most = 1
    try:
        section = para.TextSection
        while section is not None:
            if section.TextColumns is not None:
                most = max(most, section.TextColumns.ColumnCount)
            section = section.ParentSection
    except Exception:
        pass
    # the page style's columns (Word: Layout › Spalten for the whole
    # document); PAGE_COLUMNS is filled before the paragraphs are read
    try:
        most = max(most, PAGE_COLUMNS.get(para.PageStyleName, 1))
    except Exception:
        pass
    return most


def paragraph_border(para):
    """the width of the line on each side of a paragraph, in pt (0: none)"""
    out = {}
    for side in ('Top', 'Bottom', 'Left', 'Right'):
        width = 0
        try:
            line = getattr(para, side + 'Border')
            width = line.LineWidth or (line.OuterLineWidth + line.InnerLineWidth + line.LineDistance)
        except Exception:
            pass
        out[side.lower()] = round(width / 100 / 25.4 * 72, 2)
    return out


def paragraph_shade(para):
    """the colour behind a paragraph (#RRGGBB), or None (none, or white)"""
    try:
        if enum_value(para.FillStyle) == 'SOLID' and para.FillColor not in (-1, 0xFFFFFF):
            return '#%06X' % (para.FillColor & 0xFFFFFF)
    except Exception:
        pass
    try:
        if not para.ParaBackTransparent and para.ParaBackColor not in (-1, 0xFFFFFF):
            return '#%06X' % (para.ParaBackColor & 0xFFFFFF)
    except Exception:
        pass
    return None


def read_paragraph(para, styles, where):
    style = para.ParaStyleName
    try:
        display = styles.getByName(style).DisplayName
    except Exception:
        display = style
    adjust = {0: 'left', 1: 'right', 2: 'justify', 3: 'center', 4: 'justify'}.get(para.ParaAdjust, 'left')
    portions = []
    for portion in iter_enum(para):
        item = read_portion(portion)
        if item:
            portions.append(item)
    text = para.getString()
    # without the numbers of footnotes (»Karabiner1«): the text as it reads
    if any(q['type'] == 'Footnote' for q in portions):
        text = ''.join(q.get('text', '') for q in portions)
    numbering = None
    try:
        if para.NumberingIsNumber and para.NumberingRules is not None:
            numbering = {'label': para.ListLabelString, 'level': para.NumberingLevel}
    except Exception:
        pass
    try:
        context = bool(para.ParaContextMargin)
    except Exception:
        context = False
    # tab stops set on the paragraph (not the default ones every few cm)
    tabs = []
    try:
        for stop in para.ParaTabStops:
            align = enum_value(stop.Alignment).lower()
            if align == 'default':
                continue
            fill = stop.FillChar if isinstance(stop.FillChar, str) else ' '
            tabs.append({'at': pt(stop.Position), 'align': align, 'fill': fill if fill.strip() else ''})
    except Exception:
        pass
    return {
        'text': text,
        'columns': paragraph_columns(para),
        # a box around the paragraph (Word: Rahmen und Schattierung,
        # LibreOffice: Umrandung, Fläche): the lines' widths and the fill
        'border': paragraph_border(para),
        'shade': paragraph_shade(para),
        'style': style,
        'style_name': display,
        'outline': para.OutlineLevel,
        'align': adjust,
        'align_direct': is_direct(para, 'ParaAdjust'),
        'space_before': pt(para.ParaTopMargin),
        'space_after': pt(para.ParaBottomMargin),
        # no spacing between paragraphs of the same style (Word: »Keinen
        # Abstand zwischen Absätzen gleicher Formatierung einfügen«)
        'context_margin': context,
        'tabs': tabs,
        'indent_left': pt(para.ParaLeftMargin),
        'indent_first': pt(para.ParaFirstLineIndent),
        # automatic hyphenation; the line spacing (mode 0: proportional, in
        # percent; 1: at least, 2: leading, 3: fixed, in pt)
        'hyphenate': bool(para.ParaIsHyphenation),
        'line_spacing': line_spacing(para),
        'break': enum_value(para.BreakType),
        'numbering': numbering,
        'portions': portions,
        'where': where,
    }


def iter_enum(container):
    e = container.createEnumeration()
    while e.hasMoreElements():
        yield e.nextElement()


TABLES = []
# the names of the document's indexes (tables of contents), to know their
# paragraphs: those are in a text section of the index's name or below it
INDEX_NAMES = set()


def index_of(paragraph):
    try:
        section = paragraph.TextSection
    except Exception:
        return None
    for _ in range(5):
        if section is None:
            return None
        if section.Name in INDEX_NAMES:
            return section.Name
        try:
            section = section.ParentSection
        except Exception:
            return None
    return None


def read_indexes(doc):
    """The indexes: kind and the text the document has for them (as last
    updated by Word or LibreOffice)."""
    out = []
    try:
        indexes = doc.DocumentIndexes
        for i in range(indexes.getCount()):
            x = indexes.getByIndex(i)
            out.append({'name': x.Name, 'kind': 'content' if x.supportsService('com.sun.star.text.ContentIndex') else 'other',
                        'text': x.getAnchor().getString(), '_object': x})
    except Exception:
        traceback.print_exc()
    return out


def update_indexes(indexes):
    """After the pages are drawn: each index updated in memory, to tell
    whether the student's was up to date (its entries, without the page
    numbers, which Word may count otherwise)."""
    entries = lambda text: [re.sub(r'[\s.…·_-]*\d+\s*$', '', line).strip() for line in text.split('\n')[1:] if line.strip()]
    for x in indexes:
        obj = x.pop('_object', None)
        if obj is None:
            continue
        try:
            obj.update()
            x['updated'] = obj.getAnchor().getString()
            x['entries'] = entries(x['text'])
            x['current'] = entries(x['updated'])
            x['up_to_date'] = x['entries'] == x['current']
        except Exception:
            traceback.print_exc()


def read_footnotes(doc):
    """The footnotes the program numbers: their mark and their text."""
    out = []
    try:
        notes = doc.getFootnotes()
        for i in range(notes.getCount()):
            note = notes.getByIndex(i)
            out.append({'label': note.getAnchor().getString(), 'text': note.getString()})
    except Exception:
        traceback.print_exc()
    return out


def read_page_parts(doc, styles):
    """Headers and footers of the page styles in use: their text and the
    fields in them (a page number counted by the program)."""
    out = []
    try:
        family = doc.StyleFamilies.getByName('PageStyles')
        for name in family.getElementNames():
            st = family.getByName(name)
            if not st.isInUse():
                continue
            item = {'style': name}
            for part in ('Header', 'Footer'):
                if not getattr(st, part + 'IsOn'):
                    continue
                text = getattr(st, part + 'Text')
                portions = []
                for para in iter_enum(text):
                    if para.supportsService('com.sun.star.text.Paragraph'):
                        portions += [q for q in (read_portion(x) for x in iter_enum(para)) if q]
                item[part.lower()] = {'text': text.getString(), 'portions': portions}
            out.append(item)
    except Exception:
        traceback.print_exc()
    return out


def place_page_parts(parts, pages, part='footer'):
    """Where the footer's (or header's) words are on each page: the PDF's
    words in the bottom (top) eighth of a page that are words of a footer
    (header). A page without them has none (a title page)."""
    tokens = set()
    for item in parts:
        tokens.update((item.get(part) or {}).get('text', '').split())
    boxes = []
    if not tokens:
        return boxes
    for i, page in enumerate(pages):
        inside = (lambda y: y > page['height'] * 0.87) if part == 'footer' else (lambda y: y < page['height'] * 0.13)
        mine = [w for w in page['words'] if inside(w[1]) and w[4] in tokens]
        if mine:
            x0, y0 = min(w[0] for w in mine), min(w[1] for w in mine)
            boxes.append({'page': i + 1, 'x': round(x0, 1), 'y': round(y0, 1),
                          'w': round(max(w[2] for w in mine) - x0, 1), 'h': round(max(w[3] for w in mine) - y0, 1)})
    return boxes


def collect(text, styles, where, out, starts):
    """Paragraphs of a text (the body or a table cell), in order; the
    tables met on the way go to TABLES (rows, columns, header row)."""
    for element in iter_enum(text):
        if element.supportsService('com.sun.star.text.Paragraph'):
            here = where
            index = index_of(element)
            if index:
                here = dict(where, index=index)
            out.append(read_paragraph(element, styles, here))
            starts.append((element.getText(), element.getStart(), element.getEnd()))
        elif element.supportsService('com.sun.star.text.TextTable'):
            name = element.getName()
            try:
                TABLES.append({'name': name, 'rows': element.getRows().getCount(),
                               'cols': element.getColumns().getCount(),
                               'repeat_header': bool(element.RepeatHeadline),
                               'header_rows': int(getattr(element, 'HeaderRowCount', 0) or 0)})
            except Exception:
                TABLES.append({'name': name})
            for cell_name in element.getCellNames():
                cell = element.getCellByName(cell_name)
                collect(cell, styles, {'table': name, 'cell': cell_name}, out, starts)


def text_boxes(doc):
    """The frames with text: LibreOffice's own (a picture with its caption)
    and Word's text boxes, which LibreOffice reads as a shape on the draw
    page holding a frame (then the shape is what the text flows around).
    [(frame, shape or None)]"""
    found = []
    try:
        frames = doc.TextFrames
        for name in frames.getElementNames():
            found.append((frames.getByName(name), None))
    except Exception:
        traceback.print_exc()
    try:
        page = doc.DrawPage
        for i in range(page.getCount()):
            shape = page.getByIndex(i)
            try:
                if shape.TextBox and shape.TextBoxContent is not None:
                    found.append((shape.TextBoxContent, shape))
            except Exception:
                pass
    except Exception:
        traceback.print_exc()
    seen, out = set(), []
    for frame, shape in found:
        if frame.Name not in seen:
            seen.add(frame.Name)
            out.append((frame, shape))
    return out


def collect_frames(boxes, styles, out, starts):
    """The paragraphs in frames and text boxes: a caption that LibreOffice
    puts in a frame with its picture, or Word in a text box beside it."""
    for frame, _ in boxes:
        try:
            where = {'frame': frame.Name}
            try:
                where['width'] = pt(frame.Size.Width)
            except Exception:
                pass
            collect(frame.getText(), styles, where, out, starts)
        except Exception:
            traceback.print_exc()


WRAP = {'NONE': 'none', 'THROUGH': 'through', 'THROUGHT': 'through', 'PARALLEL': 'parallel',
        'DYNAMIC': 'dynamic', 'LEFT': 'left', 'RIGHT': 'right'}


def placement(obj):
    return {'anchor': enum_value(obj.AnchorType).lower(), 'wrap': WRAP.get(enum_value(obj.TextWrap), 'none'),
            'hori': obj.HoriOrient}


def read_pictures(doc, boxes):
    """The pictures: their alt text, size (shown and as stored), how they
    sit in the text, and the frame they are in, if any (a caption frame
    of LibreOffice: then the frame is what the text flows around)."""
    out = []
    try:
        objects = doc.GraphicObjects
        names = objects.getElementNames()
    except Exception:
        return out
    for name in names:
        try:
            o = objects.getByName(name)
            item = {'name': name, 'title': o.Title or '', 'description': o.Description or '',
                    'width': pt(o.Size.Width), 'height': pt(o.Size.Height)}
            item.update(placement(o))
            try:
                px = o.Graphic.SizePixel
                item['pixels'] = [px.Width, px.Height]
            except Exception:
                item['pixels'] = None
            try:
                a = o.ActualSize
                c = o.GraphicCrop
                item['actual'] = [a.Width, a.Height]
                item['crop'] = [c.Left, c.Right, c.Top, c.Bottom]
            except Exception:
                pass
            try:
                text = o.Anchor.getText()
                if text.supportsService('com.sun.star.text.TextFrame'):
                    shape = next((sh for fr, sh in boxes if fr.Name == text.Name), None)
                    item['frame'] = dict(placement(shape or text), name=text.Name,
                                         width=pt(text.Size.Width), height=pt(text.Size.Height))
            except Exception:
                pass
            out.append(item)
        except Exception:
            traceback.print_exc()
    return out


IMAGE = re.compile(r'<image top="([-\d.]+)" left="([-\d.]+)" width="([\d.]+)" height="([\d.]+)"')
PAGE_NO = re.compile(r'<page number="(\d+)"')


def place_pictures(pictures, pdf, folder):
    """Where each picture landed: the images of the PDF (pdftohtml), each
    matched to the picture whose shown size it has."""
    base = os.path.join(folder, 'images')
    subprocess.run(['pdftohtml', '-xml', '-zoom', '1', '-noroundcoord', '-q', '-nodrm', pdf, base],
                   capture_output=True, timeout=30)
    try:
        with open(base + '.xml', encoding='utf-8', errors='replace') as f:
            xml = f.read()
    except OSError:
        return
    found = []
    for chunk in xml.split('</page>'):
        m = PAGE_NO.search(chunk)
        if not m:
            continue
        for top, left, w, h in IMAGE.findall(chunk):
            found.append({'page': int(m.group(1)), 'x': float(left), 'y': float(top), 'w': float(w), 'h': float(h)})
    for p in pictures:
        best = None
        for f in found:
            d = abs(f['w'] - p['width']) + abs(f['h'] - p['height'])
            if d <= 0.15 * (p['width'] + p['height']) and (best is None or d < best[0]):
                best = (d, f)
        if best:
            found.remove(best[1])
            p['box'] = {k: round(v, 1) if isinstance(v, float) else v for k, v in best[1].items()}


def custom_docx_styles(source):
    """The names of the paragraph styles a .docx marks as made by the user
    (w:customStyle="1" in word/styles.xml, written by Word and by
    LibreOffice): LibreOffice does not keep that mark when it imports them.
    Only a styles.xml of up to 5 MB is read."""
    import zipfile
    import xml.etree.ElementTree as ET
    w = '{http://schemas.openxmlformats.org/wordprocessingml/2006/main}'
    try:
        with zipfile.ZipFile(source) as z:
            info = z.getinfo('word/styles.xml')
            if info.file_size > 5 * 1024 * 1024:
                return set()
            root = ET.fromstring(z.read(info))
    except Exception:
        return set()
    out = set()
    for st in root.iter(f'{w}style'):
        if st.get(f'{w}type') == 'paragraph' and st.get(f'{w}customStyle') in ('1', 'true'):
            name = st.find(f'{w}name')
            if name is not None and name.get(f'{w}val'):
                out.add(name.get(f'{w}val'))
    return out


# the paragraph styles Word and LibreOffice bring (their names in a file,
# English whatever the language of the program)
BUILTIN_STYLES = re.compile(
    r'^(Normal|Standard|Default Paragraph Style|No Spacing|Title|Subtitle|Quote|Intense Quote|Quotations|'
    r'List Paragraph|List|List Contents|List Heading|List (Bullet|Number|Continue)( \d)?|Caption|Heading|Heading \d|'
    r'TOC \d|TOC Heading|Contents \d+|Contents Heading|Index( \d)?|Index Heading|Bibliography|Body Text( \d| Indent| First Indent)?( \d)?|'
    r'Text Body|Text Body Indent|First Line Indent|Hanging Indent|Preformatted Text|Signature|Salutation|Marginalia|'
    r'Header|Footer|Header and Footer|Footnote|Footnote Text|Endnote|Endnote Text|Table Contents|Table Heading|'
    r'Frame Contents|Illustration|Figure|Drawing|Table|Text|Addressee|Sender|Block Text|Plain Text|Macro Text|'
    r'Envelope Address|Envelope Return|Comment Text|Balloon Text|Document Map|Normal \(Web\)|HTML Preformatted)$')


def user_defined(style, name, custom):
    """made in the document, not one the program brings: a .docx marks its
    own (custom), but LibreOffice's export does not, and LibreOffice counts
    Word's styles it has no match for (Quote) as made by the user; so
    besides the mark, any name that is none of the programs' own. An .odt
    says it itself (XStyle.isUserDefined)."""
    if custom is None:
        try:
            return bool(style.isUserDefined())
        except Exception:
            return False
    return name in custom or style.DisplayName in custom or not (BUILTIN_STYLES.match(name) or BUILTIN_STYLES.match(style.DisplayName or ''))


def read_styles(doc, names, custom=None):
    """The paragraph styles the document uses, as they are defined (what a
    paragraph of that style shows without formatting of its own): font,
    size, bold, italic, colour, outline level, made by the user or not."""
    out = {}
    try:
        family = doc.StyleFamilies.getByName('ParagraphStyles')
    except Exception:
        return out
    for name in sorted(set(names)):
        try:
            st = family.getByName(name)
            out[name] = {'display': st.DisplayName, 'parent': st.ParentStyle,
                         'font': st.CharFontName, 'size': round(st.CharHeight, 1),
                         'bold': st.CharWeight >= 150,
                         'italic': enum_value(st.CharPosture) not in ('NONE', None),
                         'color': st.CharColor, 'outline': getattr(st, 'OutlineLevel', 0),
                         # made in this document, not one of the program's own
                         'user': user_defined(st, name, custom)}
        except Exception:
            pass
    return out


def line_spacing(para):
    try:
        spacing = para.ParaLineSpacing
        mode = int(spacing.Mode)
        return {'mode': mode, 'value': spacing.Height if mode == 0 else pt(spacing.Height)}
    except Exception:
        return None


def page_margins(doc):
    styles = doc.StyleFamilies.getByName('PageStyles')
    cursor = doc.Text.createTextCursor()
    name = cursor.PageStyleName
    page = styles.getByName(name)
    # left and right: where the text begins and ends; margins: the four
    # page margins themselves
    return {'left': pt(page.LeftMargin), 'right': pt(page.Width - page.RightMargin),
            'width': pt(page.Width), 'height': pt(page.Height),
            'margins': {'left': pt(page.LeftMargin), 'right': pt(page.RightMargin),
                        'top': pt(page.TopMargin), 'bottom': pt(page.BottomMargin)}}


def text_columns(doc):
    """the most text columns anywhere: in a page style in use (Word: Layout
    › Spalten for the whole document) or in a section (a part in columns)"""
    most = 1
    try:
        family = doc.StyleFamilies.getByName('PageStyles')
        for name in family.getElementNames():
            st = family.getByName(name)
            if st.isInUse() and st.TextColumns is not None:
                most = max(most, st.TextColumns.ColumnCount)
        sections = doc.getTextSections()
        for i in range(sections.getCount()):
            columns = sections.getByIndex(i).TextColumns
            if columns is not None:
                most = max(most, columns.ColumnCount)
    except Exception:
        traceback.print_exc()
    return most


DEST = re.compile(r'^\s*(\d+)\s+\[\s*XYZ\s+([-\d.]+)\s+([-\d.]+)\s+\S+\s*\]\s+"([pe]\d+)"')


def destinations(pdf):
    out = subprocess.run(['pdfinfo', '-dests', pdf], capture_output=True, text=True, timeout=20).stdout
    found = {}
    for line in out.splitlines():
        m = DEST.match(line)
        if m:
            found[m.group(4)] = (int(m.group(1)), float(m.group(2)), float(m.group(3)))
    return found


WORD = re.compile(r'<word xMin="([\d.]+)" yMin="([\d.]+)" xMax="([\d.]+)" yMax="([\d.]+)">(.*?)</word>')
PAGE = re.compile(r'<page width="([\d.]+)" height="([\d.]+)">')


def words(pdf, folder):
    target = os.path.join(folder, 'words.html')
    subprocess.run(['pdftotext', '-bbox', pdf, target], capture_output=True, timeout=30)
    pages = []
    with open(target, encoding='utf-8', errors='replace') as f:
        for chunk in f.read().split('</page>'):
            m = PAGE.search(chunk)
            if not m:
                continue
            ws = [(float(a), float(b), float(c), float(d), html_unescape(t)) for a, b, c, d, t in WORD.findall(chunk)]
            pages.append({'width': float(m.group(1)), 'height': float(m.group(2)), 'words': ws})
    return pages


def html_unescape(text):
    return text.replace('&lt;', '<').replace('&gt;', '>').replace('&quot;', '"').replace('&#39;', "'").replace('&amp;', '&')


def place(paragraphs, dests, pages, margins):
    """Where each paragraph is: its page, top and bottom, the text area's
    left and right, and the words of the PDF inside it (top-left origin,
    points). A paragraph's area runs from its start to the start of the
    next paragraph on the same page; the words between belong to it."""
    starts = []
    for i in range(len(paragraphs)):
        d = dests.get(f'p{i}')
        e = dests.get(f'e{i}')
        if d is None:
            starts.append(None)
            continue
        page = pages[d[0] - 1] if d[0] - 1 < len(pages) else None
        if page is None:
            starts.append(None)
            continue
        top = page['height'] - d[2]
        end = (e[0], page['height'] - e[2]) if e else (d[0], top)
        starts.append((d[0], top, d[1], end))
    for i, p in enumerate(paragraphs):
        s = starts[i]
        if s is None:
            p['box'] = None
            continue
        page_no, top, x, (end_page, end_top) = s
        page = pages[page_no - 1]
        size = max([q['size'] for q in p['portions']] or [12])
        # in a frame: only the words within its width (the text beside it
        # is at the same height)
        width = (p.get('where') or {}).get('width')
        line = size * 1.2
        mine = []
        for pg in range(page_no, end_page + 1):
            for w in pages[pg - 1]['words']:
                lo = top - 1 if pg == page_no else -1
                hi = end_top + line * 0.75 if pg == end_page else 1e9
                mid = (w[1] + w[3]) / 2
                if width and not (x - 2 <= w[0] and w[2] <= x + width + 2):
                    continue
                if lo <= mid <= hi:
                    mine.append([pg] + list(w))
        # the next paragraph on the same page bounds an empty one
        following = next((t for t in starts[i + 1:] if t is not None), None)
        if mine:
            bottom = max(w[4] for w in mine if w[0] == end_page) if any(w[0] == end_page for w in mine) else end_top + line
        else:
            bottom = following[1] if following and following[0] == page_no and following[1] > top else top + line
        left, right = (x, x + width) if width else (margins['left'], margins['right'])
        p['box'] = {'page': page_no, 'top': round(top, 1), 'bottom': round(bottom, 1),
                    'end_page': end_page, 'x': round(x, 1),
                    'left': round(left, 1), 'right': round(right, 1)}
        p['words'] = [[w[0]] + [round(v, 1) for v in w[1:5]] + [w[5]] for w in mine]


def pdf_pages(pdf, folder, pages):
    """each page of a PDF as SVG, as the card shows it"""
    out = []
    for i in range(min(len(pages), MAX_PAGES)):
        target = os.path.join(folder, f'page-{i + 1}.svg')
        subprocess.run(['pdftocairo', '-svg', '-f', str(i + 1), '-l', str(i + 1), pdf, target],
                       capture_output=True, timeout=60)
        if not os.path.exists(target):
            break
        with open(target, 'rb') as f:
            svg = base64.b64encode(f.read()).decode('ascii')
        out.append({'width': pages[i]['width'], 'height': pages[i]['height'], 'svg': svg})
    return out


FONT_LINE = re.compile(r'^(\S+)\s+.*\s(yes|no)\s+(yes|no)\s+(yes|no)\s+\d+\s+\d+\s*$')


def analyze_pdf(data):
    """A PDF someone exported (the hand-in as PDF): read by poppler, not
    LibreOffice; its pages, its text and its fonts, whether each is
    embedded (so that it looks the same on every computer)."""
    folder = tempfile.mkdtemp(prefix='pdf-')
    try:
        pdf = os.path.join(folder, 'in.pdf')
        with open(pdf, 'wb') as f:
            f.write(data)
        if not data.startswith(b'%PDF'):
            return {'error': 'format', 'formats': sorted(FORMATS | {'pdf'})}
        pages = words(pdf, folder)
        if not pages:
            return {'error': 'pdf'}
        text = subprocess.run(['pdftotext', '-layout', '-l', str(MAX_PAGES), pdf, '-'],
                              capture_output=True, timeout=30).stdout.decode('utf-8', errors='replace')
        listing = subprocess.run(['pdffonts', pdf], capture_output=True, timeout=30).stdout.decode('utf-8', errors='replace')
        fonts = []
        for line in listing.splitlines()[2:]:
            m = FONT_LINE.match(line)
            if m:
                fonts.append({'name': m.group(1).split('+')[-1], 'embedded': m.group(2) == 'yes'})
        return {'pdf': True, 'paragraphs': [], 'text': text[:200000], 'fonts': fonts,
                'pages': pdf_pages(pdf, folder, pages), 'page_count': len(pages)}
    finally:
        shutil.rmtree(folder, ignore_errors=True)


def analyze(data, filename, probes=None):
    ext = filename.rsplit('.', 1)[-1].lower() if '.' in filename else ''
    if ext == 'pdf':
        return analyze_pdf(data)
    if ext not in FORMATS | SHEETS | PRESENTATIONS:
        return {'error': 'format', 'formats': sorted(FORMATS | SHEETS | PRESENTATIONS)}
    folder = tempfile.mkdtemp(prefix='job-')
    try:
        source = os.path.join(folder, f'in.{ext}')
        with open(source, 'wb') as f:
            f.write(data)
        if not OFFICE.alive():
            OFFICE.start()
        url = uno.systemPathToFileUrl(source)
        # MacroExecutionMode 0: never; UpdateDocMode 0: no links updated
        doc = OFFICE.desktop.loadComponentFromURL(url, '_blank', 0, (
            prop('Hidden', True), prop('ReadOnly', False), prop('MacroExecutionMode', 0),
            prop('UpdateDocMode', 0), prop('Silent', True)))
        if doc is not None and doc.supportsService('com.sun.star.sheet.SpreadsheetDocument'):
            import sheet
            try:
                return sheet.analyze(OFFICE, doc, folder, probes)
            finally:
                doc.close(True)
        if doc is not None and doc.supportsService('com.sun.star.presentation.PresentationDocument'):
            import slides
            try:
                return slides.analyze(OFFICE, doc, folder)
            finally:
                doc.close(True)
        if doc is None or not doc.supportsService('com.sun.star.text.TextDocument'):
            if doc is not None:
                doc.close(True)
            return {'error': 'not_text'}
        try:
            styles = doc.StyleFamilies.getByName('ParagraphStyles')
            paragraphs, starts = [], []
            del TABLES[:]
            indexes = read_indexes(doc)
            INDEX_NAMES.clear()
            INDEX_NAMES.update(x['name'] for x in indexes)
            PAGE_COLUMNS.clear()
            PAGE_COLUMNS.update(page_columns(doc))
            collect(doc.Text, styles, {}, paragraphs, starts)
            boxes = text_boxes(doc)
            collect_frames(boxes, styles, paragraphs, starts)
            tables = list(TABLES)
            pictures = read_pictures(doc, boxes)
            # the headings' and the title's always, to compare with the
            # starting file even where it does not use them yet
            styles_used = read_styles(doc, [p['style'] for p in paragraphs] +
                                      ['Title'] + [f'Heading {n}' for n in range(1, 4)],
                                      custom_docx_styles(source) if ext == 'docx' else None)
            page_parts = read_page_parts(doc, styles)
            footnotes = read_footnotes(doc)
            margins = page_margins(doc)
            columns = text_columns(doc)
            for i, (text, start, end) in enumerate(starts):
                for name, at in ((f'p{i}', start), (f'e{i}', end)):
                    mark = doc.createInstance('com.sun.star.text.Bookmark')
                    mark.Name = name
                    text.insertTextContent(text.createTextCursorByRange(at), mark, False)
            pdf = os.path.join(folder, 'out.pdf')
            filter_data = uno.Any('[]com.sun.star.beans.PropertyValue', tuple([
                prop('ExportBookmarksToPDFDestination', True), prop('ExportNotes', False),
                prop('ExportBookmarks', False)]))
            doc.storeToURL(uno.systemPathToFileUrl(pdf), (
                prop('FilterName', 'writer_pdf_Export'), prop('FilterData', filter_data)))
            update_indexes(indexes)
        finally:
            doc.close(True)
        dests = destinations(pdf)
        pages = words(pdf, folder)
        place(paragraphs, dests, pages, margins)
        place_pictures(pictures, pdf, folder)
        footer_boxes = place_page_parts(page_parts, pages)
        header_boxes = place_page_parts(page_parts, pages, 'header')
        # each page as SVG: vector, sharp at any zoom on the card; the
        # letters become paths, so the fonts need not be on the viewer's
        # machine (shown as <img>, nothing in it runs)
        out_pages = pdf_pages(pdf, folder, pages)
        return {'paragraphs': paragraphs, 'tables': tables, 'pictures': pictures, 'styles': styles_used,
                'indexes': indexes, 'page_parts': page_parts, 'footer_boxes': footer_boxes, 'header_boxes': header_boxes, 'footnotes': footnotes,
                'pages': out_pages, 'page_count': len(pages), 'margins': margins, 'columns': columns}
    finally:
        shutil.rmtree(folder, ignore_errors=True)


def run_job(data, filename, probes=None):
    """One job with a time limit: LibreOffice is killed and started again
    when a document takes too long or brings it down."""
    result = {}

    def work():
        try:
            result['value'] = analyze(data, filename, probes)
        except Exception as e:
            traceback.print_exc()
            result['value'] = {'error': 'failed', 'detail': f'{type(e).__name__}'}

    with LOCK:
        t = threading.Thread(target=work, daemon=True)
        t.start()
        t.join(JOB_SECONDS)
        if t.is_alive():
            OFFICE.stop()
            return {'error': 'timeout'}
        value = result.get('value', {'error': 'failed'})
        if value.get('error') == 'failed':
            OFFICE.stop()
        return value


class Handler(BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):
        pass

    def answer(self, status, body):
        raw = json.dumps(body).encode('utf-8')
        self.send_response(status)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', str(len(raw)))
        self.end_headers()
        self.wfile.write(raw)

    def do_GET(self):
        if self.path == '/health':
            self.answer(200, {'ok': True})
        else:
            self.answer(404, {'error': 'not_found'})

    def do_POST(self):
        if self.path != '/analyze':
            return self.answer(404, {'error': 'not_found'})
        length = int(self.headers.get('Content-Length') or 0)
        if length <= 0:
            return self.answer(400, {'error': 'empty'})
        if length > MAX_BYTES:
            return self.answer(413, {'error': 'too_big', 'max': MAX_BYTES})
        data = self.rfile.read(length)
        name = os.path.basename(self.headers.get('X-Filename') or 'upload.docx')
        # a spreadsheet's probes (sheet.py), as base64 JSON
        probes = None
        if self.headers.get('X-Probes'):
            try:
                probes = json.loads(base64.b64decode(self.headers.get('X-Probes')).decode('utf-8'))
            except Exception:
                return self.answer(400, {'error': 'probes'})
        started = time.time()
        result = run_job(data, name, probes)
        result['seconds'] = round(time.time() - started, 2)
        self.answer(200, result)


if __name__ == '__main__':
    OFFICE.start()
    print(f'office service on port {PORT}', flush=True)
    ThreadingHTTPServer(('0.0.0.0', PORT), Handler).serve_forever()
