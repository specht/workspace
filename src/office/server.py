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
        ], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        local = uno.getComponentContext()
        resolver = local.ServiceManager.createInstanceWithContext('com.sun.star.bridge.UnoUrlResolver', local)
        for _ in range(100):
            try:
                context = resolver.resolve(f'uno:pipe,name={PIPE};urp;StarOffice.ComponentContext')
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
    else:
        return None
    item['bold'] = portion.CharWeight >= 150
    item['italic'] = enum_value(portion.CharPosture) not in ('NONE', None)
    item['underline'] = portion.CharUnderline != 0
    item['size'] = round(portion.CharHeight, 1)
    item['font'] = portion.CharFontName
    item['color'] = portion.CharColor
    item['caps'] = portion.CharCaseMap == 1
    item['direct'] = [n for n in CHAR_PROPS if is_direct(portion, n)]
    return item


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
    return {
        'text': text,
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
        'indent_left': pt(para.ParaLeftMargin),
        'indent_first': pt(para.ParaFirstLineIndent),
        'break': enum_value(para.BreakType),
        'numbering': numbering,
        'portions': portions,
        'where': where,
    }


def iter_enum(container):
    e = container.createEnumeration()
    while e.hasMoreElements():
        yield e.nextElement()


def collect(text, styles, where, out, starts):
    """Paragraphs of a text (the body or a table cell), in order."""
    for element in iter_enum(text):
        if element.supportsService('com.sun.star.text.Paragraph'):
            out.append(read_paragraph(element, styles, where))
            starts.append((element.getText(), element.getStart(), element.getEnd()))
        elif element.supportsService('com.sun.star.text.TextTable'):
            name = element.getName()
            for cell_name in element.getCellNames():
                cell = element.getCellByName(cell_name)
                collect(cell, styles, {'table': name, 'cell': cell_name}, out, starts)


def page_margins(doc):
    styles = doc.StyleFamilies.getByName('PageStyles')
    cursor = doc.Text.createTextCursor()
    name = cursor.PageStyleName
    page = styles.getByName(name)
    return {'left': pt(page.LeftMargin), 'right': pt(page.Width - page.RightMargin),
            'width': pt(page.Width), 'height': pt(page.Height)}


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
        line = size * 1.2
        mine = []
        for pg in range(page_no, end_page + 1):
            for w in pages[pg - 1]['words']:
                lo = top - 1 if pg == page_no else -1
                hi = end_top + line * 0.75 if pg == end_page else 1e9
                mid = (w[1] + w[3]) / 2
                if lo <= mid <= hi:
                    mine.append([pg] + list(w))
        # the next paragraph on the same page bounds an empty one
        following = next((t for t in starts[i + 1:] if t is not None), None)
        if mine:
            bottom = max(w[4] for w in mine if w[0] == end_page) if any(w[0] == end_page for w in mine) else end_top + line
        else:
            bottom = following[1] if following and following[0] == page_no and following[1] > top else top + line
        p['box'] = {'page': page_no, 'top': round(top, 1), 'bottom': round(bottom, 1),
                    'end_page': end_page, 'x': round(x, 1),
                    'left': margins['left'], 'right': margins['right']}
        p['words'] = [[w[0]] + [round(v, 1) for v in w[1:5]] + [w[5]] for w in mine]


def analyze(data, filename):
    ext = filename.rsplit('.', 1)[-1].lower() if '.' in filename else ''
    if ext not in FORMATS:
        return {'error': 'format', 'formats': sorted(FORMATS)}
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
        if doc is None or not doc.supportsService('com.sun.star.text.TextDocument'):
            if doc is not None:
                doc.close(True)
            return {'error': 'not_text'}
        try:
            styles = doc.StyleFamilies.getByName('ParagraphStyles')
            paragraphs, starts = [], []
            collect(doc.Text, styles, {}, paragraphs, starts)
            margins = page_margins(doc)
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
        finally:
            doc.close(True)
        dests = destinations(pdf)
        pages = words(pdf, folder)
        place(paragraphs, dests, pages, margins)
        # each page as SVG: vector, sharp at any zoom on the card; the
        # letters become paths, so the fonts need not be on the viewer's
        # machine (shown as <img>, nothing in it runs)
        out_pages = []
        for i in range(min(len(pages), MAX_PAGES)):
            target = os.path.join(folder, f'page-{i + 1}.svg')
            subprocess.run(['pdftocairo', '-svg', '-f', str(i + 1), '-l', str(i + 1), pdf, target],
                           capture_output=True, timeout=60)
            if not os.path.exists(target):
                break
            with open(target, 'rb') as f:
                svg = base64.b64encode(f.read()).decode('ascii')
            out_pages.append({'width': pages[i]['width'], 'height': pages[i]['height'], 'svg': svg})
        return {'paragraphs': paragraphs, 'pages': out_pages, 'page_count': len(pages), 'margins': margins}
    finally:
        shutil.rmtree(folder, ignore_errors=True)


def run_job(data, filename):
    """One job with a time limit: LibreOffice is killed and started again
    when a document takes too long or brings it down."""
    result = {}

    def work():
        try:
            result['value'] = analyze(data, filename)
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
        started = time.time()
        result = run_job(data, name)
        result['seconds'] = round(time.time() - started, 2)
        self.answer(200, result)


if __name__ == '__main__':
    OFFICE.start()
    print(f'office service on port {PORT}', flush=True)
    ThreadingHTTPServer(('0.0.0.0', PORT), Handler).serve_forever()
