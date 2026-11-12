# A spreadsheet for the office service (server.py, docs/plan-tabellenkalkulation.md):
# what is in its cells (value, formula, the text as shown, the number
# format, bold, alignment, colour), the sheet's columns and rows, filters,
# conditional formats and charts; each sheet drawn as an SVG grid with
# column letters and row numbers, charts where they stand, so that the card
# can mark cells as it marks the lines of a text document.
#
# probes: [{'sheet': 0, 'set': {'B3': 10}, 'read': ['D3', 'D11']}]: each
# changes cells, recalculates and reads others, then puts the cells back
# (the check »does it calculate«, as the checks of code run it).

import base64
import os
import re
import traceback
from xml.sax.saxutils import escape

MAX_ROWS = 60
MAX_COLS = 16
MAX_SHEETS = 3
HEADER_W = 28.0    # the row numbers' column, pt
HEADER_H = 16.0    # the column letters' row, pt
FONT = "'Liberation Sans', Arial, sans-serif"


def prop(name, value):
    from com.sun.star.beans import PropertyValue
    p = PropertyValue()
    p.Name, p.Value = name, value
    return p


def pt(v):
    """1/100 mm to pt"""
    return round(v / 100 / 25.4 * 72, 2)


def column_name(c):
    name = ''
    c += 1
    while c:
        c, r = divmod(c - 1, 26)
        name = chr(65 + r) + name
    return name


def address(c, r):
    return f'{column_name(c)}{r + 1}'


CELL = re.compile(r'^\$?([A-Z]{1,3})\$?(\d+)$')


def parse_address(text):
    m = CELL.match(text.strip().upper())
    if not m:
        raise ValueError(text)
    c = 0
    for ch in m.group(1):
        c = c * 26 + ord(ch) - 64
    return c - 1, int(m.group(2)) - 1


def enum_value(v):
    return getattr(v, 'value', v)


def number_format(doc, key):
    try:
        f = doc.NumberFormats.getByKey(key)
        return {'type': int(f.Type), 'format': f.FormatString, 'currency': getattr(f, 'CurrencySymbol', '') or ''}
    except Exception:
        return {'type': 0, 'format': '', 'currency': ''}


def read_cell(doc, cell, c, r):
    kind = enum_value(cell.Type)
    item = {'cell': address(c, r), 'col': c, 'row': r, 'type': kind.lower()}
    if kind == 'EMPTY':
        return item
    item['shown'] = cell.String
    if kind == 'FORMULA':
        item['formula'] = cell.Formula
        item['error'] = int(cell.getError())
        try:
            # 1 value, 2 text, 4 error (com.sun.star.sheet.FormulaResult)
            item['result'] = {1: 'value', 2: 'text', 4: 'error'}.get(int(cell.FormulaResultType2), 'value')
        except Exception:
            item['result'] = 'value'
        item['value'] = cell.Value
    elif kind == 'VALUE':
        item['value'] = cell.Value
    else:
        item['text'] = cell.String
    item['number_format'] = number_format(doc, cell.NumberFormat)
    item['bold'] = cell.CharWeight >= 150
    item['italic'] = enum_value(cell.CharPosture) not in ('NONE', None)
    item['size'] = round(cell.CharHeight, 1)
    item['align'] = str(enum_value(cell.HoriJustify)).lower()
    item['background'] = cell.CellBackColor
    item['color'] = cell.CharColor
    return item


def used_area(sheet):
    cursor = sheet.createCursor()
    cursor.gotoEndOfUsedArea(False)
    a = cursor.getRangeAddress()
    return a.EndColumn, a.EndRow


def range_text(a):
    return f'{address(a.StartColumn, a.StartRow)}:{address(a.EndColumn, a.EndRow)}'


def conditional_formats(sheet):
    out = []
    try:
        formats = sheet.ConditionalFormats.getConditionalFormats()
        for f in formats:
            entries = []
            for i in range(f.getCount()):
                e = f.getByIndex(i)
                entry = {'type': int(getattr(e, 'Type', 0))}
                for name in ('Operator', 'Formula1', 'Formula2', 'StyleName'):
                    try:
                        value = getattr(e, name)
                        entry[name.lower()] = value if isinstance(value, str) else int(value) if name == 'Operator' else str(value)
                    except Exception:
                        pass
                entries.append(entry)
            ranges = [range_text(a) for a in f.Range.getRangeAddresses()]
            out.append({'ranges': ranges, 'entries': entries})
    except Exception:
        traceback.print_exc()
    return out


def autofilter(doc, index):
    """whether the sheet has an autofilter, and how many of its fields filter"""
    found = {'on': False, 'fields': 0}
    try:
        unnamed = doc.UnnamedDatabaseRanges
        if unnamed.hasByTable(index):
            db = unnamed.getByTable(index)
            if db.AutoFilter:
                found['on'] = True
                found['fields'] = len(db.getFilterDescriptor().getFilterFields2())
    except Exception:
        pass
    try:
        ranges = doc.DatabaseRanges
        for name in ranges.getElementNames():
            db = ranges.getByName(name)
            if db.DataArea.Sheet == index and db.AutoFilter:
                found['on'] = True
                found['fields'] = max(found['fields'], len(db.getFilterDescriptor().getFilterFields2()))
    except Exception:
        pass
    return found


CHART_TYPES = {'com.sun.star.chart.BarDiagram': 'column', 'com.sun.star.chart.LineDiagram': 'line',
               'com.sun.star.chart.PieDiagram': 'pie', 'com.sun.star.chart.DonutDiagram': 'pie',
               'com.sun.star.chart.XYDiagram': 'scatter', 'com.sun.star.chart.AreaDiagram': 'area'}


def charts(office, sheet, folder, sheet_index):
    out = []
    shapes = {}
    try:
        page = sheet.DrawPage
        for i in range(page.getCount()):
            shape = page.getByIndex(i)
            try:
                shapes[shape.PersistName] = shape
            except Exception:
                pass
    except Exception:
        pass
    try:
        names = sheet.Charts.getElementNames()
    except Exception:
        names = []
    for n, name in enumerate(names):
        try:
            chart = sheet.Charts.getByName(name)
            emb = chart.EmbeddedObject
            dia = emb.Diagram
            kind = CHART_TYPES.get(dia.DiagramType, dia.DiagramType)
            if kind == 'column' and getattr(dia, 'Vertical', False):
                kind = 'bar'      # horizontal bars
            item = {'name': name, 'type': kind,
                    'title': emb.Title.String if emb.HasMainTitle else '',
                    'legend': bool(emb.HasLegend),
                    'ranges': [range_text(a) for a in chart.Ranges]}
            for axis in ('X', 'Y'):
                try:
                    has = getattr(dia, f'Has{axis}AxisTitle')
                    item[f'{axis.lower()}_title'] = getattr(dia, f'{axis}AxisTitle').String if has else ''
                except Exception:
                    item[f'{axis.lower()}_title'] = ''
            shape = shapes.get(name)
            if shape is not None:
                p, s = shape.Position, shape.Size
                item['box'] = {'x': pt(p.X), 'y': pt(p.Y), 'w': pt(s.Width), 'h': pt(s.Height)}
                # as PNG at three times its size (LibreOffice exports a
                # chart's shape as an empty SVG)
                target = os.path.join(folder, f'chart-{sheet_index}-{n}.png')
                try:
                    import uno
                    export = office.smgr.createInstanceWithContext('com.sun.star.drawing.GraphicExportFilter', office.ctx)
                    export.setSourceDocument(shape)
                    data = uno.Any('[]com.sun.star.beans.PropertyValue', (
                        prop('PixelWidth', int(item['box']['w'] * 3)), prop('PixelHeight', int(item['box']['h'] * 3))))
                    export.filter((prop('URL', uno.systemPathToFileUrl(target)), prop('MediaType', 'image/png'), prop('FilterData', data)))
                    with open(target, 'rb') as f:
                        item['png'] = base64.b64encode(f.read()).decode('ascii')
                except Exception:
                    traceback.print_exc()
            out.append(item)
        except Exception:
            traceback.print_exc()
    return out


def run_probes(doc, probes):
    """set cells, recalculate, read others; the cells are put back after each"""
    results = []
    for probe in probes or []:
        sheet = doc.Sheets.getByIndex(int(probe.get('sheet', 0)))
        saved = []
        out = {'values': {}, 'shown': {}}
        try:
            for where, value in (probe.get('set') or {}).items():
                c, r = parse_address(where)
                cell = sheet.getCellByPosition(c, r)
                saved.append((cell, cell.Formula))
                if isinstance(value, (int, float)):
                    cell.setValue(float(value))
                elif isinstance(value, str) and value.startswith('='):
                    cell.setFormula(value)
                else:
                    cell.setString(str(value))
            doc.calculateAll()
            for where in probe.get('read') or []:
                c, r = parse_address(where)
                cell = sheet.getCellByPosition(c, r)
                out['values'][where] = None if cell.getError() else cell.Value
                out['shown'][where] = cell.String
        except Exception as e:
            out['error'] = type(e).__name__
        finally:
            for cell, formula in reversed(saved):
                cell.setFormula(formula)
            doc.calculateAll()
        results.append(out)
    return results


def draw(sheet_model, chart_list):
    """the sheet as a picture: grid, letters and numbers, the cells as shown"""
    cols, rows = sheet_model['cols'], sheet_model['rows']
    width = HEADER_W + sum(c['w'] for c in cols)
    height = HEADER_H + sum(r['h'] for r in rows if r['visible'])
    # charts may stand beyond the cells drawn
    for ch in chart_list:
        if 'box' in ch:
            width = max(width, HEADER_W + ch['box']['x'] + ch['box']['w'] + 6)
            height = max(height, HEADER_H + ch['box']['y'] + ch['box']['h'] + 6)
    out = [f"<svg xmlns='http://www.w3.org/2000/svg' width='{width:.1f}pt' height='{height:.1f}pt' viewBox='0 0 {width:.1f} {height:.1f}'>",
           f"<rect width='{width:.1f}' height='{height:.1f}' fill='#ffffff'/>",
           f"<rect width='{width:.1f}' height='{HEADER_H}' fill='#f1f3f5'/><rect width='{HEADER_W}' height='{height:.1f}' fill='#f1f3f5'/>"]
    xs = {c['col']: HEADER_W + c['x'] for c in cols}
    ws = {c['col']: c['w'] for c in cols}
    ys = {}
    y = HEADER_H
    for r in rows:
        if r['visible']:
            ys[r['row']] = y
            y += r['h']
    cells = {(c['col'], c['row']): c for c in sheet_model['cells']}
    # backgrounds first
    for (c, r), cell in cells.items():
        bg = cell.get('background', -1)
        if r in ys and c in xs and bg is not None and bg >= 0:
            out.append(f"<rect x='{xs[c]:.1f}' y='{ys[r]:.1f}' width='{ws[c]:.1f}' height='{next(x['h'] for x in rows if x['row'] == r):.1f}' fill='#{bg:06x}'/>")
    # the grid
    for c in cols:
        x = HEADER_W + c['x']
        out.append(f"<line x1='{x:.1f}' y1='0' x2='{x:.1f}' y2='{height:.1f}' stroke='#d0d5db' stroke-width='0.5'/>")
        out.append(f"<text x='{x + c['w'] / 2:.1f}' y='{HEADER_H - 4.5:.1f}' font-family=\"{FONT}\" font-size='8' fill='#555' text-anchor='middle'>{column_name(c['col'])}</text>")
    for r in rows:
        if not r['visible']:
            continue
        y = ys[r['row']]
        out.append(f"<line x1='0' y1='{y:.1f}' x2='{width:.1f}' y2='{y:.1f}' stroke='#d0d5db' stroke-width='0.5'/>")
        out.append(f"<text x='{HEADER_W / 2:.1f}' y='{y + r['h'] - 4:.1f}' font-family=\"{FONT}\" font-size='8' fill='#555' text-anchor='middle'>{r['row'] + 1}</text>")
    # the cells as shown: numbers right, text left, unless aligned otherwise;
    # a number too wide for its column shows ###, text runs on into an empty
    # neighbour, as in the program
    for (c, r), cell in sorted(cells.items(), key=lambda kv: (kv[0][1], kv[0][0])):
        if r not in ys or c not in xs or not cell.get('shown'):
            continue
        h = next(x['h'] for x in rows if x['row'] == r)
        size = min(cell.get('size', 10), h * 0.85)
        number = cell['type'] in ('value',) or (cell['type'] == 'formula' and cell.get('result') == 'value')
        align = cell.get('align', 'standard')
        if align == 'standard':
            align = 'right' if number else 'left'
        text = cell['shown']
        room = ws[c] - 4
        if number and len(text) * size * 0.55 > room:
            text = '#' * max(1, int(room / (size * 0.55)))
        elif not number:
            # how far the text may run on: through empty cells to the right
            k = c + 1
            while k in xs and (k, r) not in cells and len(text) * size * 0.5 > room:
                room += ws[k]
                k += 1
            limit = max(1, int(room / (size * 0.5)))
            if len(text) > limit:
                text = text[:limit - 1] + '…'
        weight = '700' if cell.get('bold') else '400'
        style = ' font-style=\'italic\'' if cell.get('italic') else ''
        colour = cell.get('color', -1)
        fill = '#c00000' if cell.get('error') else (f'#{colour:06x}' if colour is not None and colour >= 0 else '#1b1f24')
        if align == 'right':
            x, anchor = xs[c] + ws[c] - 2.5, 'end'
        elif align == 'center':
            x, anchor = xs[c] + ws[c] / 2, 'middle'
        else:
            x, anchor = xs[c] + 2.5, 'start'
        out.append(f"<text x='{x:.1f}' y='{ys[r] + h - (h - size) / 2 - size * 0.18:.1f}' font-family=\"{FONT}\" font-size='{size:.1f}' "
                   f"font-weight='{weight}'{style} fill='{fill}' text-anchor='{anchor}'>{escape(text)}</text>")
    for ch in chart_list:
        if 'box' in ch and ch.get('png'):
            b = ch['box']
            out.append(f"<image x='{HEADER_W + b['x']:.1f}' y='{HEADER_H + b['y']:.1f}' width='{b['w']:.1f}' height='{b['h']:.1f}' "
                       f"href='data:image/png;base64,{ch['png']}'/>")
    out.append('</svg>')
    return '\n'.join(out), width, height


def analyze(office, doc, folder, probes=None):
    sheets, pages = [], []
    for index in range(min(doc.Sheets.getCount(), MAX_SHEETS)):
        sheet = doc.Sheets.getByIndex(index)
        end_col, end_row = used_area(sheet)
        chart_list = charts(office, sheet, folder, index)
        ncols, nrows = min(end_col + 1, MAX_COLS), min(end_row + 1, MAX_ROWS)
        cols, x = [], 0.0
        for c in range(ncols):
            col = sheet.Columns.getByIndex(c)
            w = pt(col.Width) if col.IsVisible else 0.0
            cols.append({'col': c, 'x': round(x, 2), 'w': w})
            x += w
        rows = []
        y = 0.0
        for r in range(nrows):
            row = sheet.Rows.getByIndex(r)
            h = pt(row.Height)
            rows.append({'row': r, 'y': round(y, 2), 'h': h, 'visible': bool(row.IsVisible)})
            if row.IsVisible:
                y += h
        cells = []
        for r in range(nrows):
            for c in range(ncols):
                item = read_cell(doc, sheet.getCellByPosition(c, r), c, r)
                if item['type'] != 'empty':
                    cells.append(item)
        model = {'name': sheet.Name, 'cols': cols, 'rows': rows, 'cells': cells,
                 'used': address(end_col, end_row), 'conditional': conditional_formats(sheet),
                 'autofilter': autofilter(doc, index), 'charts': chart_list,
                 'header_w': HEADER_W, 'header_h': HEADER_H}
        svg, width, height = draw(model, chart_list)
        for ch in chart_list:
            ch.pop('png', None)
        sheets.append(model)
        pages.append({'width': width, 'height': height, 'svg': base64.b64encode(svg.encode('utf-8')).decode('ascii')})
    return {'kind': 'sheet', 'sheets': sheets, 'pages': pages, 'page_count': len(pages), 'paragraphs': [],
            'probes': run_probes(doc, probes)}
