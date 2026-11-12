# Building spreadsheets for the worksheets on spreadsheets (the generators
# dokumente.py of each sheet; run in the office image, src/scripts/office):
# LibreOffice through UNO, the document in German (»12,50 €«, »02.03.«),
# saved as .xlsx, the format Excel and LibreOffice both read; and pictures
# of a sheet for the page, drawn as the check draws it (sheet.py), so the
# page and the card look alike.
#
#     with Calc() as calc:
#         doc = calc.new()
#         put(doc, 'A1', [['Posten', 'Preis'], ['Bus', 1200]])
#         calc.save(doc, path)
#         calc.picture(path, 'bilder/kosten.svg')

import base64
import os
import shutil
import subprocess
import tempfile
import time

import uno
from com.sun.star.beans import PropertyValue
from com.sun.star.lang import Locale

PIPE = 'calc_build'
GERMAN = Locale('de', 'DE', '')
# com.sun.star.util.NumberFormat
CURRENCY, PERCENT, DATE = 8, 128, 2


def prop(name, value):
    p = PropertyValue()
    p.Name, p.Value = name, value
    return p


class Calc:
    """LibreOffice for the time of a with block"""

    def __enter__(self):
        self.profile = tempfile.mkdtemp(prefix='calc-')
        self.process = subprocess.Popen(['soffice', '--headless', '--invisible', '--norestore', '--nologo', '--nolockcheck',
                                         f'-env:UserInstallation=file://{self.profile}', f'--accept=pipe,name={PIPE};urp;'],
                                        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        local = uno.getComponentContext()
        resolver = local.ServiceManager.createInstanceWithContext('com.sun.star.bridge.UnoUrlResolver', local)
        for _ in range(150):
            try:
                self.ctx = resolver.resolve(f'uno:pipe,name={PIPE};urp;StarOffice.ComponentContext')
                self.smgr = self.ctx.ServiceManager
                self.desktop = self.smgr.createInstanceWithContext('com.sun.star.frame.Desktop', self.ctx)
                return self
            except Exception:
                time.sleep(0.2)
        raise RuntimeError('LibreOffice did not start')

    def __exit__(self, *_):
        self.process.kill()
        self.process.wait()
        subprocess.run(['pkill', '-f', f'pipe,name={PIPE}'], capture_output=True)
        shutil.rmtree(self.profile, ignore_errors=True)

    def new(self):
        doc = self.desktop.loadComponentFromURL('private:factory/scalc', '_blank', 0, (prop('Hidden', True),))
        # German: numbers, dates and currency as a German program shows them
        styles = doc.StyleFamilies.getByName('CellStyles')
        default = styles.getByName('Default')
        default.CharLocale = GERMAN
        default.CharFontName = 'Calibri'
        default.CharHeight = 11
        return doc

    def open(self, path):
        return self.desktop.loadComponentFromURL(uno.systemPathToFileUrl(os.path.abspath(path)), '_blank', 0,
                                                 (prop('Hidden', True),))

    def save(self, doc, path):
        doc.calculateAll()
        doc.storeToURL(uno.systemPathToFileUrl(os.path.abspath(path)), (prop('FilterName', 'Calc MS Excel 2007 XML'),))
        doc.close(True)

    def picture(self, path, target, sheet=0):
        """the sheet as the check draws it (sheet.py), as SVG for a page"""
        import sheet as reader
        doc = self.open(path)
        try:
            work = tempfile.mkdtemp()
            model = reader.analyze(self, doc, work, [])
            shutil.rmtree(work, ignore_errors=True)
        finally:
            doc.close(True)
        with open(target, 'wb') as f:
            f.write(base64.b64decode(model['pages'][sheet]['svg']))
        return model


def sheet_of(doc, index=0):
    return doc.Sheets.getByIndex(index)


def put(doc, start, rows, sheet=0):
    """rows of values from a cell on: numbers as numbers, text as text,
    »=…« as a formula"""
    sh = sheet_of(doc, sheet)
    cell = sh.getCellRangeByName(start).getCellAddress()
    for r, row in enumerate(rows):
        for c, value in enumerate(row):
            target = sh.getCellByPosition(cell.Column + c, cell.Row + r)
            if value is None:
                continue
            if isinstance(value, str) and value.startswith('='):
                target.setFormula(value)
            elif isinstance(value, str):
                target.setString(value)
            else:
                target.setValue(float(value))


def number_format(doc, code):
    """the key of a German number format (»#.##0,00 €«, »0 %«, »TT.MM.«)"""
    formats = doc.NumberFormats
    key = formats.queryKey(code, GERMAN, False)
    return key if key != -1 else formats.addNew(code, GERMAN)


def fmt(doc, ranges, code, sheet=0):
    sh = sheet_of(doc, sheet)
    key = number_format(doc, code)
    for r in ranges.split(','):
        sh.getCellRangeByName(r.strip()).NumberFormat = key


def bold(doc, ranges, sheet=0):
    sh = sheet_of(doc, sheet)
    for r in ranges.split(','):
        sh.getCellRangeByName(r.strip()).CharWeight = 150


def widths(doc, cm, sheet=0):
    """column widths in cm, from column A on"""
    cols = sheet_of(doc, sheet).Columns
    for i, w in enumerate(cm):
        cols.getByIndex(i).Width = int(w * 1000)
