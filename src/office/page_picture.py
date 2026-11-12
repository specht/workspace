# A part of a document's page as a picture for an activity's page, for the
# scripts that make the documents of the office worksheets (dokumente.py,
# run in the office image: src/scripts/office). Not used by the service.
#
# LibreOffice makes a PDF; one page of it becomes an SVG (pdftocairo, the
# letters as outlines: sharp at every size, no fonts needed in the browser)
# cut to the rectangle asked for, on white paper. A target ending in .webp
# gets pixels instead (the card in the catalog needs them).

import os
import re
import shutil
import subprocess
import tempfile

PT_PER_CM = 72 / 2.54


def page_picture(docx, target, page=1, top_cm=0.0, height_cm=29.7, left_cm=0.0, width_cm=21.0, dpi=220):
    work = tempfile.mkdtemp()
    try:
        subprocess.run(['soffice', '--headless', f'-env:UserInstallation=file://{work}/profile',
                        '--convert-to', 'pdf', '--outdir', work, docx], check=True, capture_output=True)
        pdf = os.path.join(work, os.path.splitext(os.path.basename(docx))[0] + '.pdf')
        if target.endswith('.svg'):
            svg = os.path.join(work, 'page.svg')
            subprocess.run(['pdftocairo', '-svg', '-f', str(page), '-l', str(page), pdf, svg], check=True)
            with open(svg, encoding='utf-8') as f:
                text = f.read()
            x, y = left_cm * PT_PER_CM, top_cm * PT_PER_CM
            w, h = width_cm * PT_PER_CM, height_cm * PT_PER_CM
            box = f'{x:.2f} {y:.2f} {w:.2f} {h:.2f}'
            # the root: the cut as its view, its size in points
            text = re.sub(r'<svg\b[^>]*>', lambda m: re.sub(r'\s(width|height|viewBox)="[^"]*"', '', m.group(0))[:-1] +
                          f' width="{w:.2f}pt" height="{h:.2f}pt" viewBox="{box}">'
                          f'<rect x="{x:.2f}" y="{y:.2f}" width="{w:.2f}" height="{h:.2f}" fill="#ffffff"/>', text, count=1)
            with open(target, 'w', encoding='utf-8') as f:
                f.write(text)
        else:
            px = lambda cm: int(round(cm / 2.54 * dpi))
            subprocess.run(['pdftoppm', '-r', str(dpi), '-png', '-f', str(page), '-l', str(page), '-singlefile',
                            '-x', str(px(left_cm)), '-y', str(px(top_cm)), '-W', str(px(width_cm)), '-H', str(px(height_cm)),
                            pdf, os.path.join(work, 'page')], check=True)
            subprocess.run(['cwebp', '-quiet', '-q', '90', os.path.join(work, 'page.png'), '-o', target], check=True)
    finally:
        shutil.rmtree(work, ignore_errors=True)
