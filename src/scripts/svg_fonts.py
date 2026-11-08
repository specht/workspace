#!/usr/bin/env python3
"""Embeds the fonts an SVG drawing uses, so that it looks the same in every
browser. A page shows a drawing as <img src='x.svg'>; such an SVG cannot use
the page's web fonts, and its text would come out in whatever font the
reader has. This puts the fonts into the SVG itself: for every font family
the drawing names that the site has (IBM Plex Sans, IBM Plex Mono, 0xProto),
in every weight and style it names, only the characters of its texts, as
WOFF2 in a data URI (WOFF where Python lacks brotli: sudo apt install
python3-brotli, then run again). A few kilobytes per drawing.

    python3 src/scripts/svg_fonts.py FILE.svg ...

Rewrites each file in place. The fonts stand in one <style id='embedded-fonts'>
right after the opening <svg> tag; a second run replaces it, so it can run
after every change (render-svg.rb runs it). Families the site does not have
(Arial, system-ui) stay as they are. Labels from diagram_labels.math are
paths already and need nothing.
"""

import base64
import io
import os
import re
import sys
import xml.etree.ElementTree as ET

from fontTools import subset
from fontTools.ttLib import TTFont

try:
    import brotli  # noqa: F401  (fontTools needs it for WOFF2)
    FLAVOR = 'woff2'
except ImportError:
    FLAVOR = 'woff'

FONTS = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'static', 'include', 'fonts')
WEIGHTS = {'100': 'Thin', '200': 'ExtraLight', '300': 'Light', '400': '', 'normal': '', '500': 'Medium',
           '600': 'SemiBold', '700': 'Bold', 'bold': 'Bold'}


def plex(folder, prefix):
    def file(weight, italic):
        name = WEIGHTS.get(weight, '') + ('Italic' if italic else '')
        return os.path.join(FONTS, folder, f'{prefix}-{name or "Regular"}.ttf')
    return file


def proto(weight, italic):
    name = 'Italic' if italic else ('Bold' if WEIGHTS.get(weight) in ('SemiBold', 'Bold') else 'Regular')
    return os.path.join(FONTS, '0xProtoNerdFontMono', f'0xProtoNerdFontMono-{name}.ttf')


FAMILIES = {
    'IBM Plex Sans': plex('IBM_Plex_Sans', 'IBMPlexSans'),
    'IBM Plex Mono': plex('IBM_Plex_Mono', 'IBMPlexMono'),
    '0xProto Nerd Font Mono': proto,
}
CSS_WEIGHT = {'normal': '400', 'bold': '700'}
BLOCK = re.compile(r"\n?\s*<style id=['\"]embedded-fonts['\"]>.*?</style>", re.S)


def mentioned(svg):
    """the families the site has, and the weights and styles named anywhere
    in the drawing (attributes, style attributes, a <style> block)"""
    families = set()
    for value in re.findall(r"font-family\s*[=:]\s*(\"[^\"]*\"|'[^']*'|[^;\"'>]+)", svg):
        for name in value.strip('"\'').split(','):
            name = name.strip().strip('"\'')
            if name in FAMILIES:
                families.add(name)
    weights = {'400'} | {CSS_WEIGHT.get(w, w) for w in re.findall(r"font-weight\s*[=:]\s*['\"]?([a-z0-9]+)", svg)}
    italic = bool(re.search(r"font-style\s*[=:]\s*['\"]?italic", svg))
    return families, weights, italic


def characters(svg):
    root = ET.fromstring(svg)
    text = ''
    for element in root.iter():
        if element.tag.split('}')[-1] == 'text':
            text += ''.join(element.itertext())
    return set(text) - {'\n', '\r', '\t'}


def woff(path, chars):
    options = subset.Options()
    options.flavor = FLAVOR
    options.hinting = False
    options.desubroutinize = True
    options.layout_features = ['kern', 'liga', 'calt']
    font = TTFont(path)
    subsetter = subset.Subsetter(options)
    subsetter.populate(text=''.join(chars))
    subsetter.subset(font)
    out = io.BytesIO()
    font.flavor = FLAVOR
    font.save(out)
    return base64.b64encode(out.getvalue()).decode('ascii')


def embed(path):
    svg = open(path, encoding='utf-8').read()
    svg = BLOCK.sub('', svg)
    families, weights, italic = mentioned(svg)
    chars = characters(svg) if families else set()
    rules = []
    for family in sorted(families):
        for weight in sorted(weights):
            for slant in ([False, True] if italic else [False]):
                font = FAMILIES[family](weight, slant)
                if not os.path.exists(font) or not chars:
                    continue
                rules.append(f"@font-face{{font-family:'{family}';font-weight:{weight};"
                             f"font-style:{'italic' if slant else 'normal'};"
                             f"src:url(data:font/{FLAVOR};base64,{woff(font, chars)}) format('{FLAVOR}')}}")
    if rules:
        # right after the opening tag, before anything that draws text
        svg = re.sub(r'(<svg\b[^>]*>)', lambda m: m.group(1) + "\n  <style id='embedded-fonts'>" + ''.join(rules) + '</style>',
                     svg, count=1)
    with open(path, 'w', encoding='utf-8') as f:
        f.write(svg)
    return len(rules), len(chars)


if __name__ == '__main__':
    for path in sys.argv[1:]:
        faces, count = embed(path)
        print(f'{path}: {faces} font faces, {count} characters' if faces else f'{path}: no fonts to embed')
