# A presentation for the office service (server.py, docs/plan-praesentationen.md):
# each slide with its layout, its background and its shapes: placeholders
# (title, content, subtitle) and what is not one (a text box, a picture, a
# chart, a table), each with its place, its text paragraph by paragraph with
# the sizes, fonts and colours of its letters and whether they were set by
# hand, how far its text runs past it, a picture's pixels, crop and alt
# text; the slide's notes, its transition and its animations; the masters'
# styles. Each slide is drawn by LibreOffice (PDF, then SVG per page), and
# the marks of the check stand on the shapes.

import base64
import os
import subprocess

from com.sun.star.beans.PropertyState import DIRECT_VALUE

MAX_SLIDES = 20
# the colour of letters set to »automatic«: dark on a light background,
# light on a dark one (both programs do so)
AUTO = -1


def prop(name, value):
    from com.sun.star.beans import PropertyValue
    p = PropertyValue()
    p.Name, p.Value = name, value
    return p


def pt(v):
    """1/100 mm to pt"""
    return round(v / 100 / 25.4 * 72, 2)


def enum_value(v):
    return getattr(v, 'value', v)


def hexcolour(c):
    return None if c is None or c < 0 else '#%06x' % (c & 0xFFFFFF)


def direct(obj, name):
    try:
        return obj.getPropertyState(name) == DIRECT_VALUE
    except Exception:
        return False


def get(obj, name, default=None):
    try:
        return obj.getPropertyValue(name)
    except Exception:
        return default


KINDS = {
    'com.sun.star.presentation.TitleTextShape': 'title',
    'com.sun.star.presentation.SubtitleShape': 'subtitle',
    'com.sun.star.presentation.OutlinerShape': 'body',
    'com.sun.star.presentation.GraphicObjectShape': 'picture',
    'com.sun.star.drawing.GraphicObjectShape': 'picture',
    'com.sun.star.presentation.ChartShape': 'chart',
    'com.sun.star.presentation.TableShape': 'table',
    'com.sun.star.drawing.TableShape': 'table',
    'com.sun.star.drawing.TextShape': 'text',
    'com.sun.star.presentation.NotesShape': 'notes',
    'com.sun.star.presentation.PageShape': 'page',
    'com.sun.star.presentation.SlideNumberShape': 'number',
    'com.sun.star.presentation.FooterShape': 'footer',
    'com.sun.star.presentation.DateTimeShape': 'date',
    'com.sun.star.drawing.GroupShape': 'group',
}
CHART_CLSID = '12dcae26-281f-416f-a234-c3086127382e'


def shape_kind(shape):
    kind = KINDS.get(shape.ShapeType)
    if kind:
        return kind
    if shape.ShapeType.endswith('OLE2Shape'):
        return 'chart' if (get(shape, 'CLSID') or '').lower() == CHART_CLSID else 'object'
    return 'shape'


def background(obj):
    """a slide's or a master's own background: a colour, a picture or a
    gradient, or None when it has none of its own"""
    bg = get(obj, 'Background')
    if bg is None:
        return None
    style = enum_value(get(bg, 'FillStyle'))
    if style == 'SOLID':
        return {'colour': hexcolour(get(bg, 'FillColor'))}
    if style == 'BITMAP':
        return {'picture': True}
    if style == 'GRADIENT':
        g = get(bg, 'FillGradient')
        return {'gradient': [hexcolour(g.StartColor), hexcolour(g.EndColor)]} if g else {'gradient': []}
    if style == 'NONE':
        return None
    return {'other': style}


def read_paragraphs(shape):
    """the text of a shape, paragraph by paragraph, with the size, font,
    colour and weight of each run of letters and whether it was set by hand
    (on the slide, not by the master or the layout)"""
    out = []
    try:
        paragraphs = shape.Text.createEnumeration()
    except Exception:
        return out
    while paragraphs.hasMoreElements():
        para = paragraphs.nextElement()
        runs = []
        portions = para.createEnumeration()
        while portions.hasMoreElements():
            portion = portions.nextElement()
            text = portion.getString()
            if not text.strip():
                continue
            runs.append({
                'text': text,
                'size': round(get(portion, 'CharHeight', 0), 1),
                'font': get(portion, 'CharFontName'),
                'colour': hexcolour(get(portion, 'CharColor', AUTO)),
                'bold': get(portion, 'CharWeight', 100) > 100,
                'direct': [n for n, p in (('size', 'CharHeight'), ('font', 'CharFontName'), ('colour', 'CharColor'))
                           if direct(portion, p)],
            })
        out.append({'text': para.getString(), 'level': get(para, 'NumberingLevel', 0) or 0,
                    'bullet': bool(get(para, 'NumberingIsNumber', False)) or None, 'runs': runs})
    return out


def overflow(shape, kind):
    """how far (pt) a shape's text runs past its bottom: the shape is let
    grow to its text in memory, measured, and put back. A shape that shrinks
    its text to fit (PowerPoint's »Größe des Texts bei Überlauf anpassen«)
    does not run over; its letters are smaller instead (font_scale)."""
    if kind not in ('title', 'subtitle', 'body', 'text') or not shape.getString().strip():
        return 0.0
    fit = enum_value(get(shape, 'TextFitToSize'))
    if fit and fit != 'NONE':
        return 0.0
    if get(shape, 'TextAutoGrowHeight'):
        return 0.0
    before = shape.Size
    try:
        shape.TextAutoGrowHeight = True
        grown = shape.Size.Height
        shape.TextAutoGrowHeight = False
        shape.Size = before
    except Exception:
        return 0.0
    return max(0.0, pt(grown - before.Height))


def read_picture(shape):
    out = {}
    graphic = get(shape, 'Graphic')
    if graphic is not None:
        try:
            px = graphic.SizePixel
            out['pixels'] = [px.Width, px.Height]
        except Exception:
            pass
        try:
            size = graphic.Size100thMM
            if size.Width > 0 and size.Height > 0:
                out['size_hmm'] = [size.Width, size.Height]
        except Exception:
            pass
    crop = get(shape, 'GraphicCrop')
    if crop is not None:
        out['crop_hmm'] = [crop.Left, crop.Top, crop.Right, crop.Bottom]
    return out


def walk_animations(node, out, depth=0):
    """the effects of a slide's timeline: each with its kind (entrance,
    emphasis, exit, motion path), its preset (ooo-entrance-fly-in …) and the
    shape it moves"""
    if node is None or depth > 12:
        return
    data = {}
    try:
        for p in node.UserData or ():
            data[p.Name] = p.Value
    except Exception:
        pass
    if 'preset-id' in data:
        target = None
        try:
            t = node.Target
            target = t if hasattr(t, 'ShapeType') else getattr(t, 'Shape', None)
        except Exception:
            pass
        out.append({'preset': data.get('preset-id'), 'class': data.get('preset-class'),
                    'node_type': data.get('node-type'), '_target': target})
        return
    try:
        children = node.createEnumeration()
    except Exception:
        return
    while children.hasMoreElements():
        walk_animations(children.nextElement(), out, depth + 1)


def master_styles(doc, master):
    """the master's text styles a slide's placeholders follow"""
    out = {}
    try:
        family = doc.StyleFamilies.getByName(master.Name)
    except Exception:
        return out
    for key, name in (('title', 'title'), ('body', 'outline1'), ('subtitle', 'subtitle')):
        try:
            s = family.getByName(name)
        except Exception:
            continue
        out[key] = {'font': get(s, 'CharFontName'), 'size': round(get(s, 'CharHeight', 0), 1),
                    'colour': hexcolour(get(s, 'CharColor', AUTO)), 'bold': get(s, 'CharWeight', 100) > 100}
    return out


def read_shape(shape, index):
    kind = shape_kind(shape)
    pos, size = shape.Position, shape.Size
    item = {'i': index, 'kind': kind, 'name': get(shape, 'Name') or '',
            'x': pt(pos.X), 'y': pt(pos.Y), 'w': pt(size.Width), 'h': pt(size.Height),
            'placeholder': bool(get(shape, 'IsPresentationObject', False)),
            'empty': bool(get(shape, 'IsEmptyPresentationObject', False)),
            'alt': (get(shape, 'Description') or '').strip(), 'alt_title': (get(shape, 'Title') or '').strip(),
            'rotation': (get(shape, 'RotateAngle', 0) or 0) / 100}
    if kind in ('title', 'subtitle', 'body', 'text', 'shape', 'notes'):
        item['text'] = shape.getString() if hasattr(shape, 'getString') else ''
        item['paragraphs'] = read_paragraphs(shape)
        scale = get(shape, 'TextFitToSizeFontScale')
        fit = enum_value(get(shape, 'TextFitToSize'))
        if fit and fit != 'NONE' and scale:
            item['font_scale'] = round(scale / 100.0, 3) if scale > 1 else round(scale, 3)
        item['overflow'] = overflow(shape, kind)
        if enum_value(get(shape, 'FillStyle')) == 'SOLID':
            item['fill'] = hexcolour(get(shape, 'FillColor'))
    if kind == 'picture':
        item.update(read_picture(shape))
    if kind == 'group':
        item['count'] = shape.getCount()
    return item


def footer_settings(slide):
    return {'number': bool(get(slide, 'IsPageNumberVisible', False)),
            'footer': bool(get(slide, 'IsFooterVisible', False)), 'footer_text': get(slide, 'FooterText') or '',
            'date': bool(get(slide, 'IsDateTimeVisible', False))}


def number_fields(slide):
    """a slide number as a field in a shape of the slide (how a .pptx from
    PowerPoint brings it)"""
    for i in range(slide.getCount()):
        shape = slide.getByIndex(i)
        try:
            fields = shape.Text.createEnumeration()
        except Exception:
            continue
        while fields.hasMoreElements():
            portions = fields.nextElement().createEnumeration()
            while portions.hasMoreElements():
                p = portions.nextElement()
                if get(p, 'TextPortionType') == 'TextField':
                    f = get(p, 'TextField')
                    if f is not None and f.supportsService('com.sun.star.text.textfield.PageNumber'):
                        return True
    return False


def read_slide(doc, slide, index):
    shapes = [read_shape(slide.getByIndex(i), i) for i in range(slide.getCount())]
    animations = []
    try:
        walk_animations(slide.AnimationNode, animations)
    except Exception:
        pass
    for a in animations:
        target = a.pop('_target')
        a['shape'] = next((i for i in range(slide.getCount()) if target is not None and slide.getByIndex(i) == target), None)
    notes = ''
    try:
        page = slide.NotesPage
        for i in range(page.getCount()):
            s = page.getByIndex(i)
            if s.ShapeType == 'com.sun.star.presentation.NotesShape':
                notes += s.getString()
    except Exception:
        pass
    master = slide.MasterPage
    transition = get(slide, 'TransitionType', 0) or 0
    effect = enum_value(get(slide, 'Effect'))
    item = {'index': index, 'layout': get(slide, 'Layout'), 'master': master.Name,
            'hidden': not get(slide, 'Visible', True), 'shapes': shapes, 'notes': notes.strip(),
            'transition': {'type': transition, 'subtype': get(slide, 'TransitionSubtype', 0),
                           'duration': get(slide, 'TransitionDuration')} if transition else
                          ({'effect': effect} if effect and effect != 'NONE' else None),
            'animations': animations, 'background': background(slide) or background(master),
            'footer': footer_settings(slide)}
    item['footer']['number_field'] = number_fields(slide)
    return item


def analyze(office, doc, folder):
    pages = doc.DrawPages
    first = pages.getByIndex(0)
    width, height = pt(first.Width), pt(first.Height)
    slides = [read_slide(doc, pages.getByIndex(i), i) for i in range(min(pages.getCount(), MAX_SLIDES))]
    masters = []
    for i in range(doc.MasterPages.getCount()):
        m = doc.MasterPages.getByIndex(i)
        masters.append({'name': m.Name, 'styles': master_styles(doc, m), 'background': background(m)})
    pdf = os.path.join(folder, 'out.pdf')
    filter_data = uno_props([prop('ExportHiddenSlides', True), prop('ExportNotesPages', False),
                             prop('ExportBookmarks', False), prop('UseTransitionEffects', False)])
    doc.storeToURL(office_url(pdf), (prop('FilterName', 'impress_pdf_Export'), prop('FilterData', filter_data)))
    out_pages = []
    for i in range(len(slides)):
        target = os.path.join(folder, f'slide-{i + 1}.svg')
        subprocess.run(['pdftocairo', '-svg', '-f', str(i + 1), '-l', str(i + 1), pdf, target],
                       capture_output=True, timeout=60)
        if not os.path.exists(target):
            break
        with open(target, 'rb') as f:
            out_pages.append({'width': width, 'height': height,
                              'svg': base64.b64encode(f.read()).decode('ascii')})
    return {'kind': 'slides', 'width': width, 'height': height, 'slides': slides, 'masters': masters,
            'slide_count': pages.getCount(), 'pages': out_pages, 'page_count': len(out_pages), 'paragraphs': []}


def uno_props(items):
    import uno
    return uno.Any('[]com.sun.star.beans.PropertyValue', tuple(items))


def office_url(path):
    import uno
    return uno.systemPathToFileUrl(path)
