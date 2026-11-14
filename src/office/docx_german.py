"""German as the language of a document python-docx writes, whose template
says en-US: set on »Normal«, which the other styles follow, as a German Word
or LibreOffice writes it. LibreOffice checks the spelling and hyphenates in
the language of the text: with the template's en-US it underlines every
German word and hyphenates with English patterns. Called right after
Document(), before the fonts, so that python-docx keeps the order of the
style's properties."""
from docx.oxml import OxmlElement
from docx.oxml.ns import qn


def german(doc):
    props = doc.styles['Normal'].element.get_or_add_rPr()
    for old in props.findall(qn('w:lang')):
        props.remove(old)
    lang = OxmlElement('w:lang')
    lang.set(qn('w:val'), 'de-DE')
    props.append(lang)
    return doc
