"""The words of a text the German dictionary does not know (the rule
spelling, docs/plan-textverarbeitung.md »Rechtschreibung«): LibreOffice's
spell checker with hunspell-de-de, the same a student sees underlined in
LibreOffice or Nextcloud Office. Every word counts as German, whatever
language the document sets, except the words set to no language
(»Keine«, zxx) in the file: LibreOffice keeps that per word in .odt and
.docx; Word writes »Rechtschreibung und Grammatik nicht prüfen« as
w:noProof, which LibreOffice drops on import, so those runs are read from
the .docx itself (no_proof_words). Called by server.py only for a task
that has the rule."""
import html
import re
import zipfile

import uno

# a word: letters, soft hyphens inside, joined by hyphens or apostrophes
# (»E-Mail«, »geht's«); digits and single letters are no words here
WORD = re.compile(r"(?:[^\W\d_]|\xad)+(?:[-'’](?:[^\W\d_]|\xad)+)*")
MAX_WORDS = 4000
SUGGESTIONS = 3


def no_proof_words(source):
    """the words of a .docx's runs that Word does not check: w:noProof, or
    the language zxx"""
    try:
        xml = zipfile.ZipFile(source).read('word/document.xml').decode('utf-8')
    except Exception:
        return set()
    words = set()
    for run in re.finditer(r'<w:r(?:\s[^>]*)?>(.*?)</w:r>', xml, re.S):
        body = run.group(1)
        props = re.search(r'<w:rPr>(.*?)</w:rPr>', body, re.S)
        if not props:
            continue
        p = props.group(1)
        off = re.search(r'<w:noProof(?:\s+w:val="(\w+)")?\s*/>', p)
        skip = (off and off.group(1) not in ('0', 'false', 'off')) or re.search(r'<w:lang\b[^>]*w:val="zxx"', p)
        if not skip:
            continue
        text = ''.join(html.unescape(t) for t in re.findall(r'<w:t(?:\s[^>]*)?>(.*?)</w:t>', body, re.S))
        words.update(m.group(0).replace('\xad', '') for m in WORD.finditer(text))
    return words


class Checker:
    def __init__(self, office):
        self.spell = office.smgr.createInstanceWithContext('com.sun.star.linguistic2.SpellChecker', office.ctx)
        # built field by field: a Locale passed whole does not convert
        self.locale = uno.createUnoStruct('com.sun.star.lang.Locale')
        self.locale.Language, self.locale.Country = 'de', 'DE'
        self.cache = {}

    def alternatives(self, word):
        """None for a known word, else up to three suggestions"""
        if word not in self.cache:
            found = self.spell.spell(word, self.locale, ())
            self.cache[word] = None if found is None else list(found.getAlternatives())[:SUGGESTIONS]
        return self.cache[word]

    def unknown(self, word):
        found = self.alternatives(word)
        if found is None:
            return None
        # »Kletterwald-Tag«: known when each of its parts is
        parts = [x for x in re.split(r"[-'’]", word) if x]
        if len(parts) > 1 and all(len(x) < 2 or self.alternatives(x) is None for x in parts):
            return None
        return found


def check(office, paragraphs, excluded=()):
    """each paragraph gets 'unknown': [[start, end, word, [suggestions]]]
    (offsets in its text); returns the words counted and those left out
    of the check (set to no language)"""
    checker = Checker(office)
    counted = unchecked = 0
    for p in paragraphs:
        text = p.get('text', '')
        portions = p.get('portions') or []
        # the ranges of portions set to no language, where the portions
        # make up the text (not where footnote numbers were left out)
        ranges, at = [], 0
        if ''.join(q.get('text', '') for q in portions) == text:
            for q in portions:
                n = len(q.get('text', ''))
                if q.get('unchecked'):
                    ranges.append((at, at + n))
                at += n
        found = []
        for m in WORD.finditer(text):
            word = m.group(0).replace('\xad', '')
            if len(word) < 2:
                continue
            counted += 1
            if counted > MAX_WORDS:
                break
            if word in excluded or any(a <= m.start() and m.end() <= b for a, b in ranges):
                unchecked += 1
                continue
            suggestions = checker.unknown(word)
            if suggestions is not None:
                found.append([m.start(), m.end(), word, suggestions])
        p['unknown'] = found
    return {'words': counted, 'unchecked': unchecked}
