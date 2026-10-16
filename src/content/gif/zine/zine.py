#!/usr/bin/env python3
"""Sets the original GIF89a specification as an A5 zine (zine.tex).

Every page of the original typewritten document becomes one A5 page: its page
number stands alone, right-aligned, at the top of each page, so the text is
split there, and the page numbers of the original table of contents stay
right. The front matter (cover sheet, title page, table of contents) is split
at its long runs of blank lines. Nothing in the text is changed.

Build it with build.sh, which needs lualatex, the font Courier 10 Pitch
(Debian: xfonts-scalable) and pdfjam.
"""
import re
import sys

source, target = sys.argv[1], sys.argv[2]
lines = open(source, encoding='ascii').read().rstrip('\n').split('\n')

# Page number lines: only a number, right-aligned, after blank lines.
numbers = [i for i, l in enumerate(lines)
           if re.fullmatch(r' {60,}\d{1,2}', l) and all(not x.strip() for x in lines[max(0, i - 4):i])]

def trim(block):
    while block and not block[0].strip():
        block = block[1:]
    while block and not block[-1].strip():
        block = block[:-1]
    return block

# Front matter: split at runs of ten or more blank lines.
front, run, current = [], 0, []
for l in lines[:numbers[0]]:
    if not l.strip():
        run += 1
    else:
        if run >= 10 and trim(current):
            front.append(trim(current))
            current = []
        run = 0
    current.append(l)
if trim(current):
    front.append(trim(current))

# The page header stands above the table of contents, on the same page.
for i, block in enumerate(front[:-1]):
    if any('Document Date' in l for l in block) and len(block) <= 3:
        front[i:i + 2] = [block + [''] * 10 + front[i + 1]]
        break

body = []
for k, start in enumerate(numbers):
    end = numbers[k + 1] if k + 1 < len(numbers) else len(lines)
    body.append(trim(lines[start:end]))

LINES_PER_PAGE = 60      # at the normal leading
NORMAL_LEADING = 8.6     # pt
TEXT_HEIGHT = 508        # pt, a little under the 516 of the type area

def verbatim(block, centered=False):
    leading = min(NORMAL_LEADING, TEXT_HEIGHT / max(len(block), 1))
    out = []
    if centered:
        out.append('\\vspace*{\\fill}')
    out.append('\\begin{Verbatim}[fontsize=\\fontsize{7.3}{%.2f}\\selectfont]' % leading)
    out.extend(block)
    out.append('\\end{Verbatim}')
    if centered:
        out.append('\\vspace*{\\fill}')
    out.append('\\newpage')
    return '\n'.join(out)

pages = []
for i, block in enumerate(front):
    # The title page stands in the middle of its page, as on paper.
    pages.append(verbatim(block, centered=any('Version 89a' in l for l in block)))
pages.extend(verbatim(block) for block in body)

HEAD = r"""\documentclass{article}
\usepackage[a5paper, twoside, inner=14mm, outer=12mm, top=14mm, bottom=14mm]{geometry}
\usepackage{fontspec}
\usepackage{fancyvrb}
% A serif typewriter face, as the specification was typed in 1990.
\setmonofont{Courier 10 Pitch}
\renewcommand{\familydefault}{\ttdefault}
\pagestyle{empty}
\setlength{\parindent}{0pt}
\begin{document}
"""

COVER = r"""\vspace*{30mm}
\begin{center}
\fontsize{9}{11}\selectfont
\begin{Verbatim}[fontsize=\fontsize{11}{13}\selectfont]
+-----------------------------------------+
|                                         |
|   G R A P H I C S                       |
|   I N T E R C H A N G E                 |
|   F O R M A T                           |
|                                         |
|                         Version 89a     |
|                                         |
+-----------------------------------------+
\end{Verbatim}
\vspace{12mm}
Programming Reference\\[2mm]
CompuServe Incorporated\\[1mm]
31 July 1990
\end{center}
\vspace*{\fill}
\begin{center}
\fontsize{7.3}{9}\selectfont
Der Originaltext der Spezifikation, unver\"andert gesetzt.
\end{center}
\newpage
"""

COLOPHON = r"""\vspace*{\fill}
\begin{Verbatim}[fontsize=\fontsize{7.3}{8.6}\selectfont]
Quelle

  https://www.w3.org/Graphics/GIF/spec-gif89a.txt

  The Graphics Interchange Format(c) is the Copyright property of
  CompuServe Incorporated. GIF(sm) is a Service Mark property of
  CompuServe Incorporated.

Zu diesem Heft

  Der Text ist unveraendert. Jede Seite des Originals ist eine Seite
  dieses Hefts; die Seitenzahlen oben rechts stammen aus dem Original
  und passen zum Inhaltsverzeichnis. Gesetzt in Courier 10 Pitch.
\end{Verbatim}
\newpage
"""

BLANK = r"\null\newpage"

# Cover, the original pages, blank pages as needed, colophon on the last
# page: a booklet needs a multiple of four pages.
count = 1 + len(pages) + 1
blanks = (-count) % 4
with open(target, 'w') as out:
    out.write(HEAD)
    out.write(COVER)
    out.write('\n\n'.join(pages))
    out.write('\n' + '\n'.join([BLANK] * blanks) + '\n')
    out.write(COLOPHON)
    out.write('\\end{document}\n')
print(f'{count + blanks} pages in all ({blanks} blank)', file=sys.stderr)

print(f'{len(front)} front pages, {len(body)} numbered pages; longest {max(len(b) for b in front + body)} lines', file=sys.stderr)
