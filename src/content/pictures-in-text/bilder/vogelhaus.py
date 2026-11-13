# Two pictures inside the students' documents of the extras of »Bilder im
# Text« (not on the page itself), in the style of karte.py: a bird house the
# 7b built in the project week, and the plan of the school garden with the
# five places where the houses hang. Writes vogelhaus.svg and
# schulgarten.svg and, with rsvg-convert, their PNGs (1200 × 800, 3:2),
# which dokumente.py puts into the documents. Run in the drawings image:
#
#     src/scripts/drawings python3 src/content/pictures-in-text/bilder/vogelhaus.py
#
# The bird house's entrance hole is a circle: in a picture stretched by a
# tenth it is an oval, the one place where the eye can tell (Zehn Fallen).

import os
import subprocess

HERE = os.path.dirname(os.path.abspath(__file__))
INK = '#2f3b2a'


def house():
    return f'''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1200 800" width="1200" height="800" font-family="PT Sans, sans-serif">
  <!-- made by vogelhaus.py -->
  <rect width="1200" height="800" fill="#e8f0f4"/>
  <!-- the garden behind: a meadow, two bushes -->
  <path d="M0 610c200-40 420-30 600-10s420 20 600-14V800H0z" fill="#c4dba6"/>
  <g fill="#a9c98a" stroke="#8fb071" stroke-width="3">
    <path d="M60 640c-10-70 40-120 100-118 40-50 120-40 140 14 60-4 90 50 70 104z"/>
    <path d="M880 630c-6-60 40-104 96-100 34-46 112-40 132 10 60 0 90 50 70 96z"/>
  </g>
  <!-- the post -->
  <rect x="578" y="520" width="44" height="280" fill="#8a6a43" stroke="#5e472b" stroke-width="4"/>
  <!-- the house: walls, roof, entrance hole, perch -->
  <path d="M440 300L600 170 760 300V560H440z" fill="#6fa6a0" stroke="#3f6f6a" stroke-width="6" stroke-linejoin="round"/>
  <g stroke="#3f6f6a" stroke-width="3" opacity="0.45">
    <path d="M440 380h320M440 460h320"/>
  </g>
  <path d="M410 318L600 150 790 318" fill="none" stroke="#8f3b2e" stroke-width="34" stroke-linecap="round" stroke-linejoin="round"/>
  <circle cx="600" cy="360" r="46" fill="#2a2a2a" stroke="#3f6f6a" stroke-width="6"/>
  <rect x="580" y="440" width="40" height="12" rx="6" fill="#8a6a43" stroke="#5e472b" stroke-width="3"/>
  <rect x="430" y="556" width="340" height="24" rx="4" fill="#8a6a43" stroke="#5e472b" stroke-width="4"/>
  <!-- a blue tit on the roof -->
  <g transform="translate(720 196)">
    <ellipse cx="0" cy="0" rx="34" ry="26" fill="#f2d24b" stroke="#6b5a1e" stroke-width="3"/>
    <path d="M-30-6c-20-6-34 0-44 10 16 2 30 0 44-4z" fill="#4f7fb8" stroke="#2f4f78" stroke-width="3"/>
    <circle cx="26" cy="-22" r="18" fill="#ffffff" stroke="#2f4f78" stroke-width="3"/>
    <path d="M12-36c8-8 22-8 30 2-10-2-20 0-30-2z" fill="#4f7fb8"/>
    <circle cx="32" cy="-24" r="3.5" fill="#1f1f1f"/>
    <path d="M43-20l10 3-10 3z" fill="#3a3a3a"/>
    <path d="M-6 24v12M8 24v12" stroke="#3a3a3a" stroke-width="3"/>
  </g>
</svg>
'''


def garden():
    # clockwise from the apple tree
    trees = [(180, 200, 'Apfelbaum'), (620, 120, 'Eiche'), (1010, 170, 'Kastanie'), (980, 520, 'Linde'), (240, 560, 'Birke')]
    tree_svg = ''
    houses = ''
    labels = ''
    for n, (x, y, name) in enumerate(trees, 1):
        tree_svg += f'<circle cx="{x}" cy="{y}" r="70" fill="#a9c98a" stroke="#7fa362" stroke-width="4"/>'
        houses += (f'<g transform="translate({x + 34} {y - 30})"><path d="M-22 18v-26l22-18 22 18v26z" fill="#6fa6a0" stroke="#3f6f6a" '
                   f'stroke-width="3" stroke-linejoin="round"/><text y="13" text-anchor="middle" font-size="22" font-weight="bold" fill="#ffffff">{n}</text></g>')
        labels += f'<text x="{x}" y="{y + 100}" text-anchor="middle">{name}</text>'
    return f'''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1200 800" width="1200" height="800" font-family="PT Sans, sans-serif">
  <!-- made by vogelhaus.py -->
  <rect width="1200" height="800" fill="#f1f3e6"/>
  <!-- the school at the bottom, the paths -->
  <rect x="300" y="700" width="600" height="100" fill="#d9c7a8" stroke="#9c8660" stroke-width="4"/>
  <text x="600" y="765" text-anchor="middle" font-size="32" font-weight="bold" fill="{INK}">Schule</text>
  <path d="M600 700V380M600 380H200M600 380H1000" stroke="#d8cdb4" stroke-width="40" fill="none" stroke-linecap="round"/>
  <!-- raised beds and the pond -->
  <g fill="#b98f5f" stroke="#7a5a32" stroke-width="4">
    <rect x="360" y="220" width="190" height="90" rx="6"/><rect x="660" y="220" width="190" height="90" rx="6"/>
  </g>
  <g stroke="#7fa362" stroke-width="5" stroke-linecap="round">
    <path d="M385 245h140M385 265h140M385 285h140M685 245h140M685 265h140M685 285h140"/>
  </g>
  <ellipse cx="760" cy="560" rx="110" ry="70" fill="#a8cfe8" stroke="#5f93bd" stroke-width="4"/>
  <g fill="{INK}" font-size="26">
    <text x="455" y="345" text-anchor="middle">Hochbeet</text>
    <text x="755" y="345" text-anchor="middle">Hochbeet</text>
    <text x="760" y="568" text-anchor="middle" fill="#2f6690" font-style="italic">Teich</text>
  </g>
  {tree_svg}
  <g fill="{INK}" font-size="24">{labels}</g>
  {houses}
  <!-- legend -->
  <g transform="translate(1000 700)">
    <path d="M-22 18v-26l22-18 22 18v26z" fill="#6fa6a0" stroke="#3f6f6a" stroke-width="3" stroke-linejoin="round"/>
    <text x="34" y="12" font-size="24" fill="{INK}">Vogelhaus</text>
  </g>
</svg>
'''


def main():
    for name, svg in (('vogelhaus', house()), ('schulgarten', garden())):
        path = os.path.join(HERE, f'{name}.svg')
        with open(path, 'w') as f:
            f.write(svg)
        subprocess.run(['rsvg-convert', '-w', '1200', '-h', '800', path, '-o', os.path.join(HERE, f'{name}.png')], check=True)


if __name__ == '__main__':
    main()
