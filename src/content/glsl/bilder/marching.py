import math, sys
# Raymarching von der Seite: eine Kugel über dem Boden, ein Strahl vom Auge.
S = 100.0          # Pixel je Einheit
W, H = 960, 420
GROUND = 370       # y des Bodens in Pixeln
def X(x): return 40 + x * S
def Y(y): return GROUND - y * S
cx, cy, r = 7.3, 1.55, 1.05
eye = (1.0, 0.95)
target = (6.55, 2.42)
dx, dy = target[0] - eye[0], target[1] - eye[1]
l = math.hypot(dx, dy); dx /= l; dy /= l
def sdf(x, y): return min(math.hypot(x - cx, y - cy) - r, y)
pts = []
t = 0.0
for i in range(30):
    x, y = eye[0] + dx * t, eye[1] + dy * t
    d = sdf(x, y)
    pts.append((x, y, d))
    if d < 0.03: break
    t += d
ink, blue, grey, red, pale, sep = '#1f2937', '#2b5fb4', '#6b7280', '#dc2626', '#eef3fb', '#d1d5db'
o = []
o.append(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}" font-family="IBM Plex Sans, DejaVu Sans, sans-serif">')
o.append('  <!-- Raymarching von der Seite: Um jeden Punkt des Strahls passt ein Kreis, der nichts berührt; sein Radius ist der Abstand zur nächsten Fläche (map). So weit springt der Strahl, bis der Abstand fast 0 ist.\n       Gemacht von bilder/marching.py; als Bild, hell und dunkel: src/scripts/render-svg.rb src/content/glsl/marching.svg -->')
o.append(f'  <rect width="{W}" height="{H}" fill="#ffffff"/>')
o.append(f'  <rect x="0" y="{GROUND}" width="{W}" height="{H-GROUND}" fill="{pale}"/>')
o.append(f'  <line x1="0" y1="{GROUND}" x2="{W}" y2="{GROUND}" stroke="{ink}" stroke-width="2"/>')
o.append(f'  <text x="{W-20}" y="{GROUND+34}" font-size="20" fill="{ink}" text-anchor="end">Boden</text>')
o.append(f'  <circle cx="{X(cx):.1f}" cy="{Y(cy):.1f}" r="{r*S:.1f}" fill="{pale}" stroke="{ink}" stroke-width="2"/>')
o.append(f'  <text x="{X(cx):.1f}" y="{Y(cy)+7:.1f}" font-size="20" fill="{ink}" text-anchor="middle">Kugel</text>')
# circles
for (x, y, d) in pts[:-1]:
    o.append(f'  <circle cx="{X(x):.1f}" cy="{Y(y):.1f}" r="{d*S:.1f}" fill="none" stroke="{grey}" stroke-width="1.5" stroke-dasharray="5 5"/>')
# ray
hx, hy, _ = pts[-1]
o.append(f'  <line x1="{X(eye[0]):.1f}" y1="{Y(eye[1]):.1f}" x2="{X(hx):.1f}" y2="{Y(hy):.1f}" stroke="{blue}" stroke-width="2.5"/>')
# first circle radius: from the first point straight down to the floor
x0, y0, d0 = pts[0]
o.append(f'  <line x1="{X(x0):.1f}" y1="{Y(y0):.1f}" x2="{X(x0):.1f}" y2="{Y(y0-d0):.1f}" stroke="{grey}" stroke-width="1.5"/>')
o.append(f'  <text x="{X(x0)+10:.1f}" y="{Y(y0-d0/2)+7:.1f}" font-size="20" fill="{grey}" font-family="0xProto Nerd Font Mono, DejaVu Sans Mono, monospace">d</text>')
for (x, y, d) in pts[:-1]:
    o.append(f'  <circle cx="{X(x):.1f}" cy="{Y(y):.1f}" r="5" fill="{blue}"/>')
o.append(f'  <circle cx="{X(hx):.1f}" cy="{Y(hy):.1f}" r="6" fill="{red}"/>')
o.append(f'  <text x="{X(eye[0]):.1f}" y="{Y(eye[1])-16:.1f}" font-size="20" fill="{ink}" text-anchor="middle">Auge</text>')
o.append(f'  <text x="{X(hx)+12:.1f}" y="{Y(hy)-14:.1f}" font-size="20" fill="{red}">Treffer</text>')
o.append('</svg>')
open(sys.argv[1], 'w').write('\n'.join(o) + '\n')
print(len(pts), 'points', [round(p[2], 3) for p in pts])
