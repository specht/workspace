#!/usr/bin/env python3
# Draws src/content/reference/raylib/axes.svg in perspective: a camera in
# front of the scene, to the right and above, looks at the origin. Then
# src/scripts/render-svg.rb src/content/reference/raylib/axes.svg 1200
# renders the light and the dark picture.
import math
def sub(a,b): return [a[i]-b[i] for i in range(3)]
def dot(a,b): return sum(a[i]*b[i] for i in range(3))
def cross(a,b): return [a[1]*b[2]-a[2]*b[1], a[2]*b[0]-a[0]*b[2], a[0]*b[1]-a[1]*b[0]]
def norm(a): l=math.sqrt(dot(a,a)); return [v/l for v in a]
C=[3.6,4.0,9.2]; T=[0.4,0.9,0.0]
f=norm(sub(T,C)); r=norm(cross(f,[0,1,0])); u=cross(r,f)
def cam(P): d=sub(P,C); return (dot(d,r), dot(d,u), dot(d,f))
def proj(P): x,y,z=cam(P); return (x/z, -y/z)
N=3; LX, LY, LZ = 3.6, 3.2, 3.8
pts=[]
grid=[]
for k in range(-N,N+1):
    grid.append(((k,0,-N),(k,0,N))); grid.append(((-N,0,k),(N,0,k)))
axes={'x':(LX,0,0),'y':(0,LY,0),'z':(0,0,LZ)}
for a,b in grid: pts+= [proj(a),proj(b)]
for v in axes.values(): pts.append(proj(v))
minx=min(p[0] for p in pts); maxx=max(p[0] for p in pts); miny=min(p[1] for p in pts); maxy=max(p[1] for p in pts)
W,H=600,380; margin_l, margin_r, margin_t, margin_b = 30, 120, 40, 30
s=min((W-margin_l-margin_r)/(maxx-minx),(H-margin_t-margin_b)/(maxy-miny))
ox=margin_l-minx*s+((W-margin_l-margin_r)-(maxx-minx)*s)/2; oy=margin_t-miny*s
def S(P): x,y=proj(P); return (ox+x*s, oy+y*s)
O=S((0,0,0))
out=[]
for a,b in grid:
    p,q=S(a),S(b); out.append(f'  <line class="grid" x1="{p[0]:.1f}" y1="{p[1]:.1f}" x2="{q[0]:.1f}" y2="{q[1]:.1f}"/>')
def arrow(name, v, head=22, half=9.5):
    tip=S(v); d=(tip[0]-O[0], tip[1]-O[1]); L=math.hypot(*d); d=(d[0]/L,d[1]/L); n=(-d[1],d[0])
    base=(tip[0]-d[0]*head, tip[1]-d[1]*head)
    a=(base[0]+n[0]*half, base[1]+n[1]*half); b=(base[0]-n[0]*half, base[1]-n[1]*half)
    return (f'  <line class="{name}" x1="{O[0]:.1f}" y1="{O[1]:.1f}" x2="{base[0]+d[0]*2:.1f}" y2="{base[1]+d[1]*2:.1f}"/>\n'
            f'  <polygon class="{name}-head" points="{tip[0]:.1f},{tip[1]:.1f} {a[0]:.1f},{a[1]:.1f} {b[0]:.1f},{b[1]:.1f}"/>'), tip, d
ax,xt,xd=arrow('x',axes['x']); ay,yt,yd=arrow('y',axes['y']); az,zt,zd=arrow('z',axes['z'])
svg=f'''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}" font-family="IBM Plex Sans, DejaVu Sans, sans-serif">
  <!-- Die drei Achsen im Raum, wie raylib und der Anaglyph-Editor sie
       verwenden: x nach rechts, y nach oben, z nach vorne, zum Betrachter.
       Das Gitter liegt flach in x und z, wie DrawGrid und das Gitter im
       Editor. In Perspektive, von vorne rechts oben gesehen; gezeichnet von
       src/scripts/draw-axes.py. Als Bild, hell und dunkel:
       src/scripts/render-svg.rb src/content/reference/raylib/axes.svg 1200 -->
  <style>
    .grid {{ stroke: #d1d5db; stroke-width: 1.5; }}
    .x, .y, .z {{ stroke-width: 5; stroke-linecap: round; }}
    .x {{ stroke: #dc2626; }} .x-head {{ fill: #dc2626; }}
    .y {{ stroke: #16a34a; }} .y-head {{ fill: #16a34a; }}
    .z {{ stroke: #2b5fb4; }} .z-head {{ fill: #2b5fb4; }}
    .letter, .way {{ paint-order: stroke; stroke: #ffffff; stroke-width: 7px; stroke-linejoin: round; }}
    .letter {{ font-size: 30px; font-weight: bold; font-style: italic; }}
    .way {{ font-size: 19px; fill: #6b7280; }}
    .origin {{ fill: #1f2937; }}
  </style>
  <rect width="{W}" height="{H}" fill="#ffffff"/>

  <!-- der Boden: ein Gitter in der Ebene von x und z -->
{chr(10).join(out)}

  <!-- die Achsen vom Ursprung aus; jede Pfeilspitze liegt auf ihrer Achse -->
{ax}
{ay}
{az}
  <circle class="origin" cx="{O[0]:.1f}" cy="{O[1]:.1f}" r="5"/>

  <text class="letter" x="{xt[0]+12:.1f}" y="{xt[1]+10:.1f}" fill="#dc2626">x</text>
  <text class="way" x="{xt[0]+12:.1f}" y="{xt[1]+34:.1f}">nach rechts</text>
  <text class="letter" x="{yt[0]+14:.1f}" y="{yt[1]+14:.1f}" fill="#16a34a">y</text>
  <text class="way" x="{yt[0]+14:.1f}" y="{yt[1]+38:.1f}">nach oben</text>
  <text class="letter" x="{zt[0]+zd[0]*14-10:.1f}" y="{zt[1]+zd[1]*14+22:.1f}" fill="#2b5fb4">z</text>
  <text class="way" x="{zt[0]+zd[0]*14+14:.1f}" y="{zt[1]+zd[1]*14+22:.1f}">nach vorne, zu dir</text>
</svg>
'''
open('src/content/reference/raylib/axes.svg','w').write(svg)
