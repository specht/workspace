# GLSL-Starter

Shader im Browser, wie auf [Shadertoy](https://www.shadertoy.com): `index.html`
zeigt den Fragment-Shader `shader.frag` im ganzen Fenster. Die Anleitung dazu
steht in der Hackschule unter »Shader in GLSL«.

- `shader.frag` schreibt `mainImage(out vec4 fragColor, in vec2 fragCoord)` und
  bekommt `iResolution` (Größe in Pixeln), `iTime` (Sekunden seit dem Start) und
  `iMouse` (die Maus in Pixeln, `z > 0`, solange sie gedrückt ist).
- Öffne `index.html` mit »Go Live«. Nach jedem Speichern lädt die Vorschau neu.
- `index.html?shader=wellen.frag` zeigt eine andere Datei, `index.html?t=2.5`
  hält die Zeit an, `index.html?scale=0.5` rechnet nur halb so viele Pixel in
  jeder Richtung.
- Oben rechts steht, wie viele Bilder pro Sekunde der Rechner schafft. Sind es
  weniger als 40, rechnet die Seite weniger Pixel, bis zur halben Breite und
  Höhe.
- Ein Fehler im Shader steht unten im Fenster, mit der Zeile in `shader.frag`.
