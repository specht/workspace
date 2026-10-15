#version 100

// Für jede Ecke eines Dreiecks: wo sie liegt, wohin ihre Fläche zeigt und
// welche Farbe sie hat. raylib liefert diese Werte.
attribute vec3 vertexPosition;
attribute vec3 vertexNormal;
attribute vec4 vertexColor;
attribute vec2 vertexTexCoord;

// Kamera und Projektion zusammen: macht aus einem Punkt der Welt einen Punkt
// auf dem Bildschirm
uniform mat4 mvp;

// Weitergereicht an den Fragment-Shader, für jeden Pixel der Fläche
varying vec3 position;
varying vec3 normal;
varying vec4 color;
varying vec2 uv;           // wo in der Textur dieser Punkt liegt

void main()
{
    position = vertexPosition;
    normal = vertexNormal;
    color = vertexColor;
    uv = vertexTexCoord;
    gl_Position = mvp * vec4(vertexPosition, 1.0);
}
