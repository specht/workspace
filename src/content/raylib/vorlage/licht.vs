#version 100

// Für jede Ecke eines Dreiecks: wo sie liegt, wohin ihre Fläche zeigt und
// welche Farbe sie hat. raylib liefert diese Werte.
attribute vec3 vertexPosition;
attribute vec3 vertexNormal;
attribute vec4 vertexColor;

// Kamera und Projektion zusammen: macht aus einem Punkt der Welt einen Punkt
// auf dem Bildschirm
uniform mat4 mvp;

// Weitergereicht an den Fragment-Shader, für jeden Pixel der Fläche
varying vec3 position;
varying vec3 normale;
varying vec4 farbe;

void main()
{
    position = vertexPosition;
    normale = vertexNormal;
    farbe = vertexColor;
    gl_Position = mvp * vec4(vertexPosition, 1.0);
}
