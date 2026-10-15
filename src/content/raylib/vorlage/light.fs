#version 100
precision mediump float;

// Vom Vertex-Shader, für jeden Pixel einer Fläche
varying vec3 position;
varying vec3 normale;
varying vec4 farbe;

uniform vec3 sonne;         // Richtung, aus der die Sonne scheint
uniform vec3 sonnenlicht;   // Farbe und Stärke des Sonnenlichts
uniform vec3 umgebung;      // Licht, das von überall kommt

void main()
{
    vec3 n = normalize(normale);

    // Wie direkt trifft die Sonne diese Fläche? 1 = senkrecht, 0 = gar nicht
    float sonnig = max(dot(n, sonne), 0.0);
    vec3 licht = umgebung + sonnenlicht * sonnig;

    gl_FragColor = vec4(farbe.rgb * licht, 1.0);
}
