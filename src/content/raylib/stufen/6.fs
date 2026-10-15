#version 100
precision mediump float;

// Vom Vertex-Shader, für jeden Pixel einer Fläche
varying vec3 position;
varying vec3 normal;
varying vec4 color;

uniform vec3 sun;        // Richtung, aus der die Sonne scheint
uniform vec3 sunlight;   // Farbe und Stärke des Sonnenlichts
uniform vec3 ambient;    // Licht, das von überall kommt

void main()
{
    vec3 n = normalize(normal);

    // Wie direkt trifft die Sonne diese Fläche? 1 = senkrecht, 0 = gar nicht
    float sunAmount = max(dot(n, sun), 0.0);
    vec3 light = ambient + sunlight * sunAmount;

    gl_FragColor = vec4(color.rgb * light, 1.0);
}
