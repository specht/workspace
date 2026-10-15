#version 100
precision mediump float;

varying vec3 position;
varying vec3 normale;
varying vec4 farbe;
varying vec2 uv;

uniform sampler2D texture0;  // die Textur mit allen Kacheln

uniform vec3 sonne;         // Richtung, aus der die Sonne scheint
uniform vec3 sonnenlicht;   // Farbe und Stärke des Sonnenlichts
uniform vec3 umgebung;      // Licht, das von überall kommt
uniform vec3 himmel;        // Farbe des Himmels, für den Nebel
uniform vec3 augen;         // wo die Kamera ist

#define MAX_FACKELN 16
uniform vec3 fackeln[MAX_FACKELN];
uniform int anzahlFackeln;

void main()
{
    vec3 n = normalize(normale);

    // Wie direkt trifft die Sonne diese Fläche? 1 = senkrecht, 0 = gar nicht
    float sonnig = max(dot(n, sonne), 0.0);
    vec3 licht = umgebung + sonnenlicht * sonnig;

    // Jede Fackel leuchtet warm, je näher, desto heller
    for (int i = 0; i < MAX_FACKELN; i++) {
        if (i >= anzahlFackeln) break;
        vec3 zurFackel = fackeln[i] - position;
        float abstand = length(zurFackel);
        float zugewandt = max(dot(n, zurFackel / abstand), 0.0);
        licht += vec3(1.0, 0.6, 0.3) * (0.3 + 0.7 * zugewandt) / (1.0 + 0.3 * abstand * abstand);
    }

    vec3 pixel = texture2D(texture0, uv).rgb;
    vec3 ergebnis = pixel * farbe.rgb * min(licht, vec3(1.4));

    // Nebel: weit Entferntes verschwimmt mit dem Himmel
    float nebel = clamp((length(position - augen) - 16.0) / 20.0, 0.0, 1.0);
    gl_FragColor = vec4(mix(ergebnis, himmel, nebel), 1.0);
}
