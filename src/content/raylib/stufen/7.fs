#version 100
precision mediump float;

varying vec3 position;
varying vec3 normal;
varying vec4 color;

uniform vec3 sun;        // Richtung, aus der die Sonne scheint
uniform vec3 sunlight;   // Farbe und Stärke des Sonnenlichts
uniform vec3 ambient;    // Licht, das von überall kommt
uniform vec3 sky;        // Farbe des Himmels, für den Nebel
uniform vec3 eyes;       // wo die Kamera ist

#define MAX_TORCHES 16
uniform vec3 torches[MAX_TORCHES];
uniform int torchCount;

void main()
{
    vec3 n = normalize(normal);

    // Wie direkt trifft die Sonne diese Fläche? 1 = senkrecht, 0 = gar nicht
    float sunAmount = max(dot(n, sun), 0.0);
    vec3 light = ambient + sunlight * sunAmount;

    // Jede Fackel leuchtet warm, je näher, desto heller
    for (int i = 0; i < MAX_TORCHES; i++) {
        if (i >= torchCount) break;
        vec3 toTorch = torches[i] - position;
        float dist = length(toTorch);
        float facing = max(dot(n, toTorch / dist), 0.0);
        light += vec3(1.0, 0.6, 0.3) * (0.3 + 0.7 * facing) / (1.0 + 0.3 * dist * dist);
    }

    vec3 result = color.rgb * min(light, vec3(1.4));

    // Nebel: weit Entferntes verschwimmt mit dem Himmel
    float fog = clamp((length(position - eyes) - 16.0) / 20.0, 0.0, 1.0);
    gl_FragColor = vec4(mix(result, sky, fog), 1.0);
}
