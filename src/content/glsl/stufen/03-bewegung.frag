// Kreise in Bewegung, mit Farben aus einer Palette.

// eine Farbe für jede Zahl t: drei Kosinuswellen, gegeneinander verschoben
vec3 palette(float t)
{
    return 0.5 + 0.5 * cos(6.28318 * (t + vec3(0.0, 0.33, 0.67)));
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 p = (2.0 * fragCoord - iResolution.xy) / iResolution.y;
    float pixel = 2.0 / iResolution.y;

    vec3 color = vec3(0.04, 0.05, 0.1);
    for (int i = 0; i < 8; i++)
    {
        float k = float(i);
        // jeder Kreis auf seiner eigenen Bahn
        float a = iTime * 0.5 + k * 0.785;
        vec2 center = 0.65 * vec2(cos(a), sin(1.3 * a));
        float d = length(p - center);
        vec3 c = palette(k / 8.0 + iTime * 0.05);
        // ein Schein um den Kreis, schwächer, je weiter weg
        color += c * 0.012 / d;
        // der Kreis selbst
        color = mix(color, c, 1.0 - smoothstep(0.15 - pixel, 0.15 + pixel, d));
    }
    fragColor = vec4(color, 1.0);
}
