// Ein Kaleidoskop: Polarkoordinaten, gespiegelte Segmente und ein Muster
// aus Kacheln mit fract.

vec3 palette(float t)
{
    return 0.5 + 0.5 * cos(6.28318 * (t + vec3(0.0, 0.33, 0.67)));
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 p = (2.0 * fragCoord - iResolution.xy) / iResolution.y;

    // Polarkoordinaten: wie weit von der Mitte, in welchem Winkel
    float r = length(p);
    float a = atan(p.y, p.x);

    // in 8 Segmente teilen und jedes zweite spiegeln, wie im Kaleidoskop
    float segment = 6.28318 / 8.0;
    a = mod(a, segment);
    a = abs(a - segment / 2.0);
    vec2 q = r * vec2(cos(a), sin(a));

    // das Muster wandert langsam nach außen
    q.x -= iTime * 0.15;

    // Kacheln: fract nimmt nur den Nachkommateil, jede Kachel hat ihre Mitte bei 0
    vec2 cell = fract(q * 3.0) - 0.5;
    vec2 id = floor(q * 3.0);
    float d = length(cell);

    vec3 color = palette(0.1 * id.x + 0.3 * r + iTime * 0.05);
    // Ringe in jeder Kachel
    color *= 0.6 + 0.4 * cos(30.0 * d - iTime * 2.0);
    // nach außen dunkler
    color *= smoothstep(1.6, 0.2, r);
    fragColor = vec4(color, 1.0);
}
