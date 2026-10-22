// Ein Kreis: Jeder Pixel fragt, wie weit er von der Mitte weg ist.

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    // p: die Mitte ist (0, 0), oben ist 1, unten -1; nach links und rechts
    // so weit, wie das Bild breit ist, damit ein Kreis rund bleibt
    vec2 p = (2.0 * fragCoord - iResolution.xy) / iResolution.y;

    // so groß ist ein Pixel in diesen Koordinaten
    float pixel = 2.0 / iResolution.y;

    // der Abstand zur Mitte
    float d = length(p);

    // 1 im Kreis, 0 draußen, dazwischen ein weicher Rand über zwei Pixel
    float inside = 1.0 - smoothstep(0.5 - pixel, 0.5 + pixel, d);

    vec3 background = vec3(0.08, 0.1, 0.18) + 0.08 * p.y;
    vec3 disk = vec3(1.0, 0.6, 0.2);
    fragColor = vec4(mix(background, disk, inside), 1.0);
}
