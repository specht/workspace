// Dein Shader: mainImage rechnet für jeden Pixel seine Farbe aus.
// fragCoord ist die Position des Pixels, iResolution die Größe des Bildes,
// iTime die Zeit in Sekunden.

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    // uv läuft von 0 (links unten) bis 1 (rechts oben)
    vec2 uv = fragCoord / iResolution.xy;

    // Rot von links nach rechts, Grün von unten nach oben, Blau pulsiert
    vec3 color = vec3(uv.x, uv.y, 0.5 + 0.5 * sin(iTime));

    fragColor = vec4(color, 1.0);
}
