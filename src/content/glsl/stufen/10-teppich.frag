// Der Sierpinski-Teppich: ein Quadrat, aus dem die Mitte fehlt, und aus jedem
// der acht Quadrate drumherum wieder die Mitte, immer weiter

// 1 auf dem Teppich, 0 in einem Loch; p läuft im Quadrat von 0 bis 1
float carpet(vec2 p, int steps)
{
    for (int i = 0; i < 6; i++)
    {
        if (i >= steps) break;
        // in drei mal drei Kacheln teilen: welche Kachel, und wo darin?
        p *= 3.0;
        vec2 cell = floor(p);
        p = fract(p);
        // die mittlere Kachel ist ein Loch
        if (cell.x == 1.0 && cell.y == 1.0) return 0.0;
    }
    return 1.0;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 p = (2.0 * fragCoord - iResolution.xy) / iResolution.y;

    // jede Sekunde eine Stufe mehr: 1, 2, 3, 4, 5, dann von vorn
    int steps = 1 + int(mod(iTime, 5.0));

    // das Quadrat von -0.9 bis 0.9 auf 0 bis 1 bringen
    vec2 q = (p + 0.9) / 1.8;

    vec3 color = vec3(0.01, 0.012, 0.018);
    if (q.x > 0.0 && q.x < 1.0 && q.y > 0.0 && q.y < 1.0)
    {
        vec3 stone = vec3(0.45, 0.5, 0.55) * (0.85 + 0.15 * q.y);
        vec3 hole = vec3(0.02, 0.03, 0.04);
        color = mix(hole, stone, carpet(q, steps));
    }
    fragColor = vec4(pow(color, vec3(1.0 / 2.2)), 1.0);
}
