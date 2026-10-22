// Rauschen: Zufall, der weich ist. Daraus Wolken über einem Himmel.

// eine Zufallszahl von 0 bis 1 für jeden Punkt eines Gitters, immer dieselbe
float hash(vec2 p)
{
    vec3 q = fract(vec3(p.xyx) * 0.1031);
    q += dot(q, q.yzx + 33.33);
    return fract((q.x + q.y) * q.z);
}

// Value Noise: zwischen den Gitterpunkten weich überblenden
float noise(vec2 p)
{
    vec2 i = floor(p);
    vec2 f = fract(p);
    vec2 u = f * f * (3.0 - 2.0 * f);   // weicher als geradlinig
    float a = hash(i);
    float b = hash(i + vec2(1.0, 0.0));
    float c = hash(i + vec2(0.0, 1.0));
    float d = hash(i + vec2(1.0, 1.0));
    return mix(mix(a, b, u.x), mix(c, d, u.x), u.y);
}

// fbm: Rauschen in mehreren Größen übereinander, jede Lage halb so groß und halb so stark
float fbm(vec2 p)
{
    float sum = 0.0;
    float amplitude = 0.5;
    for (int i = 0; i < 6; i++)
    {
        sum += amplitude * noise(p);
        p *= 2.0;
        amplitude *= 0.5;
    }
    return sum;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 p = (2.0 * fragCoord - iResolution.xy) / iResolution.y;

    // der Himmel: unten hell, oben tiefblau
    vec3 sky = mix(vec3(0.85, 0.9, 1.0), vec3(0.2, 0.4, 0.8), 0.5 + 0.5 * p.y);

    // die Wolken ziehen mit der Zeit nach links
    float clouds = fbm(p * 1.5 + vec2(iTime * 0.1, 0.0));
    clouds = smoothstep(0.45, 0.75, clouds);

    vec3 color = mix(sky, vec3(1.0), clouds);
    fragColor = vec4(color, 1.0);
}
