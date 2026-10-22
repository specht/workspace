// Der Menger-Schwamm: ein Würfel, aus dem in jeder Stufe ein Kreuz aus drei Balken
// fehlt, und aus jedem der 20 Würfel drumherum wieder, immer kleiner

// Abstand von p zu einem Würfel mit der halben Kantenlänge 1 um den Nullpunkt
float box(vec3 p)
{
    vec3 q = abs(p) - 1.0;
    return length(max(q, 0.0)) + min(max(q.x, max(q.y, q.z)), 0.0);
}

// Abstand zu drei unendlich langen Balken entlang x, y und z durch den Nullpunkt,
// jeder w breit nach jeder Seite
float bars(vec3 p, float w)
{
    vec3 a = abs(p);
    return min(max(a.y, a.z), min(max(a.x, a.z), max(a.x, a.y))) - w;
}

// der Schwamm: der Würfel, weniger die Balken jeder Stufe
float sponge(vec3 p)
{
    float d = box(p);
    float size = 2.0;   // die Kantenlänge der Würfel dieser Stufe
    for (int i = 0; i < 4; i++)
    {
        // die Lage im Würfel dieser Stufe, seine Mitte bei 0: mod wiederholt ihn
        vec3 q = mod(p + size / 2.0, size) - size / 2.0;
        // durch seine Mitte ein Kreuz ein Drittel so breit wie er wegnehmen
        d = max(d, -bars(q, size / 6.0));
        size /= 3.0;
    }
    return d;
}

mat2 rotate(float a)
{
    return mat2(cos(a), sin(a), -sin(a), cos(a));
}

// der Schwamm dreht sich langsam und steht schräg
vec3 turn(vec3 p)
{
    p.xz = rotate(0.25 * iTime) * p.xz;
    p.yz = rotate(0.6) * p.yz;
    p.xy = rotate(0.5) * p.xy;
    return p;
}

float map(vec3 p)
{
    return sponge(turn(p));
}

vec3 normal(vec3 p)
{
    vec2 e = vec2(0.0005, 0.0);
    return normalize(vec3(map(p + e.xyy) - map(p - e.xyy),
                          map(p + e.yxy) - map(p - e.yxy),
                          map(p + e.yyx) - map(p - e.yyx)));
}

// Umgebungsverdeckung: tief in den Löchern kommt wenig Licht an
float occlusion(vec3 p, vec3 n)
{
    float hidden = 0.0;
    for (int i = 1; i <= 5; i++)
    {
        float h = 0.01 + 0.04 * float(i);
        hidden += (h - map(p + n * h)) / float(i);
    }
    return clamp(1.0 - 3.0 * hidden, 0.0, 1.0);
}

// weicher Schatten in Richtung l, höchstens bis zur Entfernung far
float shadow(vec3 p, vec3 l, float far)
{
    float light = 1.0;
    float t = 0.01;
    for (int i = 0; i < 32; i++)
    {
        float d = map(p + l * t);
        light = min(light, 16.0 * d / t);
        t += clamp(d, 0.01, 0.2);
        if (light < 0.001 || t > far) break;
    }
    return clamp(light, 0.0, 1.0);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 p = (2.0 * fragCoord - iResolution.xy) / iResolution.y;

    // von leicht unten: der Würfel ragt über der Kamera auf
    vec3 eye = vec3(0.0, -1.0, -3.9);
    vec3 forward = normalize(vec3(0.0, 0.05, 0.0) - eye);
    vec3 right = normalize(cross(vec3(0.0, 1.0, 0.0), forward));
    vec3 up = cross(forward, right);
    vec3 dir = normalize(p.x * right + p.y * up + 1.7 * forward);

    float t = 0.0;
    bool hit = false;
    float haze = 0.0;
    for (int i = 0; i < 110; i++)
    {
        float d = map(eye + dir * t);
        // Dunst: je knapper der Strahl am Würfel vorbeiläuft, desto mehr leuchtet die Luft
        haze += 0.004 / (1.0 + 80.0 * d * d);
        if (d < 0.0005 * t) { hit = true; break; }
        t += d;
        if (t > 8.0) break;
    }

    // fast schwarz, oben ein Hauch von kaltem Licht
    vec3 color = vec3(0.003, 0.004, 0.007) + vec3(0.006, 0.012, 0.02) * smoothstep(-0.2, 1.0, p.y);
    if (hit)
    {
        vec3 q = eye + dir * t;
        vec3 n = normal(q);
        float ao = occlusion(q, n);

        // dunkler Stein, ein kaltes Licht von oben, ein schwacher Rand
        vec3 stone = vec3(0.12, 0.13, 0.15);
        vec3 top = normalize(vec3(-0.3, 1.0, -0.2));
        float light = max(dot(n, top), 0.0) * shadow(q + n * 0.001, top, 3.0);
        color = stone * vec3(0.55, 0.75, 1.0) * 2.0 * light * ao;
        color += stone * vec3(0.05, 0.07, 0.1) * ao;
        float rim = pow(1.0 - max(dot(n, -dir), 0.0), 5.0);
        color += vec3(0.3, 0.55, 0.8) * rim * 0.25 * ao;

        // aus dem Kern dringt eisblaues Licht: je näher an der Mitte, desto heller
        vec3 inside = turn(q);
        float core = exp(-6.0 * dot(inside, inside));
        color += vec3(0.25, 0.8, 1.0) * 4.5 * core * (0.3 + 0.7 * max(dot(n, -normalize(inside)), 0.0));
    }
    color += vec3(0.2, 0.45, 0.6) * haze * 0.25;

    // zum Rand dunkler, und sanft begrenzen statt abschneiden
    color *= 1.0 - 0.4 * dot(p * 0.6, p * 0.6);
    color = 1.0 - exp(-color * 1.3);
    fragColor = vec4(pow(color, vec3(1.0 / 2.2)), 1.0);
}
