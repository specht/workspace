// Im Menger-Schwamm: ohne den äußeren Würfel füllt der Schwamm den ganzen Raum.
// Aus den Löchern werden Gänge, wo sie sich kreuzen, Räume

// Abstand zu drei unendlich langen Balken entlang x, y und z durch den Nullpunkt,
// jeder w breit nach jeder Seite
float bars(vec3 p, float w)
{
    vec3 a = abs(p);
    return min(max(a.y, a.z), min(max(a.x, a.z), max(a.x, a.y))) - w;
}

// der Schwamm ohne Würfel: überall Stein, weniger die Balken jeder Stufe
float sponge(vec3 p)
{
    float d = -100.0;
    float size = 6.0;   // die Kantenlänge der Würfel dieser Stufe: ein Raum alle 6 Einheiten
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

float map(vec3 p)
{
    return sponge(p);
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
        float h = 0.04 + 0.12 * float(i);
        hidden += (h - map(p + n * h)) / float(i);
    }
    return clamp(1.0 - 1.5 * hidden, 0.0, 1.0);
}

// eine Zufallszahl von 0 bis 1 für jeden Punkt, immer dieselbe
float hash(vec3 p)
{
    p = fract(p * 0.1031);
    p += dot(p, p.zyx + 31.32);
    return fract((p.x + p.y) * p.z);
}

// das Licht eines Raums mit der Mitte c auf einer Fläche bei q mit der Normale n:
// nur in manchen Räumen brennt eines, kalt, ganz selten warm
vec3 room(vec3 c, vec3 q, vec3 n)
{
    float r = hash(c * 0.37 + 11.0);
    if (r < 0.7) return vec3(0.0);
    vec3 tint = r < 0.95 ? vec3(0.55, 0.85, 1.0) : vec3(1.0, 0.6, 0.25);
    vec3 to = c - q;
    float d2 = dot(to, to);
    return tint * 4.0 * max(dot(n, to / sqrt(d2)), 0.0) / (1.0 + 0.6 * d2 * d2 / 9.0);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 p = (2.0 * fragCoord - iResolution.xy) / iResolution.y;

    // die Kamera ist gekippt, um gut 15 Grad, und schwankt langsam
    float roll = 0.27 + 0.05 * sin(iTime * 0.21);
    p = rotate(roll) * p;

    // langsam durch einen Gang, von Raum zu Raum
    vec3 eye = vec3(0.15 * sin(iTime * 0.3), 0.1 * cos(iTime * 0.23), iTime * 0.6);
    vec3 dir = normalize(vec3(p, 1.3));
    dir.xz = rotate(0.12 * sin(iTime * 0.17)) * dir.xz;

    float t = 0.0;
    bool hit = false;
    for (int i = 0; i < 120; i++)
    {
        float d = map(eye + dir * t);
        if (d < 0.0005 * t) { hit = true; break; }
        t += d;
        if (t > 30.0) break;
    }

    vec3 color = vec3(0.0);
    if (hit)
    {
        vec3 q = eye + dir * t;
        vec3 n = normal(q);
        float ao = occlusion(q, n);

        // das Licht des eigenen Raums und der sechs Nachbarn, durch die Gänge
        vec3 home = floor(q / 6.0 + 0.5) * 6.0;
        vec3 light = room(home, q, n);
        light += room(home + vec3(6.0, 0.0, 0.0), q, n) + room(home - vec3(6.0, 0.0, 0.0), q, n);
        light += room(home + vec3(0.0, 6.0, 0.0), q, n) + room(home - vec3(0.0, 6.0, 0.0), q, n);
        light += room(home + vec3(0.0, 0.0, 6.0), q, n) + room(home - vec3(0.0, 0.0, 6.0), q, n);

        color = (vec3(0.004, 0.006, 0.008) + light) * 0.6 * ao * ao;
        // was weit weg ist, verschwindet im Dunkeln
        color *= exp(-0.2 * t);
    }

    color *= 1.0 - 0.3 * dot(p * 0.6, p * 0.6);
    color = 1.0 - exp(-color * 1.2);
    fragColor = vec4(pow(color, vec3(1.0 / 2.2)), 1.0);
}
