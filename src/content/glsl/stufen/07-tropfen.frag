// Raymarching: Tropfen, die ineinanderfließen, über einem Schachbrett

const vec3 SUN = vec3(0.6, 0.45, -0.66);   // Richtung zur Sonne, Länge 1

// glatte Vereinigung zweier Abstände: je größer k, desto weiter fließen sie ineinander
float smin(float a, float b, float k)
{
    float h = clamp(0.5 + 0.5 * (b - a) / k, 0.0, 1.0);
    return mix(b, a, h) - k * h * (1.0 - h);
}

// wo die sechs Tropfen gerade sind; mainImage rechnet es einmal für jeden Pixel aus
vec3 blob[6];

void move_blobs()
{
    for (int i = 0; i < 6; i++)
    {
        float f = float(i);
        float t = iTime * (0.4 + 0.1 * f) + f * 1.7;
        blob[i] = vec3(1.3 * sin(t), 0.6 + 0.12 * sin(t * 1.3 + f), 0.9 * cos(t * 0.9 + f));
    }
}

// die Farbe des Tropfens i
vec3 blob_paint(int i)
{
    if (i == 0) return vec3(0.9, 0.12, 0.005);
    if (i == 1) return vec3(0.0, 0.25, 0.32);
    if (i == 2) return vec3(0.6, 0.005, 0.12);
    if (i == 3) return vec3(0.9, 0.5, 0.005);
    if (i == 4) return vec3(0.05, 0.05, 0.5);
    return vec3(0.15, 0.45, 0.02);
}

// Abstand von p zu den Tropfen
float blobs(vec3 p)
{
    // weit weg genügt der Abstand zu einer Kugel, die alle Tropfen umschließt
    float bound = length(p - vec3(0.0, 0.6, 0.0)) - 2.2;
    if (bound > 0.5) return bound;

    float d = 100.0;
    for (int i = 0; i < 6; i++)
        d = smin(d, length(p - blob[i]) - 0.42, 0.6);
    return d;
}

// Abstand von p zur nächsten Oberfläche: die Tropfen oder der Boden (y = 0)
float map(vec3 p)
{
    return min(blobs(p), p.y);
}

// die Farbe der Tropfen bei p: jeder Tropfen färbt umso mehr, je näher er ist
vec3 blob_color(vec3 p)
{
    vec3 sum = vec3(0.0);
    float weight = 0.0;
    for (int i = 0; i < 6; i++)
    {
        float w = exp(-14.0 * (length(p - blob[i]) - 0.42));
        sum += w * blob_paint(i);
        weight += w;
    }
    return sum / weight;
}

// die Richtung, in der der Abstand am schnellsten wächst: die Normale
vec3 normal(vec3 p)
{
    vec2 e = vec2(0.001, 0.0);
    return normalize(vec3(map(p + e.xyy) - map(p - e.xyy),
                          map(p + e.yxy) - map(p - e.yxy),
                          map(p + e.yyx) - map(p - e.yyx)));
}

// weicher Schatten: wie knapp läuft der Strahl zur Sonne an etwas vorbei?
float shadow(vec3 p)
{
    float light = 1.0;
    float t = 0.02;
    for (int i = 0; i < 48; i++)
    {
        float d = map(p + SUN * t);
        light = min(light, 10.0 * d / t);
        t += clamp(d, 0.02, 0.2);
        // über allen Tropfen kann nichts mehr Schatten werfen
        if (light < 0.001 || p.y + SUN.y * t > 1.5) break;
    }
    return clamp(light, 0.0, 1.0);
}

vec3 sky(vec3 dir)
{
    vec3 color = mix(vec3(0.9, 0.65, 0.45), vec3(0.05, 0.2, 0.6), pow(max(dir.y, 0.0), 0.3));
    return color + vec3(1.0, 0.8, 0.5) * pow(max(dot(dir, SUN), 0.0), 400.0) * 6.0;
}

// Raymarching: so weit gehen, wie der Abstand erlaubt; -1, wenn nichts kommt
float march(vec3 from, vec3 dir)
{
    float t = 0.0;
    for (int i = 0; i < 100; i++)
    {
        float d = map(from + dir * t);
        if (d < 0.001 * t) return t;
        t += d;
        if (t > 40.0) break;
    }
    return -1.0;
}

// die Farbe des Punktes q, den ein Strahl in Richtung dir trifft
vec3 shade(vec3 q, vec3 dir)
{
    vec3 n = normal(q);
    bool on_floor = blobs(q) > q.y;
    vec3 albedo = on_floor
        ? vec3(0.08 + 0.3 * mod(floor(q.x) + floor(q.z), 2.0))
        : blob_color(q);

    float sun = max(dot(n, SUN), 0.0) * shadow(q + n * 0.002);
    return albedo * (vec3(1.0, 0.8, 0.6) * 1.5 * sun + sky(n) * 0.25);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 p = (2.0 * fragCoord - iResolution.xy) / iResolution.y;
    move_blobs();

    // die Kamera schwenkt vor den Tropfen hin und her, die Sonne im Rücken,
    // und schaut auf ihre Mitte
    float angle = 0.7 * sin(0.2 * iTime);
    vec3 eye = vec3(3.6 * sin(angle), 1.4, -3.6 * cos(angle));
    vec3 forward = normalize(vec3(0.0, 0.6, 0.0) - eye);
    vec3 right = normalize(cross(vec3(0.0, 1.0, 0.0), forward));
    vec3 up = cross(forward, right);
    vec3 dir = normalize(p.x * right + p.y * up + 1.8 * forward);

    vec3 color = sky(dir);
    float t = march(eye, dir);
    if (t > 0.0)
    {
        color = shade(eye + dir * t, dir);

        // Dunst: was weit weg ist, verschwindet im Himmel, bei 40 ganz
        color = mix(color, sky(dir), smoothstep(0.0, 40.0, t));
    }

    fragColor = vec4(pow(color, vec3(1.0 / 2.2)), 1.0);
}
