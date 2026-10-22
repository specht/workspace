// Raymarching: eine Kugel über einem Schachbrett

const vec3 SUN = vec3(0.6, 0.45, -0.66);   // Richtung zur Sonne, Länge 1

// Abstand von p zur Kugel: zum Mittelpunkt, weniger den Radius
float ball(vec3 p)
{
    return length(p - vec3(0.0, 0.8, 0.0)) - 0.5;
}

// Abstand von p zur nächsten Oberfläche: die Kugel oder der Boden (y = 0)
float map(vec3 p)
{
    return min(ball(p), p.y);
}

// die Richtung, in der der Abstand am schnellsten wächst: die Normale
vec3 normal(vec3 p)
{
    vec2 e = vec2(0.001, 0.0);
    return normalize(vec3(map(p + e.xyy) - map(p - e.xyy),
                          map(p + e.yxy) - map(p - e.yxy),
                          map(p + e.yyx) - map(p - e.yyx)));
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
    bool on_floor = ball(q) > q.y;
    vec3 albedo = on_floor
        ? vec3(0.08 + 0.3 * mod(floor(q.x) + floor(q.z), 2.0))
        : vec3(0.9, 0.12, 0.005);

    // im Schatten, wenn ein Strahl zur Sonne etwas trifft
    float sun = max(dot(n, SUN), 0.0);
    if (march(q + n * 0.002, SUN) > 0.0) sun = 0.0;
    return albedo * (vec3(1.0, 0.8, 0.6) * 1.5 * sun + sky(n) * 0.25);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 p = (2.0 * fragCoord - iResolution.xy) / iResolution.y;

    // die Kamera bei eye schaut auf die Kugel
    vec3 eye = vec3(0.0, 1.4, -3.6);
    vec3 forward = normalize(vec3(0.0, 0.7, 0.0) - eye);
    vec3 right = normalize(cross(vec3(0.0, 1.0, 0.0), forward));
    vec3 up = cross(forward, right);
    vec3 dir = normalize(p.x * right + p.y * up + 1.8 * forward);

    vec3 color = sky(dir);
    float t = march(eye, dir);
    if (t > 0.0)
    {
        color = shade(eye + dir * t, dir);

        // Dunst: was weit weg ist, verschwindet im Himmel
        color = mix(color, sky(dir), 1.0 - exp(-0.0015 * t * t));
    }

    fragColor = vec4(pow(color, vec3(1.0 / 2.2)), 1.0);
}
