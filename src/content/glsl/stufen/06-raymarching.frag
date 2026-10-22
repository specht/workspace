// Raymarching: eine Kugel in einem dunklen Studio

const vec3 LAMP = vec3(-0.32, 0.9, -0.3);   // Richtung zur Lampe über der Szene, Länge 1
const vec3 LEFT = vec3(-0.88, 0.30, -0.37);  // Richtung zum Leuchtstreifen links hinten, Länge 1
const vec3 RIGHT = vec3(0.86, 0.38, -0.34);   // Richtung zum Leuchtstreifen rechts hinten, Länge 1
const vec3 DARK = vec3(0.003, 0.004, 0.008); // die Farbe des Studios, wo kein Licht ist

// Abstand von p zur Kugel: zum Mittelpunkt, weniger den Radius
float ball(vec3 p)
{
    return length(p - vec3(0.0, 0.6, 0.0)) - 0.5;
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

// das Studio: fast schwarz, links ein türkiser, rechts ein oranger Leuchtstreifen
vec3 studio(vec3 dir)
{
    vec3 color = DARK;
    color += vec3(0.1, 0.8, 1.0) * 4.0 * smoothstep(0.96, 0.985, dot(dir, LEFT));
    color += vec3(1.0, 0.45, 0.1) * 4.0 * smoothstep(0.96, 0.985, dot(dir, RIGHT));
    return color;
}

// das Licht der beiden Streifen auf einer Fläche, die in Richtung n zeigt
vec3 strips(vec3 n)
{
    return vec3(0.1, 0.8, 1.0) * 0.4 * max(dot(n, LEFT), 0.0)
         + vec3(1.0, 0.45, 0.1) * 0.4 * max(dot(n, RIGHT), 0.0);
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
    vec3 albedo = on_floor ? vec3(0.04) : vec3(0.5);

    // die Lampe leuchtet einen Kreis auf dem Boden aus;
    // im Schatten, wenn ein Strahl zu ihr etwas trifft
    float lamp = max(dot(n, LAMP), 0.0) * smoothstep(2.6, 0.3, length(q.xz));
    if (march(q + n * 0.002, LAMP) > 0.0) lamp = 0.0;
    return albedo * (vec3(1.0, 0.95, 0.9) * 2.0 * lamp + strips(n));
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 p = (2.0 * fragCoord - iResolution.xy) / iResolution.y;

    // die Kamera bei eye schaut auf die Kugel
    vec3 eye = vec3(0.0, 1.4, -3.6);
    vec3 forward = normalize(vec3(0.0, 0.5, 0.0) - eye);
    vec3 right = normalize(cross(vec3(0.0, 1.0, 0.0), forward));
    vec3 up = cross(forward, right);
    vec3 dir = normalize(p.x * right + p.y * up + 1.8 * forward);

    vec3 color = studio(dir);
    float t = march(eye, dir);
    if (t > 0.0)
    {
        color = shade(eye + dir * t, dir);

        // was weit weg ist, verschwindet im Dunkeln
        color = mix(color, DARK, smoothstep(6.0, 16.0, t));
    }

    fragColor = vec4(pow(color, vec3(1.0 / 2.2)), 1.0);
}
