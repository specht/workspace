// Raytracer: Kugeln, ein Schachbrett, Spiegel, Glas, Lack, Metall und weiche Schatten.
// Pfeiltasten links/rechts: um die Szene kreisen, hoch/runter: höher oder tiefer.
// Jedes Bild ist ein weiterer Durchgang; das Bild wird mit der Zeit glatter.
#include "pixelram.h"
#include <math.h>
#include <stdio.h>

#define W 320
#define H 180

// Treffer, die näher als EPS am Anfang des Strahls liegen, zählen nicht
#define EPS 1e-4
// wie oft ein Strahl höchstens gespiegelt oder gebrochen wird
#define MAX_DEPTH 6

// ein Vektor: ein Punkt, eine Richtung oder eine Farbe (x, y, z = rot, grün, blau)
typedef struct
{
    double x, y, z;
} vec;

vec add(vec a, vec b)
{
    return (vec){a.x + b.x, a.y + b.y, a.z + b.z};
}

vec sub(vec a, vec b)
{
    return (vec){a.x - b.x, a.y - b.y, a.z - b.z};
}

vec scale(vec a, double s)
{
    return (vec){a.x * s, a.y * s, a.z * s};
}

// Farben mischen: jede Komponente einzeln multiplizieren
vec mul(vec a, vec b)
{
    return (vec){a.x * b.x, a.y * b.y, a.z * b.z};
}

double dot(vec a, vec b)
{
    return a.x * b.x + a.y * b.y + a.z * b.z;
}

// steht senkrecht auf a und auf b
vec cross(vec a, vec b)
{
    return (vec){a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x};
}

double length(vec a)
{
    return sqrt(dot(a, a));
}

// gleiche Richtung, Länge 1
vec normalize(vec a)
{
    return scale(a, 1 / length(a));
}

// überblenden: t = 0 ergibt a, t = 1 ergibt b
vec mix(vec a, vec b, double t)
{
    return add(scale(a, 1 - t), scale(b, t));
}

// Zufall (xorshift): eine Zahl von 0 bis unter 1
uint32_t seed = 2463534242u;

double random_double(void)
{
    seed ^= seed << 13;
    seed ^= seed >> 17;
    seed ^= seed << 5;
    return seed / 4294967296.0;
}

// ein zufälliger Punkt in der Kugel mit Radius 1 um den Nullpunkt
vec random_in_sphere(void)
{
    while (true)
    {
        vec p = {2 * random_double() - 1, 2 * random_double() - 1, 2 * random_double() - 1};
        if (dot(p, p) < 1)
            return p;
    }
}

// der Himmel: am Horizont hell, nach oben tiefblau
vec sky(vec dir)
{
    double up = dir.y > 0 ? sqrt(dir.y) : 0;
    return mix((vec){0.95, 0.9, 0.85}, (vec){0.15, 0.4, 0.9}, up);
}

typedef struct
{
    vec color;
    double mirror;  // Anteil, der gespiegelt wird: 0 bis 1
    double glass;   // Brechungsindex bei Glas, sonst 0
    bool varnish;   // lackiert: spiegelt ein wenig, flach mehr als steil
    bool metal;     // Metall färbt, was es spiegelt
    double rough;   // wie matt die Spiegelung ist: 0 glatt, 0.3 matt
    bool planet;    // mit Textur
} material;

typedef struct
{
    vec center;
    double radius;
    material mat;
} sphere;

sphere spheres[] = {
    {{-0.3, 1.2, -1.4}, 1.2, {.color = {0.9, 0.9, 0.95}, .mirror = 0.9}},                                  // Spiegelkugel
    {{1.5, 0.8, 0.8}, 0.8, {.color = {0.95, 1.0, 0.97}, .glass = 1.5}},                                    // Glaskugel
    {{-1.5, 0.5, 2.4}, 0.5, {.color = {0.95, 1.0, 0.97}, .glass = 1.5}},                                   // hohle Glaskugel: außen Glas
    {{-1.5, 0.5, 2.4}, 0.48, {.color = {1, 1, 1}, .glass = 1 / 1.5}},                                      // innen Luft
    {{-2.2, 0.9, 0.3}, 0.9, {.planet = true}},                                                             // Planet
    {{-0.5, 0.45, 1.2}, 0.45, {.color = {0.9, 0.15, 0.1}, .varnish = true}},                               // rot, lackiert
    {{3.0, 0.4, -0.4}, 0.4, {.color = {1.0, 0.75, 0.3}, .mirror = 0.95, .metal = true, .rough = 0.12}},    // Gold
    {{2.1, 0.35, 1.7}, 0.35, {.color = {0.95, 0.55, 0.4}, .mirror = 0.95, .metal = true, .rough = 0.35}},  // Kupfer, matt
    {{0.4, 0.3, 2.3}, 0.3, {.color = {0.1, 0.35, 0.9}, .varnish = true, .rough = 0.3}},                    // blau, seidenmatt
};
// so viele Kugeln stehen in der Liste
#define SPHERE_COUNT (int)(sizeof spheres / sizeof spheres[0])

material floor_mat = {.color = {0.75, 0.75, 0.75}, .mirror = 0.1};

// die Richtung zur Sonne (Länge 1) und ihre Farbe
vec sun_dir;
vec sun_color = {1.4, 1.2, 0.9};
// wie groß die Sonne am Himmel ist (im Bogenmaß)
#define SUN_SIZE 0.08

// Abstand t, in dem der Strahl origin + t * dir die Kugel trifft, sonst INFINITY
double hit_sphere(vec origin, vec dir, sphere s)
{
    vec oc = sub(origin, s.center);
    double b = dot(oc, dir);
    double c = dot(oc, oc) - s.radius * s.radius;
    double d = b * b - c;
    if (d < 0)
        return INFINITY;  // daneben
    double t = -b - sqrt(d);
    if (t > EPS)
        return t;
    t = -b + sqrt(d);     // der Strahl startet in der Kugel (Glas)
    if (t > EPS)
        return t;
    return INFINITY;      // die Kugel liegt hinter dem Auge
}

// Abstand t, in dem der Strahl den Boden y = 0 trifft, sonst INFINITY
double hit_floor(vec origin, vec dir)
{
    double t = -origin.y / dir.y;
    if (t > EPS)
        return t;
    return INFINITY;
}

// Textur: die Farbe an dem Punkt der Kugel, in den die Normale n zeigt
vec planet_color(vec n)
{
    double u = atan2(n.z, n.x);  // Längengrad: -pi bis pi
    double v = asin(n.y);        // Breitengrad: -pi/2 bis pi/2

    // Streifen nach dem Breitengrad, breite und feine, leicht gewellt
    double band = 0.5 + 0.5 * sin(13 * v + 0.3 * sin(5 * u + 8 * v)) + 0.12 * sin(45 * v + 2 * sin(3 * u));
    vec color = mix((vec){0.58, 0.32, 0.16}, (vec){0.92, 0.84, 0.68}, band);

    // der große rote Fleck: r ist 0 in seiner Mitte und 1 an seinem Rand
    double du = (u - 1.0) / 0.36;
    double dv = (v + 0.38) / 0.15;
    double r = sqrt(du * du + dv * dv);
    // um ihn herum ein heller Ring, innen rot, zum Rand hin dunkler
    color = mix(color, (vec){0.93, 0.86, 0.72}, fmax(0, 1 - fabs(r - 1.15) / 0.35));
    vec spot = mix((vec){0.88, 0.47, 0.26}, (vec){0.62, 0.22, 0.1}, r * r);
    return mix(color, spot, fmin(1, fmax(0, (1 - r) / 0.2)));
}

// alles über einen Treffer
typedef struct
{
    double t;     // Abstand vom Anfang des Strahls
    vec point;    // Treffpunkt
    vec normal;   // steht senkrecht auf der Oberfläche, Länge 1
    material mat;
} hit;

// sucht den nächsten Treffer; false, wenn der Strahl in den Himmel geht
bool nearest_hit(vec origin, vec dir, hit *h)
{
    h->t = hit_floor(origin, dir);
    int nearest = -1;  // -1: der Boden
    for (int i = 0; i < SPHERE_COUNT; i++)
    {
        double t = hit_sphere(origin, dir, spheres[i]);
        if (t < h->t)
        {
            h->t = t;
            nearest = i;
        }
    }
    if (h->t == INFINITY)
        return false;

    h->point = add(origin, scale(dir, h->t));
    if (nearest == -1)
    {
        h->normal = (vec){0, 1, 0};
        h->mat = floor_mat;
        // Schachbrett: jedes zweite Feld dunkel
        if ((int)(floor(h->point.x) + floor(h->point.z)) % 2 == 0)
            h->mat.color = (vec){0.05, 0.06, 0.08};
    }
    else
    {
        sphere s = spheres[nearest];
        h->normal = scale(sub(h->point, s.center), 1 / s.radius);
        h->mat = s.mat;
        if (s.mat.planet)
            h->mat.color = planet_color(h->normal);
    }
    return true;
}

// wie viel Licht von point aus in Richtung dir durchkommt: 1, 0 oder dazwischen
double light_through(vec point, vec dir)
{
    double light = 1;
    for (int i = 0; i < SPHERE_COUNT; i++)
    {
        if (hit_sphere(point, dir, spheres[i]) < INFINITY)
        {
            if (spheres[i].mat.glass == 0)
                return 0;
            light *= 0.6;  // Glas lässt etwas durch
        }
    }
    return light;
}

// Licht von der Sonne: zu einem zufälligen Punkt der Sonnenscheibe
vec sunlight(hit h)
{
    vec to_sun = normalize(add(sun_dir, scale(random_in_sphere(), SUN_SIZE)));
    double d = dot(h.normal, to_sun);
    if (d <= 0)
        return (vec){0, 0, 0};
    return scale(sun_color, d * light_through(h.point, to_sun));
}

// der Strahl dir prallt an der Oberfläche mit der Normalen n ab
vec reflect(vec dir, vec n)
{
    return sub(dir, scale(n, 2 * dot(dir, n)));
}

// Licht vom Himmel: aus einer zufälligen Richtung über der Oberfläche
vec skylight(hit h)
{
    vec d = normalize(add(h.normal, random_in_sphere()));
    if (d.y < 0)
        return (vec){0, 0, 0};  // nach unten ist der Boden im Weg
    return scale(sky(d), 0.3 * light_through(h.point, d));
}

vec trace(vec origin, vec dir, int depth);

// Fresnel nach Schlick: wie viel eine Oberfläche mit diesem Brechungsindex
// spiegelt, flach mehr als steil
double fresnel(double cos_in, double index)
{
    double r0 = (1 - index) / (1 + index);
    r0 = r0 * r0;
    return r0 + (1 - r0) * pow(1 - cos_in, 5);
}

// Glas: ein Teil wird gespiegelt, der Rest gebrochen
vec glass(hit h, vec dir, int depth)
{
    vec n = h.normal;
    double eta = 1 / h.mat.glass;  // von der Luft ins Glas
    if (dot(dir, n) > 0)
    {
        n = scale(n, -1);          // von innen nach außen
        eta = h.mat.glass;
    }
    double cos_in = -dot(dir, n);
    vec reflected = trace(h.point, reflect(dir, n), depth + 1);

    // Brechung nach Snellius; k < 0: Totalreflexion
    double k = 1 - eta * eta * (1 - cos_in * cos_in);
    if (k < 0)
        return reflected;
    vec refracted_dir = add(scale(dir, eta), scale(n, eta * cos_in - sqrt(k)));
    vec refracted = mul(trace(h.point, refracted_dir, depth + 1), h.mat.color);

    return mix(refracted, reflected, fresnel(cos_in, h.mat.glass));
}

// die Farbe, die aus Richtung dir beim Punkt origin ankommt
vec trace(vec origin, vec dir, int depth)
{
    if (depth > MAX_DEPTH)
        return (vec){0, 0, 0};  // zu oft gespiegelt

    hit h;
    if (!nearest_hit(origin, dir, &h))
    {
        if (dot(dir, sun_dir) > cos(SUN_SIZE))
            // die Sonnenscheibe: ihr ganzes Licht kommt aus dieser kleinen Fläche
            return scale(sun_color, 1 / (3.14159 * SUN_SIZE * SUN_SIZE));
        return sky(dir);
    }

    if (h.mat.glass > 0)
        return glass(h, dir, depth);

    vec light = add(sunlight(h), skylight(h));
    vec color = mul(h.mat.color, light);

    // Spiegel und Lack: ein neuer Strahl vom Treffpunkt aus
    double shine = h.mat.mirror;
    if (h.mat.varnish)
        shine = fresnel(-dot(dir, h.normal), 1.5);  // Lack ist wie eine dünne Schicht Glas
    if (shine > 0)
    {
        vec r = reflect(dir, h.normal);
        // matt: in jedem Durchgang ein wenig anders, im Mittel verschwommen
        if (h.mat.rough > 0)
        {
            r = normalize(add(r, scale(random_in_sphere(), h.mat.rough)));
            if (dot(r, h.normal) < 0)
                r = reflect(dir, h.normal);
        }
        vec reflected = trace(h.point, r, depth + 1);
        if (h.mat.metal)
            reflected = mul(reflected, h.mat.color);
        color = mix(color, reflected, shine);
    }

    // Dunst: was weit weg ist, verschwimmt mit dem Himmel
    double haze = exp(-0.0004 * h.t * h.t);
    return mix(sky(dir), color, haze);
}

// die Kamera: wo das Auge steht und wohin vorne, rechts und oben ist
vec eye, forward, right, up;

// die Kamera kreist im Abstand 5 um die Mitte und schaut auf target
void set_camera(double angle, double height)
{
    eye = (vec){5 * sin(angle), height, 5 * cos(angle)};
    vec target = {0, 0.8, 0};
    forward = normalize(sub(target, eye));
    right = normalize(cross(forward, (vec){0, 1, 0}));
    up = cross(right, forward);
}

// die Richtung vom Auge durch den Pixel (x, y)
vec ray_dir(double x, double y)
{
    double sx = (x - W / 2.0) / H;
    double sy = (H / 2.0 - y) / H;
    return normalize(add(forward, add(scale(right, sx), scale(up, sy))));
}

// Farbwert 0 bis 1 zu 0 bis 255, mit Gamma für den Bildschirm
uint8_t to_byte(double c)
{
    if (c > 1)
        c = 1;
    return (uint8_t)(pow(c, 1 / 2.2) * 255 + 0.5);
}

// für jeden Pixel: die Summe der Farben aus allen Durchgängen
vec sum[H][W];

// ein Durchgang: ein Strahl je Pixel, durch einen zufälligen Punkt im Pixel
void draw(int passes)
{
    for (int y = 0; y < H; y++)
    {
        for (int x = 0; x < W; x++)
        {
            vec c = trace(eye, ray_dir(x + random_double(), y + random_double()), 0);
            if (passes == 1)
                sum[y][x] = c;  // der erste Durchgang: neu anfangen
            else
                sum[y][x] = add(sum[y][x], c);

            // gezeigt wird der Durchschnitt aller Durchgänge
            vec avg = scale(sum[y][x], 1.0 / passes);
            set_pixel_rgb(x, y, to_byte(avg.x), to_byte(avg.y), to_byte(avg.z));
        }
    }
}

int main(void)
{
    screen_open(W, H, pixel_rgb24, "Raytracer");
    sun_dir = normalize((vec){1.4, 1.0, 0.3});

    double angle = 0.2, height = 1.8;
    int passes = 0;
    double last = seconds();

    while (!should_close())
    {
        // die Zeit seit dem letzten Bild, höchstens eine Zehntelsekunde
        double now = seconds();
        double dt = fmin(now - last, 0.1);
        last = now;

        // Pfeiltasten: um die Szene kreisen, höher oder tiefer, so schnell
        // pro Sekunde, egal wie viele Bilder der Rechner schafft
        if (key_down(pixel_key_left))  { angle -= 1.2 * dt; passes = 0; }
        if (key_down(pixel_key_right)) { angle += 1.2 * dt; passes = 0; }
        if (key_down(pixel_key_up) && height < 6)     { height += 2 * dt; passes = 0; }
        if (key_down(pixel_key_down) && height > 0.3) { height -= 2 * dt; passes = 0; }

        // jedes Bild ein weiterer Durchgang: das Bild wird immer glatter
        set_camera(angle, height);
        passes++;
        draw(passes);

        char title[64];
        snprintf(title, sizeof title, "Raytracer – %d Durchgänge", passes);
        set_title(title);
        present();
    }
    screen_close();
    return 0;
}
