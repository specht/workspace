#include "pixelram.h"
#include <math.h>

#define W 320
#define H 180

// Treffer, die näher als EPS am Anfang des Strahls liegen, zählen nicht
#define EPS 1e-4

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

// der Himmel: am Horizont hell, nach oben tiefblau
vec sky(vec dir)
{
    double up = dir.y > 0 ? sqrt(dir.y) : 0;
    return mix((vec){0.95, 0.9, 0.85}, (vec){0.15, 0.4, 0.9}, up);
}

typedef struct
{
    vec color;
    bool planet;  // mit Textur
} material;

typedef struct
{
    vec center;
    double radius;
    material mat;
} sphere;

sphere spheres[] = {
    {{-0.3, 1.2, -1.4}, 1.2, {.color = {0.6, 0.6, 0.65}}},
    {{1.5, 0.8, 0.8}, 0.8, {.color = {0.9, 0.9, 0.9}}},
    {{-2.2, 0.9, 0.3}, 0.9, {.planet = true}},
    {{-0.5, 0.45, 1.2}, 0.45, {.color = {0.9, 0.15, 0.1}}},
    {{3.0, 0.4, -0.4}, 0.4, {.color = {0.95, 0.7, 0.1}}},
    {{0.4, 0.3, 2.3}, 0.3, {.color = {0.1, 0.35, 0.9}}},
};
// so viele Kugeln stehen in der Liste
#define SPHERE_COUNT (int)(sizeof spheres / sizeof spheres[0])

material floor_mat = {.color = {0.75, 0.75, 0.75}};

// die Richtung zur Sonne (Länge 1) und ihre Farbe
vec sun_dir;
vec sun_color = {1.4, 1.2, 0.9};
// Licht vom Himmel: kommt von überall, damit die Schattenseite nicht schwarz ist
vec ambient = {0.12, 0.15, 0.22};

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

// Licht von der Sonne: je steiler sie auf die Oberfläche scheint, desto heller
vec sunlight(hit h)
{
    double d = dot(h.normal, sun_dir);
    if (d <= 0)
        return (vec){0, 0, 0};
    // Schatten: liegt etwas zwischen dem Punkt und der Sonne?
    hit blocker;
    if (nearest_hit(h.point, sun_dir, &blocker))
        return (vec){0, 0, 0};
    return scale(sun_color, d);
}

// die Farbe, die aus Richtung dir beim Punkt origin ankommt
vec trace(vec origin, vec dir)
{
    hit h;
    if (!nearest_hit(origin, dir, &h))
        return sky(dir);

    vec light = add(sunlight(h), ambient);
    return mul(h.mat.color, light);
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
            vec c = trace(eye, ray_dir(x + random_double(), y + random_double()));
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

    while (!should_close())
    {
        // Pfeiltasten: um die Szene kreisen, höher oder tiefer
        if (key_down(pixel_key_left))  { angle -= 0.03; passes = 0; }
        if (key_down(pixel_key_right)) { angle += 0.03; passes = 0; }
        if (key_down(pixel_key_up) && height < 6)     { height += 0.05; passes = 0; }
        if (key_down(pixel_key_down) && height > 0.3) { height -= 0.05; passes = 0; }

        // jedes Bild ein weiterer Durchgang: das Bild wird immer glatter
        set_camera(angle, height);
        passes++;
        draw(passes);
        present();
    }
    screen_close();
    return 0;
}
