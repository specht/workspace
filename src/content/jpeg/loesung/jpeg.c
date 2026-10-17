#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "pixelram.h"

// Die ganze Datei im Speicher, und wo wir gerade lesen
uint8_t *file;
long file_size;
long pos = 0;

void fail(const char *message)
{
    printf("Fehler bei Byte %ld: %s\n", pos, message);
    exit(1);
}

uint8_t read_byte(void)
{
    if (pos >= file_size)
        fail("Die Datei ist zu Ende.");
    return file[pos++];
}

// JPEG speichert Zahlen mit zwei Bytes im Big Endian: das hohe Byte zuerst
int read_word(void)
{
    int high = read_byte();
    int low = read_byte();
    return (high << 8) | low;
}

void read_file(const char *path)
{
    FILE *f = fopen(path, "rb");
    if (!f)
    {
        printf("Kann %s nicht öffnen.\n", path);
        exit(1);
    }
    fseek(f, 0, SEEK_END);
    file_size = ftell(f);
    fseek(f, 0, SEEK_SET);
    file = malloc(file_size);
    fread(file, 1, file_size, f);
    fclose(f);
}

// Position k in Zig-Zag-Reihenfolge -> Index im 8x8-Block, Zeile für Zeile
const int ZIGZAG[64] = {
     0,  1,  8, 16,  9,  2,  3, 10,
    17, 24, 32, 25, 18, 11,  4,  5,
    12, 19, 26, 33, 40, 48, 41, 34,
    27, 20, 13,  6,  7, 14, 21, 28,
    35, 42, 49, 56, 57, 50, 43, 36,
    29, 22, 15, 23, 30, 37, 44, 51,
    58, 59, 52, 45, 38, 31, 39, 46,
    53, 60, 61, 54, 47, 55, 62, 63,
};

// Quantisierungstabellen, Zeile für Zeile (nicht mehr Zig-Zag)
int quant[4][64];

void read_dqt(int length)
{
    long end = pos + length - 2;
    while (pos < end)
    {
        int pq_tq = read_byte();
        if (pq_tq >> 4)
            fail("Nur 8-Bit-Quantisierungstabellen.");
        int slot = pq_tq & 15;
        for (int k = 0; k < 64; k++)
            quant[slot][ZIGZAG[k]] = read_byte();
    }
}

// Komponenten aus SOF0, Tabellen aus SOS
typedef struct
{
    int id, h, v, tq, td, ta;
    int prediction;            // der vorige DC-Koeffizient
    int width, height;         // Größe der Ebene in Pixeln, auf ganze MCUs gerundet
    uint8_t *plane;
} component;

component comps[3];
int width, height, hmax = 1, vmax = 1;

void read_sof0(void)
{
    if (read_byte() != 8)
        fail("Nur 8 Bit pro Wert.");
    height = read_word();
    width = read_word();
    if (read_byte() != 3)
        fail("Nur Bilder mit Y, Cb und Cr.");
    for (int i = 0; i < 3; i++)
    {
        comps[i].id = read_byte();
        int hv = read_byte();
        comps[i].h = hv >> 4;
        comps[i].v = hv & 15;
        comps[i].tq = read_byte();
        if (comps[i].h > hmax)
            hmax = comps[i].h;
        if (comps[i].v > vmax)
            vmax = comps[i].v;
    }
}

// Ein kanonischer Huffman-Code als Bereiche je Codelänge
typedef struct
{
    int min_code[17], max_code[17], val_ptr[17];
    uint8_t huffval[256];
} huffman;

huffman dc_tables[4], ac_tables[4];

void read_dht(int length)
{
    long end = pos + length - 2;
    while (pos < end)
    {
        int tc_th = read_byte();
        huffman *t = (tc_th >> 4) ? &ac_tables[tc_th & 15] : &dc_tables[tc_th & 15];
        int bits[17];
        int total = 0;
        for (int len = 1; len <= 16; len++)
        {
            bits[len] = read_byte();
            total += bits[len];
        }
        for (int i = 0; i < total; i++)
            t->huffval[i] = read_byte();
        int code = 0, k = 0;
        for (int len = 1; len <= 16; len++)
        {
            t->val_ptr[len] = k;
            t->min_code[len] = code;
            code += bits[len];
            k += bits[len];
            t->max_code[len] = bits[len] ? code - 1 : -1;
            code <<= 1;
        }
    }
}

void read_sos(void)
{
    read_word();   // Segmentlänge
    if (read_byte() != 3)
        fail("Nur Scans mit allen drei Komponenten.");
    for (int i = 0; i < 3; i++)
    {
        int id = read_byte();
        int td_ta = read_byte();
        for (int c = 0; c < 3; c++)
            if (comps[c].id == id)
            {
                comps[c].td = td_ta >> 4;
                comps[c].ta = td_ta & 15;
            }
    }
    if (read_byte() != 0 || read_byte() != 63 || read_byte() != 0)
        fail("Nur Baseline: Ss = 0, Se = 63, Ah = Al = 0.");
}

// Bits lesen, vom höchsten Bit eines Bytes an. Auf 0xFF folgt in den
// Bilddaten 0x00, das nicht dazugehört (Byte Stuffing).
int bit_buffer, bits_left = 0;

int read_bit(void)
{
    if (bits_left == 0)
    {
        bit_buffer = read_byte();
        if (bit_buffer == 0xFF)
        {
            int next = read_byte();
            if (next != 0x00)
                fail("Ein Marker mitten in den Bilddaten.");
        }
        bits_left = 8;
    }
    bits_left--;
    return (bit_buffer >> bits_left) & 1;
}

int read_bits(int n)
{
    int value = 0;
    for (int i = 0; i < n; i++)
        value = (value << 1) | read_bit();
    return value;
}

int decode_huffman(huffman *t)
{
    int code = 0;
    for (int len = 1; len <= 16; len++)
    {
        code = (code << 1) | read_bit();
        if (code <= t->max_code[len])
            return t->huffval[t->val_ptr[len] + code - t->min_code[len]];
    }
    fail("Kein gültiger Huffman-Code.");
    return 0;
}

// Die n Bits einer Zahl: höchstes Bit 0 heißt negativ
int extend(int value, int n)
{
    if (n == 0)
        return 0;
    if (value < (1 << (n - 1)))
        value -= (1 << n) - 1;
    return value;
}

const double PI = 3.14159265358979323846;

double idct_cos[8][8];   // idct_cos[x][u] = C(u) · cos((2x + 1) u π / 16)

void init_idct(void)
{
    for (int x = 0; x < 8; x++)
        for (int u = 0; u < 8; u++)
            idct_cos[x][u] = (u == 0 ? sqrt(0.5) : 1.0) * cos((2 * x + 1) * u * PI / 16);
}

// Ein Block: Koeffizienten lesen, dequantisieren, iDCT, in die Ebene schreiben
void decode_block(component *c, int bx, int by)
{
    int coef[64] = {0};
    int s = decode_huffman(&dc_tables[c->td]);
    c->prediction += extend(read_bits(s), s);
    coef[0] = c->prediction * quant[c->tq][0];
    for (int k = 1; k < 64; k++)
    {
        int rs = decode_huffman(&ac_tables[c->ta]);
        int run = rs >> 4, size = rs & 15;
        if (size == 0)
        {
            if (run == 15)
            {
                k += 15;   // ZRL: 16 Nullen, die 16. zählt die Schleife
                continue;
            }
            break;         // EOB
        }
        k += run;
        if (k > 63)
            fail("Zu viele Koeffizienten in einem Block.");
        coef[ZIGZAG[k]] = extend(read_bits(size), size) * quant[c->tq][ZIGZAG[k]];
    }

    // iDCT, erst über die Zeilen, dann über die Spalten
    double tmp[64];
    for (int v = 0; v < 8; v++)
        for (int x = 0; x < 8; x++)
        {
            double sum = 0;
            for (int u = 0; u < 8; u++)
                sum += idct_cos[x][u] * coef[v * 8 + u];
            tmp[v * 8 + x] = sum;
        }
    for (int y = 0; y < 8; y++)
        for (int x = 0; x < 8; x++)
        {
            double sum = 0;
            for (int v = 0; v < 8; v++)
                sum += idct_cos[y][v] * tmp[v * 8 + x];
            int value = (int) lround(sum / 4 + 128);
            if (value < 0)
                value = 0;
            if (value > 255)
                value = 255;
            c->plane[(long) (by * 8 + y) * c->width + bx * 8 + x] = value;
        }
}

uint8_t clamp(double value)
{
    if (value < 0)
        return 0;
    if (value > 255)
        return 255;
    return (uint8_t) lround(value);
}

void decode_scan(void)
{
    int mcus_x = (width + 8 * hmax - 1) / (8 * hmax);
    int mcus_y = (height + 8 * vmax - 1) / (8 * vmax);
    for (int i = 0; i < 3; i++)
    {
        comps[i].width = mcus_x * comps[i].h * 8;
        comps[i].height = mcus_y * comps[i].v * 8;
        comps[i].plane = calloc((long) comps[i].width * comps[i].height, 1);
        comps[i].prediction = 0;
    }
    init_idct();
    for (int my = 0; my < mcus_y; my++)
        for (int mx = 0; mx < mcus_x; mx++)
            for (int i = 0; i < 3; i++)
                for (int v = 0; v < comps[i].v; v++)
                    for (int h = 0; h < comps[i].h; h++)
                        decode_block(&comps[i], mx * comps[i].h + h, my * comps[i].v + v);

    // Farbraumkonvertierung; Cb und Cr sind bei Subsampling kleiner und
    // werden einfach vergrößert: ein Wert für hmax × vmax Pixel
    for (int y = 0; y < height; y++)
        for (int x = 0; x < width; x++)
        {
            double ycc[3];
            for (int i = 0; i < 3; i++)
            {
                component *c = &comps[i];
                int cx = x * c->h / hmax, cy = y * c->v / vmax;
                ycc[i] = c->plane[(long) cy * c->width + cx];
            }
            double Y = ycc[0], Cb = ycc[1] - 128, Cr = ycc[2] - 128;
            set_pixel_rgb(x, y, clamp(Y + 1.402 * Cr),
                                clamp(Y - 0.344136 * Cb - 0.714136 * Cr),
                                clamp(Y + 1.772 * Cb));
        }
}

// Mittelwerte von Rot, Grün und Blau über das ganze Bild
void print_average(void)
{
    double sum[3] = {0, 0, 0};
    for (int y = 0; y < screen_height(); y++)
        for (int x = 0; x < screen_width(); x++)
        {
            uint8_t rgb[3];
            get_pixel_rgb(x, y, &rgb[0], &rgb[1], &rgb[2]);
            for (int i = 0; i < 3; i++)
                sum[i] += rgb[i];
        }
    double n = (double) screen_width() * screen_height();
    printf("Mittelwerte: Rot %.1f, Grün %.1f, Blau %.1f\n", sum[0] / n, sum[1] / n, sum[2] / n);
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        printf("Welche Datei? make PROGRAM=jpeg ARGS=44-baseline.jpg\n");
        return 1;
    }
    read_file(argv[1]);
    if (read_byte() != 0xFF || read_byte() != 0xD8)
        fail("Das ist keine JPEG-Datei.");

    while (1)
    {
        if (read_byte() != 0xFF)
            fail("Hier sollte ein Marker stehen.");
        int marker = read_byte();
        if (marker == 0xD9)
            break;
        int length = read_word();
        long next = pos + length - 2;
        if (marker == 0xDB)
            read_dqt(length);
        else if (marker == 0xC0)
        {
            read_sof0();
            if (!screen_open(width, height, pixel_rgb24, argv[1]))
                return 1;
        }
        else if (marker >= 0xC1 && marker <= 0xCF && marker != 0xC4 && marker != 0xC8 && marker != 0xCC)
            fail("Nur Baseline-JPEG (SOF0).");
        else if (marker == 0xC4)
            read_dht(length);
        else if (marker == 0xDA)
        {
            pos -= 2;
            read_sos();
            decode_scan();
            break;
        }
        pos = next;
    }

    printf("%s: %d x %d\n", argv[1], width, height);
    print_average();
    return 0;
}
