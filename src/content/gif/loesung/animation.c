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

// GIF speichert Zahlen mit zwei Bytes im Little Endian: das niedrige Byte zuerst
int read_word(void)
{
    int low = read_byte();
    int high = read_byte();
    return low | (high << 8);
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

// Data Sub-blocks: Länge, so viele Bytes, Länge, …, bis zur Länge 0.
// Hängt alle Bytes hintereinander in data, wenn data nicht NULL ist.
long read_sub_blocks(uint8_t *data)
{
    long size = 0;
    while (1)
    {
        int length = read_byte();
        if (length == 0)
            return size;
        for (int i = 0; i < length; i++)
        {
            uint8_t b = read_byte();
            if (data)
                data[size] = b;
            size++;
        }
    }
}

// Farbtabellen: die globale für alle Bilder, eine lokale nur für ein Bild
uint8_t global_colors[256][3];
uint8_t local_colors[256][3];

void read_color_table(uint8_t colors[256][3], int count)
{
    for (int i = 0; i < count; i++)
    {
        colors[i][0] = read_byte();
        colors[i][1] = read_byte();
        colors[i][2] = read_byte();
    }
}

// Aus der Graphic Control Extension, gilt für das nächste Bild
int disposal = 0;
int delay = 0;
int transparent = -1;
uint8_t background[3];

void read_graphic_control(void)
{
    read_byte();   // Block Size: 4
    uint8_t packed = read_byte();
    disposal = (packed >> 2) & 7;
    delay = read_word();
    int index = read_byte();
    transparent = (packed & 1) ? index : -1;
    read_sub_blocks(NULL);   // Block Terminator
}

// Codes lesen: GIF füllt jedes Byte vom niedrigsten Bit an
uint8_t *bits;
long bit_pos;

int read_code(int size)
{
    int code = 0;
    for (int i = 0; i < size; i++)
    {
        int bit = (bits[bit_pos / 8] >> (bit_pos % 8)) & 1;
        code |= bit << i;
        bit_pos++;
    }
    return code;
}

// Die Tabelle: Eintrag k ist der Eintrag prefix[k] und dahinter das Byte suffix[k]
int prefix[4096];
uint8_t suffix[4096];
uint8_t first[4096];
int length[4096];

// Schreibt den Eintrag code nach out[n] und gibt die neue Länge zurück
long output(int code, uint8_t *out, long n, long out_size)
{
    long end = n + length[code];
    if (end > out_size)
        fail("Mehr Pixel als das Bild hat.");
    // Die Kette führt vom letzten Byte zum ersten, also von hinten schreiben
    for (long i = end - 1; i >= n; i--)
    {
        out[i] = suffix[code];
        code = prefix[code];
    }
    return end;
}

long decode_lzw(uint8_t *data, long data_size, int min_code_size, uint8_t *out, long out_size)
{
    int clear_code = 1 << min_code_size;
    int end_code = clear_code + 1;
    for (int i = 0; i < clear_code; i++)
    {
        prefix[i] = -1;
        suffix[i] = i;
        first[i] = i;
        length[i] = 1;
    }
    int next = end_code + 1;
    int code_size = min_code_size + 1;
    int prev = -1;
    long n = 0;

    bits = data;
    bit_pos = 0;
    while (bit_pos + code_size <= data_size * 8)
    {
        int code = read_code(code_size);
        if (code == clear_code)
        {
            next = end_code + 1;
            code_size = min_code_size + 1;
            prev = -1;
            continue;
        }
        if (code == end_code)
            break;
        if (code > next || (code == next && prev == -1))
            fail("Unbekannter Code.");

        if (prev != -1 && next < 4096)
        {
            // Neuer Eintrag: der vorige Eintrag und das erste Byte des jetzigen.
            // Ist der jetzige Code genau der neue, ist dieses Byte das erste des vorigen.
            prefix[next] = prev;
            suffix[next] = (code == next) ? first[prev] : first[code];
            first[next] = first[prev];
            length[next] = length[prev] + 1;
            next++;
        }
        n = output(code, out, n, out_size);
        prev = code;

        if (next == (1 << code_size) && code_size < 12)
            code_size++;
    }
    return n;
}

// Zeile i der Daten ist bei Interlacing eine andere Zeile im Bild
int image_row(int i, int height, int interlaced)
{
    if (!interlaced)
        return i;
    int start[4] = {0, 4, 2, 1};
    int step[4] = {8, 8, 4, 2};
    for (int pass = 0; pass < 4; pass++)
    {
        int rows = (height - start[pass] + step[pass] - 1) / step[pass];
        if (i < rows)
            return start[pass] + i * step[pass];
        i -= rows;
    }
    return 0;
}

// Adler-32 über Rot, Grün und Blau aller Pixel, Zeile für Zeile
uint32_t screen_checksum(void)
{
    uint32_t a = 1, b = 0;
    for (int y = 0; y < screen_height(); y++)
        for (int x = 0; x < screen_width(); x++)
        {
            uint8_t rgb[3];
            get_pixel_rgb(x, y, &rgb[0], &rgb[1], &rgb[2]);
            for (int i = 0; i < 3; i++)
            {
                a = (a + rgb[i]) % 65521;
                b = (b + a) % 65521;
            }
        }
    return (b << 16) | a;
}

// Was unter einem Bild war, für Disposal Method 3
uint8_t *saved;

// Nur, was auf dem Bildschirm liegt: Ein Bild darf über den Rand ragen
int on_screen(int x, int y)
{
    return x >= 0 && y >= 0 && x < screen_width() && y < screen_height();
}

void save_area(int left, int top, int width, int height)
{
    for (int y = 0; y < height; y++)
        for (int x = 0; x < width; x++)
        {
            if (!on_screen(left + x, top + y))
                continue;
            uint8_t *p = saved + ((long) (top + y) * screen_width() + left + x) * 3;
            get_pixel_rgb(left + x, top + y, &p[0], &p[1], &p[2]);
        }
}

void dispose(int left, int top, int width, int height)
{
    for (int y = 0; y < height; y++)
        for (int x = 0; x < width; x++)
        {
            if (!on_screen(left + x, top + y))
                continue;
            if (disposal == 2)
                set_pixel_rgb(left + x, top + y, background[0], background[1], background[2]);
            else if (disposal == 3)
            {
                uint8_t *p = saved + ((long) (top + y) * screen_width() + left + x) * 3;
                set_pixel_rgb(left + x, top + y, p[0], p[1], p[2]);
            }
        }
}

int first_pass = 1;

void read_image(void)
{
    int left = read_word();
    int top = read_word();
    int width = read_word();
    int height = read_word();
    uint8_t packed = read_byte();
    uint8_t (*colors)[3] = global_colors;
    if (packed & 0x80)
    {
        read_color_table(local_colors, 2 << (packed & 7));
        colors = local_colors;
    }
    int interlaced = (packed & 0x40) != 0;

    int min_code_size = read_byte();
    long start = pos;
    long data_size = read_sub_blocks(NULL);
    pos = start;
    uint8_t *data = malloc(data_size);
    read_sub_blocks(data);

    uint8_t *pixels = calloc((long) width * height, 1);
    decode_lzw(data, data_size, min_code_size, pixels, (long) width * height);

    if (disposal == 3)
        save_area(left, top, width, height);
    for (int i = 0; i < height; i++)
    {
        int y = top + image_row(i, height, interlaced);
        for (int x = 0; x < width; x++)
        {
            int index = pixels[(long) i * width + x];
            if (index != transparent)
                set_pixel_rgb(left + x, y, colors[index][0], colors[index][1], colors[index][2]);
        }
    }
    free(data);
    free(pixels);

    // Ist das das letzte Bild? Dann kommt gleich der Trailer.
    if (first_pass && pos < file_size && file[pos] == 0x3B)
        printf("Prüfsumme nach dem letzten Bild %08x\n", screen_checksum());

    // Zeigen, warten, dann wegräumen, wie es die Graphic Control Extension sagt.
    // Browser warten bei weniger als 2 Hundertstelsekunden 10, das tun wir auch.
    present();
    sleep_ms((delay < 2 ? 10 : delay) * 10);
    dispose(left, top, width, height);
    disposal = 0;
    delay = 0;
    transparent = -1;
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        printf("Welche Datei? make PROGRAM=animation ARGS=flug.gif\n");
        return 1;
    }
    read_file(argv[1]);

    // Header
    if (read_byte() != 'G' || read_byte() != 'I' || read_byte() != 'F')
        fail("Das ist keine GIF-Datei.");
    pos += 3;   // Version: 87a oder 89a

    // Logical Screen Descriptor
    int width = read_word();
    int height = read_word();
    uint8_t packed = read_byte();
    int background_index = read_byte();
    read_byte();   // Pixel Aspect Ratio

    if (!screen_open(width, height, pixel_rgb24, argv[1]))
        return 1;
    if (packed & 0x80)
        read_color_table(global_colors, 2 << (packed & 7));
    for (int i = 0; i < 3; i++)
        background[i] = global_colors[background_index][i];
    clear_rgb(background[0], background[1], background[2]);
    saved = malloc((long) width * height * 3);

    // Ab hier wiederholt sich die Animation, bis die Seite geschlossen wird
    long first_block = pos;
    while (!should_close())
    {
        uint8_t introducer = read_byte();
        if (introducer == 0x3B)   // Trailer: von vorn
        {
            first_pass = 0;
            pos = first_block;
        }
        else if (introducer == 0x21)   // Extension
        {
            uint8_t label = read_byte();
            if (label == 0xF9)
                read_graphic_control();
            else
                read_sub_blocks(NULL);
        }
        else if (introducer == 0x2C)   // Image Descriptor
            read_image();
        else
            fail("Unbekannter Block.");
    }
    screen_close();
    return 0;
}
