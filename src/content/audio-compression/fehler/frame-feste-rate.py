BITRATES = [0, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320]
RATES = [44100, 48000, 32000]


def id3_size(data):
    if data[:3] != b'ID3':
        return 0
    size = 0
    for byte in data[6:10]:
        size = size * 128 + byte
    return 10 + size


def frame_length(header):
    bitrate = BITRATES[header[2] >> 4] * 1000
    rate = 44100
    padding = (header[2] >> 1) & 1
    return 144 * bitrate // rate + padding


if __name__ == '__main__':
    with open('musik.mp3', 'rb') as f:
        data = f.read()
    pos = id3_size(data)
    print(f'ID3-Tag: {pos} Bytes')
    frames = 0
    padded = 0
    while pos + 4 <= len(data) and data[pos] == 0xFF and data[pos + 1] & 0xE0 == 0xE0:
        length = frame_length(data[pos:pos + 4])
        padded += (data[pos + 2] >> 1) & 1
        pos += length
        frames += 1
    print(f'{frames} Frames, {padded} davon mit Padding, {len(data) - pos} Bytes danach')
    print(f'{(frames - 1) * 1152 / 44100:.4f} Sekunden Klang')
