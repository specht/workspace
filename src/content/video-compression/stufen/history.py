from quality import encode, measure

CODECS = [
    ('MPEG-1', 1993, ['-c:v', 'mpeg1video'], 'mpg'),
    ('MPEG-2', 1995, ['-c:v', 'mpeg2video'], 'mpg'),
    ('H.264', 2003, ['-c:v', 'libx264'], 'mp4'),
    ('H.265', 2013, ['-c:v', 'libx265', '-x265-params', 'log-level=error'], 'mp4'),
    ('AV1', 2018, ['-c:v', 'libsvtav1', '-svtav1-params', 'lp=1'], 'mp4'),
]

if __name__ == '__main__':
    for name, year, options, ending in CODECS:
        target = f'history-{name}.{ending}'
        encode('slope.mp4', target, options + ['-b:v', '500k'])
        size, quality = measure('slope.mp4', target)
        print(f'{name:7} {year}  {size:7} Bytes  {quality:5.2f} dB')
