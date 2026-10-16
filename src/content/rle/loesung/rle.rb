# Lauflängen: statt »AAAA« steht »4A« da, die Länge eines Laufs und was
# sich wiederholt. Die Lösung zur Aktivität »Komprimieren mit Lauflängen«.

# Als Text: 'WWWWWWBBBWWWW' wird '6W3B4W'. Geht nur ohne Ziffern im Text.
def rle(text)
    out = ''
    text.each_char.chunk_while { |a, b| a == b }.each do |run|
        out << "#{run.size}#{run[0]}"
    end
    out
end

def unrle(code)
    code.scan(/(\d+)(\D)/).map { |count, char| char * count.to_i }.join
end

# Als Bytes: immer zwei, die Länge (1 bis 255) und das Byte.
def rle_bytes(data)
    out = []
    data.each_byte.chunk_while { |a, b| a == b }.each do |run|
        run.each_slice(255) { |part| out << part.size << part[0] }
    end
    out
end

def unrle_bytes(bytes)
    out = []
    bytes.each_slice(2) { |count, byte| out.concat([byte] * count) }
    out.pack('C*')
end

p rle('WWWWWWBBBWWWW')
if ARGV[0]
    data = File.binread(ARGV[0])
    packed = rle_bytes(data)
    raise 'unrle_bytes liefert etwas anderes' unless unrle_bytes(packed) == data.b
    puts "#{data.bytesize} Bytes werden zu #{packed.size}"
end
