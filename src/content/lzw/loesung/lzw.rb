# LZW: komprimieren und entpacken, mit einem Wörterbuch, das nie mitgeschickt
# wird. Die Lösung zur Aktivität »Komprimieren mit LZW«.

# Macht aus den Bytes eines Strings eine Liste von Codes.
def compress(data)
    table = {}
    256.times { |i| table[[i]] = i }
    codes = []
    current = []
    data.each_byte do |byte|
        longer = current + [byte]
        if table.key?(longer)
            current = longer
        else
            codes << table[current]
            table[longer] = table.size
            current = [byte]
        end
    end
    codes << table[current] unless current.empty?
    codes
end

# Macht aus einer Liste von Codes wieder die Bytes, als String.
def decompress(codes)
    return '' if codes.empty?

    table = {}
    256.times { |i| table[i] = [i] }
    previous = table[codes[0]]
    out = previous.dup
    codes.drop(1).each do |code|
        if table.key?(code)
            entry = table[code]
        elsif code == table.size
            # Der Kompressor hat den Eintrag gerade erst angelegt
            entry = previous + [previous[0]]
        else
            raise "Unbekannter Code #{code}"
        end
        table[table.size] = previous + [entry[0]]
        out.concat(entry)
        previous = entry
    end
    out.pack('C*')
end

# Wie viele Bytes die Codes brauchen, wenn jeder so breit ist wie nötig:
# Beim i-ten Code (ab 0) hat die Tabelle 256 + i Einträge, der größte
# mögliche Code ist also 255 + i.
def size_in_bytes(codes)
    bits = codes.each_index.sum { |i| (255 + i).bit_length }
    (bits + 7) / 8
end

if __FILE__ == $0
    p compress('ABABABA')
    if ARGV[0]
        data = File.binread(ARGV[0])
        codes = compress(data)
        raise 'decompress liefert etwas anderes' unless decompress(codes) == data
        puts "#{data.bytesize} Bytes, #{codes.size} Codes, #{size_in_bytes(codes)} Bytes mit wachsender Codebreite"
    end
end
