# Huffman-Codes: häufige Bytes bekommen kurze Codes, seltene lange.
# Die Lösung zur Aktivität »Komprimieren mit Huffman«.
#
# Ein Baum ist entweder ein Blatt, das Byte selbst (eine Zahl), oder ein
# Knoten, ein Array [links, rechts]. Links steht für 0, rechts für 1.

# Wie oft jedes Byte vorkommt: {65 => 5, 66 => 2, ...}
def frequencies(data)
    counts = Hash.new(0)
    data.each_byte { |byte| counts[byte] += 1 }
    counts
end

# Immer die beiden leichtesten Bäume zusammenfassen, bis einer übrig ist.
def build_tree(counts)
    trees = counts.map { |byte, count| [count, byte] }
    while trees.size > 1
        trees.sort_by! { |weight, _| weight }
        left = trees.shift
        right = trees.shift
        trees << [left[0] + right[0], [left[1], right[1]]]
    end
    trees[0][1]
end

# Der Code jedes Bytes: der Weg von der Wurzel zu seinem Blatt.
def code_table(tree, prefix = '', table = {})
    if tree.is_a?(Integer)
        table[tree] = prefix.empty? ? '0' : prefix   # ein einziges Byte: Code 0
    else
        code_table(tree[0], prefix + '0', table)
        code_table(tree[1], prefix + '1', table)
    end
    table
end

def encode(data, table)
    data.each_byte.map { |byte| table[byte] }.join
end

def decode(bits, tree)
    return ([tree] * bits.length).pack('C*') if tree.is_a?(Integer)

    out = []
    node = tree
    bits.each_char do |bit|
        node = node[bit == '0' ? 0 : 1]
        if node.is_a?(Integer)
            out << node
            node = tree
        end
    end
    out.pack('C*')
end

# Kanonische Codes aus den Längen allein: nach Länge, dann nach Byte
# sortiert, und jeder Code ist der vorige plus 1, bei mehr Stellen mit
# Nullen dahinter. So speichert JPEG seine Tabellen.
def canonical(lengths)
    table = {}
    code = 0
    previous = 0
    lengths.sort_by { |byte, length| [length, byte] }.each do |byte, length|
        code <<= (length - previous)
        table[byte] = code.to_s(2).rjust(length, '0')
        code += 1
        previous = length
    end
    table
end

data = ARGV[0] ? File.binread(ARGV[0]) : 'ABRACADABRA'
tree = build_tree(frequencies(data))
table = code_table(tree)
bits = encode(data, table)
raise 'decode liefert etwas anderes' unless decode(bits, tree) == data.b
puts "#{data.bytesize} Bytes, #{bits.length} Bits, #{(bits.length + 7) / 8} Bytes, #{table.size} verschiedene Bytes, längster Code #{table.values.map(&:length).max}"
