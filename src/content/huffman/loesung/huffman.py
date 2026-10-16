# Huffman-Codes: häufige Bytes bekommen kurze Codes, seltene lange.
# Die Lösung zur Aktivität »Komprimieren mit Huffman«.
#
# Ein Baum ist entweder ein Blatt, das Byte selbst (eine Zahl), oder ein
# Knoten, ein Tupel (links, rechts). Links steht für 0, rechts für 1.

import sys


def frequencies(data):
    """Wie oft jedes Byte vorkommt: {65: 5, 66: 2, ...}"""
    counts = {}
    for byte in data:
        counts[byte] = counts.get(byte, 0) + 1
    return counts


def build_tree(counts):
    """Immer die beiden leichtesten Bäume zusammenfassen, bis einer übrig ist."""
    trees = [(count, byte) for byte, count in counts.items()]
    while len(trees) > 1:
        trees.sort(key=lambda pair: pair[0])
        left = trees.pop(0)
        right = trees.pop(0)
        trees.append((left[0] + right[0], (left[1], right[1])))
    return trees[0][1]


def code_table(tree, prefix=''):
    """Der Code jedes Bytes: der Weg von der Wurzel zu seinem Blatt."""
    if isinstance(tree, int):
        return {tree: prefix or '0'}   # ein einziges Byte: Code 0
    table = code_table(tree[0], prefix + '0')
    table.update(code_table(tree[1], prefix + '1'))
    return table


def encode(data, table):
    return ''.join(table[byte] for byte in data)


def decode(bits, tree):
    if isinstance(tree, int):
        return bytes([tree] * len(bits))
    out = []
    node = tree
    for bit in bits:
        node = node[0] if bit == '0' else node[1]
        if isinstance(node, int):
            out.append(node)
            node = tree
    return bytes(out)


def canonical(lengths):
    """Kanonische Codes aus den Längen allein, so wie JPEG sie speichert."""
    table = {}
    code = 0
    previous = 0
    for byte, length in sorted(lengths.items(), key=lambda item: (item[1], item[0])):
        code <<= length - previous
        table[byte] = format(code, 'b').rjust(length, '0')
        code += 1
        previous = length
    return table


data = open(sys.argv[1], 'rb').read() if len(sys.argv) > 1 else b'ABRACADABRA'
tree = build_tree(frequencies(data))
table = code_table(tree)
bits = encode(data, table)
assert decode(bits, tree) == data
print(f'{len(data)} Bytes, {len(bits)} Bits, {(len(bits) + 7) // 8} Bytes, {len(table)} verschiedene Bytes')
