# Ein Gitter aus Bits mit Paritätsbits: Jede Zeile und jede Spalte hat eine
# gerade Zahl von Einsen. Kippt ein Bit, findet find_error, wo.

def parity(bits)
  ones = 0
  bits.each do |bit|
    ones = ones + bit
  end
  ones % 2
end

def find_error(grid)
  # die Zeile mit einer ungeraden Zahl von Einsen
  row = nil
  grid.each_with_index do |bits, r|
    row = r if parity(bits) == 1
  end
  # die Spalte mit einer ungeraden Zahl von Einsen
  column = nil
  (0...grid[0].length).each do |c|
    bits = grid.map { |line| line[c] }
    column = c if parity(bits) == 1
  end
  # alles gerade: kein Fehler
  return nil if row.nil? || column.nil?

  [row, column]
end
