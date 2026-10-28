# Häufiger Fehler: für die Spalte c wird die Zeile c genommen

def parity(bits)
  bits.sum % 2
end

def find_error(grid)
  row = nil
  grid.each_with_index do |bits, r|
    row = r if parity(bits) == 1
  end
  column = nil
  (0...grid[0].length).each do |c|
    bits = grid[c] || []
    column = c if parity(bits) == 1
  end
  return nil if row.nil? || column.nil?

  [row, column]
end
