# Häufiger Fehler: die Zahl der Einsen statt ihres Rests bei der Division durch 2

def parity(bits)
  ones = 0
  bits.each do |bit|
    ones = ones + bit
  end
  ones
end
