# Prüfziffern: Strichcodes (EAN-13) und Bücher (ISBN-10)

def weight(index)
  if index.even?
    1
  else
    3
  end
end

def check_digit(digits)
  total = 0
  (0..11).each do |i|
    total = total + weight(i) * digits[i]
  end
  (10 - total % 10) % 10
end

def valid_ean?(code)
  return false unless code.length == 13
  return false unless code.chars.all? { |c| ('0'..'9').include?(c) }

  digits = code.chars.map(&:to_i)
  check_digit(digits[0..11]) == digits[12]
end
