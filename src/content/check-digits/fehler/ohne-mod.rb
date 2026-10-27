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
  10 - total % 10
end

def valid_ean?(code)
  return false unless code.length == 13
  return false unless code.chars.all? { |c| ('0'..'9').include?(c) }

  digits = code.chars.map(&:to_i)
  check_digit(digits[0..11]) == digits[12]
end

def valid_isbn?(isbn)
  text = isbn.delete('-')
  return false unless text.length == 10

  total = 0
  text.chars.each_with_index do |c, i|
    if c == 'X' && i == 9
      value = 10
    elsif ('0'..'9').include?(c)
      value = c.to_i
    else
      return false
    end
    total = total + (10 - i) * value
  end
  total % 11 == 0
end
