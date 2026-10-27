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
