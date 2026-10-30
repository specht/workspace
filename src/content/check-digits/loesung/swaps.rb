require_relative 'check_digits'

missed = 0
(0..9).each do |a|
  (0..9).each do |b|
    next if a == b

    digits = [a, b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    code = (digits + [check_digit(digits)]).join
    swapped = code[1] + code[0] + code[2..]
    missed = missed + 1 if valid_ean?(swapped)
  end
end
puts "Unbemerkt: #{missed} von 90"
