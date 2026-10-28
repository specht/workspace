# Eingaben prüfen, bevor ein Programm mit ihnen arbeitet
def postcode?(text)
    text.match?(/\A\d{5}\z/)
end

# Ein Datum wie 24.12.2026: Tag 01 bis 31, Monat 01 bis 12, ein Jahr mit vier Ziffern
def date?(text)
    text.match?(/\A(0[1-9]|[12][0-9]|3[01])\.(0[1-9]|1[0-2])\.\d{4}\z/)
end

# 3 bis 16 Zeichen: kleine Buchstaben, Ziffern und _, vorn ein Buchstabe
def username?(text)
    text.match?(/\A[a-z][a-z0-9_]{2,15}\z/)
end

['10115', '1011', 'D-10115 Berlin', '101150'].each do |text|
    puts "#{text.inspect} #{postcode?(text)}"
end
['24.12.2026', '32.01.2026', '1.1.2026', '31.02.2026'].each do |text|
    puts "#{text.inspect} #{date?(text)}"
end
