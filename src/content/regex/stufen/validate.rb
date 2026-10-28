# Eingaben prüfen, bevor ein Programm mit ihnen arbeitet
def postcode?(text)
    text.match?(/\A\d{5}\z/)
end

['10115', '1011', 'D-10115 Berlin', '101150'].each do |text|
    puts "#{text.inspect} #{postcode?(text)}"
end
