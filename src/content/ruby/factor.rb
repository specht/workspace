print "Enter a number: "
number = gets.chomp.to_i

factors = []
divisor = 2

while number > 1
    if number % divisor == 0
        factors << divisor
        number /= divisor
    else
        divisor += 1
    end
end

puts "Prime factors: #{factors.join(', ')}"