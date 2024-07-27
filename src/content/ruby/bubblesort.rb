# Generate an array of 10 random integers
numbers = Array.new(10) { rand(1..100) }

# Print the original array
puts "Original array: #{numbers}"

# Bubble sort algorithm
n = numbers.length
loop do
    swapped = false

    (n-1).times do |i|
        if numbers[i] > numbers[i+1]
            numbers[i], numbers[i+1] = numbers[i+1], numbers[i]
            swapped = true
        end
    end

    break unless swapped
end

# Print the sorted array
puts "Sorted array: #{numbers}"