-- Function to find prime factors
function primeFactors(n)
    local factors = {}
    for i = 2, n do
        while n % i == 0 do
            table.insert(factors, i)
            n = n / i
        end
    end
    return factors
end

-- Read number from user
io.write("Enter a number: ")
local number = tonumber(io.read())

-- Find and print prime factors
local factors = primeFactors(number)
io.write("Prime factors of " .. number .. ": ")
for _, factor in ipairs(factors) do
    io.write(factor .. " ")
end
print()