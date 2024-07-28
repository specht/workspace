-- Generate an array of 10 random integers
local array = {}
for i = 1, 10 do
    array[i] = math.random(1, 100)
end

-- Print the original array
print("Original array:")
for i = 1, 10 do
    io.write(array[i] .. " ")
end
print()

-- Bubble sort function
local function bubbleSort(arr)
    for i = 1, #arr do
        for j = 1, #arr - i do
            if arr[j] > arr[j + 1] then
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
            end
        end
    end
end

-- Sort the array using bubble sort
bubbleSort(array)

-- Print the sorted array
print("Sorted array:")
for i = 1, 10 do
    io.write(array[i] .. " ")
end
print()
