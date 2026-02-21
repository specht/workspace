# Return indices of two numbers that add up to target.
def two_sum(nums, target)
    nums.each.with_index do |n1, i|
        nums.each_with_index do |n2, j|
            return [i, j] if i != j && n1 + n2 == target
        end
    end
    nil
end
