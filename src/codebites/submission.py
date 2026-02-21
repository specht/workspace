def two_sum(nums, target):
    for i, a in enumerate(nums):
        for j, b in enumerate(nums):
            if i != j and a + b == target:
                return [i, j]
    return None