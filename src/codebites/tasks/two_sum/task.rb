require_relative "../../include/core"
require_relative "../../include/function_task"

def build_task(executor:)
    two_sum_valid = lambda do |actual, args|
        nums, target = args

        # Expect nil when no solution (based on your tests/spec)
        return [true, nil] if actual.nil?

        return [false, "Expected [i, j] or nil, got #{actual.inspect}"] unless actual.is_a?(Array) && actual.length == 2
        i, j = actual

        return [false, "Indices must be integers"] unless i.is_a?(Integer) && j.is_a?(Integer)
        return [false, "Indices must be different"] unless i != j
        return [false, "Index out of bounds"] unless i.between?(0, nums.length - 1) && j.between?(0, nums.length - 1)
        return [false, "Values do not sum to target"] unless nums[i] + nums[j] == target

        [true, nil]
    end

    validator = Judge::Validator.new(two_sum_valid: two_sum_valid)

    cases = [
        Judge::Case.new(args: [[2,7,11,15], 9], expect: Judge::Expect.property(:two_sum_valid)),
        Judge::Case.new(args: [[1,2,3,4], 10],  expect: Judge::Expect.equals(nil)),
        Judge::Case.new(args: [[2], 4],         expect: Judge::Expect.equals(nil))
    ]

    Judge::FunctionTask.new(
        function_name: :two_sum,
        cases: cases,
        validator: validator,
        executor: executor
    )
end