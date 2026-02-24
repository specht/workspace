require_relative "../../include/core"
require_relative "../../include/function_task"

def build_task(executor:)
    validator = Judge::Validator.new()

    cases = []
    cases << Judge::Case.new(args: [[2,7,11,15]], expect: Judge::Expect.equals(35))
    cases << Judge::Case.new(args: [[1,2,3,4,5]], expect: Judge::Expect.equals(15))

    nums = []
    100.times { nums << rand(-1000..1000) }
    cases << Judge::Case.new(args: [nums], expect: Judge::Expect.equals(nums.sum))

    Judge::FunctionTask.new(
        function_name: :array_sum,
        cases: cases,
        validator: validator,
        executor: executor
    )
end