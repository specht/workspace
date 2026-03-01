require_relative "../../include/core"
require_relative "../../include/function_task"

def build_task(executor:)
    # No custom properties needed for this task, but FunctionTask requires a validator.
    validator = Judge::Validator.new

    cases = [
        Judge::Case.new(
            args: [[1, 2, 3, 4]],
            expect: Judge::Expect.equals([4, 3, 2, 1])
        ),
        Judge::Case.new(
            args: [[42]],
            expect: Judge::Expect.equals([42])
        ),
        Judge::Case.new(
            args: [[]],
            expect: Judge::Expect.equals([])
        ),
        Judge::Case.new(
            args: [["a", "b", "c"]],
            expect: Judge::Expect.equals(["c", "b", "a"])
        ),
        Judge::Case.new(
            args: [[1, 1, 2, 2]],
            expect: Judge::Expect.equals([2, 2, 1, 1])
        )
    ]

    Judge::FunctionTask.new(
        function_name: :reverse_array,
        cases: cases,
        validator: validator,
        executor: executor
    )
end