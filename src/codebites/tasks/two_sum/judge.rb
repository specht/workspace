require_relative "../../include/core"
require_relative "../../include/docker_executor"
require_relative "../../include/function_task"

include Judge

# Property validator for Two Sum (Ruby-side, shared across languages)
two_sum_valid = lambda do |actual, args|
  nums, target = args

  return [true, nil] if actual.nil? && !nums.is_a?(Array) # just defensive; optional
  return [true, nil] if actual.nil? && nums.is_a?(Array) && nums.length < 2 && target # optional

  return [false, "Expected [i, j] or nil, got #{actual.inspect}"] unless actual.is_a?(Array) && actual.length == 2
  i, j = actual

  return [false, "Indices must be integers"] unless i.is_a?(Integer) && j.is_a?(Integer)
  return [false, "Indices must be different"] unless i != j
  return [false, "Index out of bounds"] unless i.between?(0, nums.length - 1) && j.between?(0, nums.length - 1)
  return [false, "Values do not sum to target"] unless nums[i] + nums[j] == target

  [true, nil]
end

validator = Validator.new(two_sum_valid: two_sum_valid)

cases = [
  Case.new(args: [[2,7,11,15], 9], expect: Expect.property(:two_sum_valid)),
  Case.new(args: [[1,2,3,4], 10], expect: Expect.equals(nil)),
  Case.new(args: [[2], 4],        expect: Expect.equals(nil))
]

executor = DockerExecutor.new(image: "hackschule-exec-python") # or python image; judge stays same
task = FunctionTask.new(function_name: :two_sum, cases: cases, validator: validator, executor: executor)

submission_code = STDIN.read
res = task.run(submission_code: submission_code)

puts JSON.generate(res.to_h)
exit(res.to_h[:status] == "pass" ? 0 : 1)