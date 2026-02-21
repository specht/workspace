require "json"
require_relative "core"

module Judge
  class FunctionTask
    def initialize(function_name:, cases:, validator:, executor:)
      @function_name = function_name
      @cases = cases
      @validator = validator
      @executor = executor
    end

    def call_string(args)
      "#{@function_name}(#{args.map(&:inspect).join(', ')})"
    end

    def run(submission_code:)
      result = Result.new

      request = {
        mode: "function",
        entry: { name: @function_name.to_s },
        cases: @cases.map { |c| { args: c.args } },
        files: { submission: submission_code }
      }

      exec_resp = @executor.run(request)

      # exec_resp example:
      # { "runs":[{"ok":true,"result":...,"error":...}, ...], "error": null }
      if exec_resp["error"]
        e = exec_resp["error"]
        result.set_error(type: e["type"], message: e["message"], location: e["location"])
        return result
      end

      exec_resp.fetch("runs").each_with_index do |run, idx|
        c = @cases[idx]
        args = c.args

        test_entry = {
          call: call_string(args),
          expected: c.expect,
          status: nil,
          actual: nil,
          message: nil,
          location: nil
        }

        if run["ok"]
          actual = run["result"]
          test_entry[:actual] = actual
          ok, msg = @validator.validate(c.expect, actual, args)
          test_entry[:status] = ok ? "pass" : "fail"
          test_entry[:message] = msg
        else
          err = run["error"] || {}
          test_entry[:status] = "fail"
          test_entry[:message] = "#{err["type"]}: #{err["message"]}".strip
          test_entry[:location] = err["location"]
        end

        result.add_test(test_entry)
      end

      result
    end
  end
end