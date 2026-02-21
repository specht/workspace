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

    def build_request(submission_code, stream: false)
      {
        mode: "function",
        entry: { name: @function_name.to_s },
        cases: @cases.map { |c| { args: c.args } },
        files: { submission: submission_code },
        stream: stream
      }
    end

    def build_init(submission_code)
      {
        cmd: "init",
        mode: "function",
        entry: @function_name.to_s,
        files: { submission: submission_code }
      }
    end

    def build_case(idx)
      { cmd: "case", index: idx, args: @cases[idx].args }
    end

    # Backwards-compatible, non-streaming.
    def run(submission_code:)
      result = Result.new(store: :failures)
      exec_resp = @executor.run(build_request(submission_code, stream: false))

      if exec_resp["error"]
        e = exec_resp["error"]
        result.set_error(type: e["type"], message: e["message"], location: e["location"])
        return result
      end

      exec_resp.fetch("runs").each_with_index do |run, idx|
        add_run_to_result!(result, run, idx)
      end

      result
    end

    # Streaming: expects executor NDJSON events but also supports legacy.
    #
    # Yields judge-level NDJSON events:
    #   {"event":"test","index":i,"total":n,"test":{...}}
    #   {"event":"error","error":{...}}
    #
    # Returns the final Result.
    def run_stream(submission_code:)
      # Interactive, fail-fast streaming:
      # - start one docker process
      # - send init once
      # - send each case and stream output events live
      # - stop at first failure (runtime error or validator mismatch)
      result = Result.new(store: :failures)
      total = @cases.length

      session = @executor.open_session
      if session.is_a?(Hash) && session["error"]
        e = session["error"]
        result.set_error(type: e["type"], message: e["message"], location: e["location"])
        yield({ "event" => "error", "error" => e }) if block_given?
        return result
      end

      begin
        session.send(build_init(submission_code))

        # Wait for ready or fatal
        loop do
          obj = session.recv
          break if obj.nil?
          if obj["event"] == "ready"
            break
          elsif obj["event"] == "fatal" && obj["error"]
            e = obj["error"]
            result.set_error(type: e["type"], message: e["message"], location: e["location"])
            yield({ "event" => "error", "error" => e }) if block_given?
            session.close
            return result
          end
          # ignore other events during init
        end

        (0...total).each do |idx|
          session.send(build_case(idx))

          loop do
            obj = session.recv
            break if obj.nil?

            if obj["event"] == "output"
              yield(obj) if block_given?
              next
            end

            if obj["event"] == "case" && obj["index"] == idx
              run = { "ok" => obj["ok"], "result" => obj["result"], "error" => obj["error"] }
              test_entry = add_run_to_result!(result, run, idx)
              yield({ "event" => "test", "index" => idx, "total" => total, "test" => test_entry }) if block_given?

              # Fail-fast: stop after first failing test.
              if test_entry[:status] != "pass"
                session.close
                return result
              end
              break
            end

            if obj["event"] == "fatal" && obj["error"]
              e = obj["error"]
              result.set_error(type: e["type"], message: e["message"], location: e["location"])
              yield({ "event" => "error", "error" => e }) if block_given?
              session.close
              return result
            end
          end
        end
      rescue Timeout::Error
        e = { "type" => "Timeout", "message" => "Execution exceeded #{total} cases within timeout", "location" => nil }
        result.set_error(type: e["type"], message: e["message"], location: e["location"])
        yield({ "event" => "error", "error" => e }) if block_given?
      ensure
        session.close if session.respond_to?(:close)
      end

      result
    end

    private

    def add_run_to_result!(result, run, idx)
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
      test_entry
    end
  end
end