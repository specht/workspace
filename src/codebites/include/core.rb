require "json"

module Judge
    Case = Struct.new(:args, :expect, keyword_init: true)

    # Expectations:
    # - { kind: :equals, value: ... }
    # - { kind: :property, name: :two_sum_valid }
    module Expect
        def self.equals(value) = { kind: :equals, value: value }
        def self.property(name) = { kind: :property, name: name }
    end

    class Result
        attr_reader :tests, :status, :error

        def initialize
            @tests = []
            @status = "pass"
            @error = nil
        end

        def add_test(entry)
            @tests << entry
            @status = "fail" if entry[:status] != "pass" && @status == "pass"
        end

        def set_error(type:, message:, location: nil)
            @status = "error"
            @error = { type: type, message: message, location: location }
        end

        def to_h
            { status: @status, tests: @tests, error: @error }
        end
    end

    # Applies expectations on the Ruby side (language-agnostic)
    class Validator
        def initialize(property_validators = {})
            @property_validators = property_validators # name => proc(actual, args) -> [ok, message]
        end

        def validate(expect, actual, args)
            case expect[:kind]
            when :equals
                ok = (actual == expect[:value])
                msg = ok ? nil : "Expected #{expect[:value].inspect}, got #{actual.inspect}"
                [ok, msg]
            when :property
                fn = @property_validators.fetch(expect[:name])
                fn.call(actual, args)
            else
                [false, "Unknown expectation: #{expect.inspect}"]
            end
        end
    end
end