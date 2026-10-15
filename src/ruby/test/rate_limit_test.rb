require 'minitest/autorun'
require_relative '../include/rate_limit'

class RateLimitTest < Minitest::Test
    def test_limit_per_key_within_the_period
        limit = RateLimit.new(:limit => 2, :period => 60)
        assert limit.allow?('a', :now => 0)
        assert limit.allow?('a', :now => 10)
        refute limit.allow?('a', :now => 20)
        assert limit.allow?('b', :now => 20)
        assert limit.allow?('a', :now => 61)
        refute limit.allow?('a', :now => 62)
        assert limit.allow?('a', :now => 71)
    end
end
