require_relative "../include/single_file_task"
require 'prime'

def build_judge(executor:, rng:)
    validator = Judge::Validator.new()

    cases = []
    cases << Judge::Case.new(args: [4], expect: Judge::Expect.equals(false))
    cases << Judge::Case.new(args: [17], expect: Judge::Expect.equals(true))
    cases << Judge::Case.new(args: [51], expect: Judge::Expect.equals(false))

    all_numbers = (1..1000).to_a
    primes = Prime.each(1000).to_a
    not_primes = all_numbers - primes
    not_primes.sample(5).each do |n|
        cases << Judge::Case.new(args: [n], expect: Judge::Expect.equals(false))
    end
    primes.sample(5).each do |n|
        cases << Judge::Case.new(args: [n], expect: Judge::Expect.equals(true))
    end

    Judge::FunctionTask.new(
        function_name: :is_prime,
        cases: cases,
        validator: validator,
        executor: executor
    )
end

__END__
@@task.md
## Primzahl erkennen

Implementiere eine Funktion `is_prime`, die überprüft, ob eine Zahl eine Primzahl ist.

**Beispiele:**

<div class='table-responsive'>
<table class='table table-sm'>
<tr><th>n</th><th></th><th>Rückgabewert</th></tr>
<tr><td>4</td><td>→</td><td>false</td></tr>
<tr><td>17</td><td>→</td><td>true</td></tr>
<tr><td>67</td><td>→</td><td>true</td></tr>
</table>
</div>

@@starter.rb
def is_prime(n)
    #_
end

@@starter.py
def is_prime(n):
    #_

@@starter.js
function is_prime(n) {
    #_
}

@@patch.rb
class Integer
    def prime?
        raise "Use of Integer#prime? is disabled for this exercise."
    end
end
class Prime
    def self.each(limit)
        raise "Use of Prime.each is disabled for this exercise."
    end
end

@@patch.py
import builtins
def _is_prime(n):
    raise RuntimeError("Use of is_prime() is disabled for this exercise.")
builtins.is_prime = _is_prime