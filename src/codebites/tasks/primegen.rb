require_relative "../include/single_file_task"
require 'prime'

def build_judge(executor:, rng:)
    validator = Judge::Validator.new()

    cases = []
    primes = Prime.each(1000).to_a
    cases << Judge::Case.new(args: [4], expect: Judge::Expect.equals(primes.select { |p| p <= 4 }))
    cases << Judge::Case.new(args: [17], expect: Judge::Expect.equals(primes.select { |p| p <= 17 }))

    3.times do |i|
        n = rng.rand(100..1000)
        cases << Judge::Case.new(args: [n], expect: Judge::Expect.equals(primes.select { |p| p <= n }))
    end

    Judge::FunctionTask.new(
        function_name: :generate_primes,
        cases: cases,
        validator: validator,
        executor: executor
    )
end

__END__
@@task.md
## Primzahlen generieren

Implementiere eine Funktion `generate_primes`, die alle Primzahlen bis zu einer gegebenen Zahl n zurückgibt.

**Beispiele:**

<div class='table-responsive'>
<table class='table table-sm'>
<tr><th>n</th><th></th><th>Rückgabewert</th></tr>
<tr><td>8</td><td>→</td><td>[2, 3, 5, 7]</td></tr>
<tr><td>17</td><td>→</td><td>[2, 3, 5, 7, 11, 13, 17]</td></tr>
</table>
</div>

@@starter.rb
def generate_primes(n)
    #_
end

@@starter.py
def generate_primes(n):
    #_

@@starter.js
function generate_primes(n) {
    #_
}

@@meta.yaml
difficulty: 4