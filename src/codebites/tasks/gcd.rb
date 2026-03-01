require_relative "../include/single_file_task"

def build_judge(executor:, rng:)
    validator = Judge::Validator.new()

    cases = []
    cases << Judge::Case.new(args: [60, 48], expect: Judge::Expect.equals(12))
    cases << Judge::Case.new(args: [101, 10], expect: Judge::Expect.equals(1))
    cases << Judge::Case.new(args: [54, 24], expect: Judge::Expect.equals(6))
    cases << Judge::Case.new(args: [17, 13], expect: Judge::Expect.equals(1))

    3.times do |i|
        a = rng.rand(1..1000)
        b = rng.rand(1..1000)
        expected = a.gcd(b)
        cases << Judge::Case.new(args: [a, b], expect: Judge::Expect.equals(expected))
    end

    Judge::FunctionTask.new(
        function_name: :gcd,
        cases: cases,
        validator: validator,
        executor: executor
    )
end

__END__
@@task.md
## Größten gemeinsamen Teiler berechnen

Implementiere eine Funktion `gcd`, die den größten gemeinsamen Teiler (greatest common divisor, GCD) von zwei Zahlen berechnet. Du kannst dafür z. B. den Algorithmus von Euklid verwenden.

**Beispiele:**

<div class='table-responsive'>
<table class='table table-sm'>
<tr><th>a</th><th>b</th><th></th><th>Rückgabewert</th></tr>
<tr><td>60</td><td>48</td><td>→</td><td>12</td></tr>
<tr><td>17</td><td>5</td><td>→</td><td>1</td></tr>
<tr><td>102</td><td>51</td><td>→</td><td>51</td></tr>
</table>
</div>

@@starter.rb
def gcd(a, b)
    #_
end

@@starter.py
def gcd(a, b):
    #_

@@starter.js
function gcd(a, b) {
    #_
}

@@patch.rb
class Integer
    def gcd(other)
        raise "Use of Integer#gcd is disabled for this exercise."
    end
    def lcm(other)
        raise "Use of Integer#lcm is disabled for this exercise."
    end
end

@@patch.py
import builtins
def _sorted(a, b):
    raise RuntimeError("Use of sorted() is disabled for this exercise.")
builtins.sorted = _sorted

@@patch.js
Array.prototype.max = function() {
    throw new Error("Use of Array.prototype.max is disabled for this exercise.");
};
Array.prototype.sort = function() {
    throw new Error("Use of Array.prototype.sort is disabled for this exercise.");
};
