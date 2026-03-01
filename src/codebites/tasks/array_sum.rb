require_relative "../include/single_file_task"

def build_judge(executor:, rng:)
    validator = Judge::Validator.new()

    cases = []
    cases << Judge::Case.new(args: [[1, 2, 3, 4, 5]], expect: Judge::Expect.equals(15))
    cases << Judge::Case.new(args: [[2, 7, 11, 15]], expect: Judge::Expect.equals(35))
    cases << Judge::Case.new(args: [[]], expect: Judge::Expect.equals(0))

    # Random cases: use rng injected by runner (so you can have true-random per run)
    3.times do |i|
        size = rng.rand(0..200)
        nums = Array.new(size) { rng.rand(-(10 ** (i + 1))..(10 ** (i + 1))) }
        cases << Judge::Case.new(args: [nums], expect: Judge::Expect.equals(nums.reduce(0, :+)))
    end

    Judge::FunctionTask.new(
        function_name: :array_sum,
        cases: cases,
        validator: validator,
        executor: executor
    )
end

__END__
@@task.md
## Summe mehrerer Zahlen berechnen

Implementiere eine Funktion `array_sum`, die die Summe aller Zahlen in einem Array berechnet.

**Beispiel:**

<table>
<tr>
<td rowspan="1">Eingabe:</td>
<td>nums</td>
<td>=</td>
<td>[2, 7, 11, 15]</td>
</tr>
<tr>
<td>Ausgabe:</td>
<td colspan="3">35</td>
</tr>
</table>

*Achtung:* Falls das Array leer ist, soll die Funktion 0 zurückgeben.

@@starter.rb
def array_sum(nums)
    #_
end

@@starter.py
def array_sum(nums):
    #_

@@starter.js
function array_sum(nums) {
    #_
}

@@patch.rb
class Array
    def sum
        raise "Use of Array#sum is disabled for this exercise."
    end
end

@@patch.py
import builtins
def _sum(iterable, start=0):
    raise RuntimeError("Use of sum() is disabled for this exercise.")
builtins.sum = _sum