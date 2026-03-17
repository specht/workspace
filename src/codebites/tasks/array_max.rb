require_relative "../include/single_file_task"

def build_judge(executor:, rng:)
    validator = Judge::Validator.new()

    cases = []
    cases << Judge::Case.new(args: [[1, 2, 3, 4, 5]], expect: Judge::Expect.equals(5))
    cases << Judge::Case.new(args: [[2, 7, 11, 15]], expect: Judge::Expect.equals(15))
    cases << Judge::Case.new(args: [[]], expect: Judge::Expect.equals(nil))

    3.times do |i|
        size = rng.rand(3..200)
        nums = Array.new(size) { rng.rand(-(10 ** (i + 1))..(10 ** (i + 1))) }
        cases << Judge::Case.new(args: [nums], expect: Judge::Expect.equals(nums.max))
    end
    1.times do |i|
        size = rng.rand(3..8)
        nums = Array.new(size) { rng.rand(1..(10 ** (i + 1))) }
        nums = nums.map { |n| -n }
        cases << Judge::Case.new(args: [nums], expect: Judge::Expect.equals(nums.max))
    end

    Judge::FunctionTask.new(
        function_name: :find_max,
        cases: cases,
        validator: validator,
        executor: executor
    )
end

__END__
@@task.md
## Größte Zahl finden

Implementiere eine Funktion `find_max`, die die größte Zahl in einem Array findet.

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
<td colspan="3">15</td>
</tr>
</table>

*Achtung:* Falls das Array leer ist, soll die Funktion `nil` zurückgeben.

@@starter.rb
def find_max(nums)
    #_
end

@@starter.py
def find_max(nums):
    #_

@@starter.js
function find_max(nums) {
    #_
}

@@patch.rb
class Array
    def max
        raise "Use of Array#max is disabled for this exercise."
    end
end

@@patch.py
import builtins
def _max(iterable, start=None):
    raise RuntimeError("Use of max() is disabled for this exercise.")
builtins.max = _max

@@meta.yaml
difficulty: 1
