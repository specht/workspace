require_relative "../include/single_file_task"

def build_judge(executor:, rng:)
    validator = Judge::Validator.new()

    cases = []
    cases << Judge::Case.new(args: [[2, 7, 11, 15]], expect: Judge::Expect.equals([15, 11, 7, 2]))
    cases << Judge::Case.new(args: [[1, 2, 3, 4, 5]], expect: Judge::Expect.equals([5, 4, 3, 2, 1]))
    cases << Judge::Case.new(args: [[]], expect: Judge::Expect.equals([]))

    3.times do |i|
        size = rng.rand(0..200)
        nums = Array.new(size) { rng.rand(-(10 ** (i + 1))..(10 ** (i + 1))) }
        cases << Judge::Case.new(args: [nums], expect: Judge::Expect.equals(nums.reverse))
    end

    Judge::FunctionTask.new(
        function_name: :array_reverse,
        cases: cases,
        validator: validator,
        executor: executor
    )
end

__END__
@@task.md
## Array umkehren

Implementiere eine Funktion `array_reverse`, die die Reihenfolge der Elemente in einem Array umkehrt.

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
<td colspan="2"></td>
<td>[15, 11, 7, 2]</td>
</tr>
</table>

*Achtung:* Falls das Array leer ist, soll die Funktion auch ein leeres Array zurückgeben.

@@starter.rb
def array_reverse(nums)
    #_
end

@@starter.py
def array_reverse(nums):
    #_

@@starter.js
function array_reverse(nums) {
    #_
}

@@patch.rb
class Array
    def reverse
        raise "Use of Array#reverse is disabled for this exercise."
    end
    def reverse!
        raise "Use of Array#reverse! is disabled for this exercise."
    end
end

@@patch.py
import builtins

def _reversed(iterable):
    raise RuntimeError("Use of reversed() is disabled for this exercise.")

builtins.reversed = _reversed

_original_list_reverse = list.reverse
def _blocked_reverse(self):
    raise RuntimeError("Use of list.reverse() is disabled for this exercise.")

list.reverse = _blocked_reverse

@@patch.js

Array.prototype.reverse = function() {
    throw new Error("Use of Array.prototype.reverse is disabled for this exercise.");
};

@@meta.yaml
difficulty: 2
