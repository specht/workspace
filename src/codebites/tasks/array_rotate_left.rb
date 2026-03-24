require_relative "../include/single_file_task"

def build_judge(executor:, rng:)
    validator = Judge::Validator.new()

    cases = []
    cases << Judge::Case.new(args: [[2, 7, 11, 15]], expect: Judge::Expect.equals([2, 7, 11, 15].rotate(1)))
    cases << Judge::Case.new(args: [[1, 2, 3, 4, 5]], expect: Judge::Expect.equals([1, 2, 3, 4, 5].rotate(1)))
    cases << Judge::Case.new(args: [[]], expect: Judge::Expect.equals([]))

    3.times do |i|
        size = rng.rand(0..200)
        nums = Array.new(size) { rng.rand(-(10 ** (i + 1))..(10 ** (i + 1))) }
        cases << Judge::Case.new(args: [nums], expect: Judge::Expect.equals(nums.rotate(1)))
    end

    Judge::FunctionTask.new(
        function_name: :array_rotate_left,
        cases: cases,
        validator: validator,
        executor: executor
    )
end

__END__
@@task.md
## Array nach links rotieren

Implementiere eine Funktion `array_rotate_left`, die die Elemente eines Arrays um eine Position nach links rotiert.

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
<td>[7, 11, 15, 2]</td>
</tr>
</table>

*Achtung:* Falls das Array leer ist, soll die Funktion auch ein leeres Array zurückgeben.

@@starter.rb
def array_rotate_left(nums)
    #_
end

@@starter.py
def array_rotate_left(nums):
    #_

@@starter.js
function array_rotate_left(nums) {
    #_
}

@@patch.rb
class Array
    def rotate(*)
        raise "Use of Array#rotate is disabled for this exercise."
    end

    def rotate!(*)
        raise "Use of Array#rotate! is disabled for this exercise."
    end
end

@@patch.py
import collections

def _blocked_deque_rotate(self, n=1):
    raise RuntimeError("Use of deque.rotate() is disabled for this exercise.")

collections.deque.rotate = _blocked_deque_rotate

@@meta.yaml
difficulty: 2
