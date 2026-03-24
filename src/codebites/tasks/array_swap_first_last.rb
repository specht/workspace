require_relative "../include/single_file_task"

def build_judge(executor:, rng:)
    validator = Judge::Validator.new()

    cases = []
    cases << Judge::Case.new(args: [[2, 7, 11, 15]], expect: Judge::Expect.equals([15, 7, 11, 2]))
    cases << Judge::Case.new(args: [[1, 2, 3, 4, 5]], expect: Judge::Expect.equals([5, 2, 3, 4, 1]))
    cases << Judge::Case.new(args: [[]], expect: Judge::Expect.equals([]))

    3.times do |i|
        size = rng.rand(0..200)
        nums = Array.new(size) { rng.rand(-(10 ** (i + 1))..(10 ** (i + 1))) }
        cases << Judge::Case.new(args: [nums], expect: Judge::Expect.equals([nums.last] + nums[1..-2] + [nums.first]))
    end

    Judge::FunctionTask.new(
        function_name: :swap_first_last,
        cases: cases,
        validator: validator,
        executor: executor
    )
end

__END__
@@task.md
## Erstes und letztes Element tauschen

Implementiere eine Funktion `swap_first_last`, die das erste und das letzte Element eines Arrays vertauscht.

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
<td>[15, 7, 11, 2]</td>
</tr>
</table>

@@starter.rb
def swap_first_last(nums)
    #_
end

@@starter.py
def swap_first_last(nums):
    #_

@@starter.js
function swap_first_last(nums) {
    #_
}

@@meta.yaml
difficulty: 1
