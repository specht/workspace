require_relative "../include/single_file_task"

def build_judge(executor:, rng:)
    validator = Judge::Validator.new()

    cases = []

    # Fixed cases
    cases << Judge::Case.new(args: [[4, 5, 7, 8]], expect: Judge::Expect.equals(6))
    cases << Judge::Case.new(args: [[-2, -1, 0, 2]], expect: Judge::Expect.equals(1))
    cases << Judge::Case.new(args: [[100, 101, 102, 104]], expect: Judge::Expect.equals(103))

    # Random cases
    3.times do |i|
        start = rng.rand(-(10 ** (i + 1))..(10 ** (i + 1)))
        length = rng.rand(3..50) # at least 3 numbers
        full = (start...(start + length)).to_a

        missing = full[rng.rand(1...(full.length-1))]
        nums = full - [missing]
        nums.shuffle!(random: rng)

        cases << Judge::Case.new(
            args: [nums],
            expect: Judge::Expect.equals(missing)
        )
    end

    Judge::FunctionTask.new(
        function_name: :find_missing_number,
        cases: cases,
        validator: validator,
        executor: executor
    )
end

__END__
@@task.md
## Fehlende Zahl in einer Zahlenfolge finden

Gegeben ist ein Array von ganzen Zahlen, die eigentlich eine aufeinanderfolgende Zahlenfolge bilden.
Allerdings fehlt genau eine Zahl.

Implementiere eine Funktion `find_missing_number`, die die fehlende Zahl zurückgibt.

**Beispiel:**

<div class='table-responsive'>
<table class='table table-sm'>
<tr><th>nums</th><th></th><th>Rückgabewert</th></tr>
<tr><td>[4, 5, 7, 8]</td><td>→</td><td>6</td></tr>
<tr><td>[-2, -1, 0, 2]</td><td>→</td><td>1</td></tr>
<tr><td>[100, 104, 102, 101]</td><td>→</td><td>103</td></tr>
</table>
</div>

### Hinweise

- Das Array enthält mindestens 2 Zahlen.
- Die Zahlen können negativ sein.
- Die Reihenfolge der Zahlen im Array kann beliebig sein.
- Es fehlt genau eine Zahl.
- Es gibt keine Duplikate.

@@starter.rb
def find_missing_number(nums)
    #_
end

@@starter.py
def find_missing_number(nums):
    #_

@@starter.js
function find_missing_number(nums) {
    #_
}
