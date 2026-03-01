require_relative "../include/single_file_task"

def build_judge(executor:, rng:)
    validator = Judge::Validator.new()

    cases = []
    cases << Judge::Case.new(args: [4, 8], expect: Judge::Expect.equals(12))
    cases << Judge::Case.new(args: [-3, 7], expect: Judge::Expect.equals(4))
    cases << Judge::Case.new(args: [0, 0], expect: Judge::Expect.equals(0))
    3.times do |i|
        a = rng.rand(-(10 ** (i + 1))..(10 ** (i + 1)))
        b = rng.rand(-(10 ** (i + 1))..(10 ** (i + 1)))
        cases << Judge::Case.new(args: [a, b], expect: Judge::Expect.equals(a + b))
    end

    Judge::FunctionTask.new(
        function_name: :add_numbers,
        cases: cases,
        validator: validator,
        executor: executor
    )
end

__END__
@@task.md
## Summe zweier Zahlen berechnen

Implementiere eine Funktion `add_numbers`, die die Summe von zwei Zahlen berechnet.

**Beispiele:**

<div class='table-responsive'>
<table class='table table-sm'>
<tr><th>a</th><th>b</th><th></th><th>Rückgabewert</th></tr>
<tr><td>4</td><td>8</td><td>→</td><td>12</td></tr>
<tr><td>-3</td><td>7</td><td>→</td><td>4</td></tr>
<tr><td>0</td><td>0</td><td>→</td><td>0</td></tr>
<tr><td>-2</td><td>2</td><td>→</td><td>0</td></tr>
</table>
</div>

@@starter.rb
def add_numbers(a, b)
    #_
end

@@starter.py
def add_numbers(a, b):
    #_

@@starter.js
function add_numbers(a, b) {
    #_
}
