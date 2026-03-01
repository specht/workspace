require_relative "../include/single_file_task"

def build_judge(executor:, rng:)
    validator = Judge::Validator.new()

    cases = []
    cases << Judge::Case.new(args: [4], expect: Judge::Expect.equals(true))
    cases << Judge::Case.new(args: [17], expect: Judge::Expect.equals(false))
    cases << Judge::Case.new(args: [0], expect: Judge::Expect.equals(true))
    cases << Judge::Case.new(args: [-2], expect: Judge::Expect.equals(true))

    3.times do |i|
        n = rng.rand(-1000..1000)
        cases << Judge::Case.new(args: [n], expect: Judge::Expect.equals(n.even?))
    end

    Judge::FunctionTask.new(
        function_name: :is_even,
        cases: cases,
        validator: validator,
        executor: executor
    )
end

__END__
@@task.md
## Gerade Zahl erkennen

Implementiere eine Funktion `is_even`, die überprüft, ob eine Zahl gerade ist.

**Beispiel:**

<table>
<tr><td rowspan="1">Eingabe:</td><td>n</td><td>=</td><td>4</td></tr>
<tr><td>Ausgabe:</td><td colspan="3">true</td></tr>
</table>

Die Funktion gibt `true` zurück, weil 4 ohne Rest durch 2 teilbar ist.

**Beispiel:**

<table>
<tr><td rowspan="1">Eingabe:</td><td>n</td><td>=</td><td>17</td></tr>
<tr><td>Ausgabe:</td><td colspan="3">false</td></tr>
</table>

Die Funktion gibt `false` zurück, weil 17 nicht ohne Rest durch 2 teilbar ist.

@@starter.rb
def is_even(n)
    #_
end

@@starter.py
def is_even(n):
    #_

@@starter.js
function is_even(n) {
    #_
}
