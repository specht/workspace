require_relative "../include/single_file_task"

def build_judge(executor:, rng:)
    validator = Judge::Validator.new()

    cases = []
    cases << Judge::Case.new(args: [4], expect: Judge::Expect.equals(4 * 3 * 2 * 1))
    cases << Judge::Case.new(args: [7], expect: Judge::Expect.equals(7 * 6 * 5 * 4 * 3 * 2 * 1))
    cases << Judge::Case.new(args: [1], expect: Judge::Expect.equals(1))
    cases << Judge::Case.new(args: [0], expect: Judge::Expect.equals(1))

    numbers = (10...20).to_a.sample(3)

    numbers.each do |n|
        f = (1..n).reduce(1, :*)
        cases << Judge::Case.new(args: [n], expect: Judge::Expect.equals(f))
    end

    Judge::FunctionTask.new(
        function_name: :factorial,
        cases: cases,
        validator: validator,
        executor: executor
    )
end

__END__
@@task.md
## Fakultät berechnen

Implementiere eine Funktion `factorial`, die die Fakultät einer Zahl berechnet.
Die Fakultät einer Zahl n (geschrieben als n!) ist das Produkt aller positiven ganzen Zahlen von 1 bis n. Zum Beispiel:

- 4! = 4 × 3 × 2 × 1 = 24
- 7! = 7 × 6 × 5 × 4 × 3 × 2 × 1 = 5040

**Beispiele:**

<div class='table-responsive'>
<table class='table table-sm'>
<tr><th>n</th><th></th><th>Rückgabewert</th></tr>
<tr><td>5</td><td>→</td><td>120</td></tr>
<tr><td>6</td><td>→</td><td>720</td></tr>
</table>
</div>

*Achtung: Die Fakultät von 0 ist per Definition 1.*

@@starter.rb
def factorial(n)
    #_
end

@@starter.py
def factorial(n):
    #_

@@starter.js
function factorial(n) {
    #_
}
