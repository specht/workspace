require_relative "../include/single_file_task"

def build_judge(executor:, rng:)
    validator = Judge::Validator.new()

    cases = []
    cases << Judge::Case.new(args: [4], expect: Judge::Expect.equals(3))
    cases << Judge::Case.new(args: [0], expect: Judge::Expect.equals(0))
    cases << Judge::Case.new(args: [1], expect: Judge::Expect.equals(1))
    cases << Judge::Case.new(args: [10], expect: Judge::Expect.equals(55))

    _fib = lambda do |n|
        return n if n <= 1
        _fib.call(n - 1) + _fib.call(n - 2)
    end

    3.times do |i|
        n = rng.rand(15..30)
        fib = _fib.call(n)
        cases << Judge::Case.new(args: [n], expect: Judge::Expect.equals(fib))
    end

    Judge::FunctionTask.new(
        function_name: :fibonacci,
        cases: cases,
        validator: validator,
        executor: executor
    )
end

__END__
@@task.md
## Fibonacci-Zahl berechnen

Implementiere eine Funktion `fibonacci`, die die n-te Fibonacci-Zahl berechnet.

Die Fibonacci-Zahlen sind definiert als:

- F(0) = 0
- F(1) = 1
- F(n) = F(n-1) + F(n-2) für n > 1

Die ersten Fibonacci-Zahlen sind also: 0, 1, 1, 2, 3, 5, 8, 13, 21, ...

**Beispiele:**

<div class='table-responsive'>
<table class='table table-sm'>
<tr><th>n</th><th></th><th>Rückgabewert</th></tr>
<tr><td>0</td><td>→</td><td>0</td></tr>
<tr><td>1</td><td>→</td><td>1</td></tr>
<tr><td>2</td><td>→</td><td>1</td></tr>
<tr><td>3</td><td>→</td><td>2</td></tr>
<tr><td>4</td><td>→</td><td>3</td></tr>
<tr><td>10</td><td>→</td><td>55</td></tr>
</table>
</div>

*Hinweis: Du kannst die n-te Fibonacci-Zahl iterativ ermitteln, indem du ein Array aller Fibonacci-Zahlen bis n Schritt für Schritt aufbaust.*

@@starter.rb
def fibonacci(n)
    #_
end

@@starter.py
def fibonacci(n):
    #_

@@starter.js
function fibonacci(n) {
    #_
}

@@meta.yaml
difficulty: 2