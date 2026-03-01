require_relative "../include/single_file_task"

def build_judge(executor:, rng:)
    validator = Judge::Validator.new()

    cases = []
    cases << Judge::Case.new(args: [4, 7], expect: Judge::Expect.equals(7))
    cases << Judge::Case.new(args: [10, 5], expect: Judge::Expect.equals(10))
    cases << Judge::Case.new(args: [-3, -1], expect: Judge::Expect.equals(-1))
    cases << Judge::Case.new(args: [0, 0], expect: Judge::Expect.equals(0))

    3.times do |i|
        a = rng.rand(-(10 ** (i + 1))..(10 ** (i + 1)))
        b = rng.rand(-(10 ** (i + 1))..(10 ** (i + 1)))
        cases << Judge::Case.new(args: [a, b], expect: Judge::Expect.equals([a, b].max))
    end

    Judge::FunctionTask.new(
        function_name: :max,
        cases: cases,
        validator: validator,
        executor: executor
    )
end

__END__
@@task.md
## Maximum von zwei Zahlen bestimmen

Implementiere eine Funktion `max`, die die größere von zwei Zahlen zurückgibt.

**Beispiel:**

<table>
<tr><td rowspan="2">Eingabe:</td><td>a</td><td>=</td><td>4</td></tr>
<tr><td>b</td><td>=</td><td>7</td></tr>
<tr><td>Ausgabe:</td><td colspan="3">7</td></tr>
</table>

Die Funktion gibt 7 zurück, da 7 größer als 4 ist.

@@starter.rb
def max(a, b)
    #_
end

@@starter.py
def max(a, b):
    #_

@@starter.js
function max(a, b) {
    #_
}

@@patch.rb
class Array
    def max
        raise "Use of Array#max is disabled for this exercise."
    end
    def sort
        raise "Use of Array#sort is disabled for this exercise."
    end
    def sort!
        raise "Use of Array#sort! is disabled for this exercise."
    end
end

@@patch.py
import builtins
def _sorted(a, b):
    raise RuntimeError("Use of sorted() is disabled for this exercise.")
builtins.sorted = _sorted

@@patch.js
/*
Array.prototype.max = function() {
    throw new Error("Use of Array.prototype.max is disabled for this exercise.");
};
Array.prototype.sort = function() {
    throw new Error("Use of Array.prototype.sort is disabled for this exercise.");
};
*/
