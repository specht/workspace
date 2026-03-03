require_relative "../include/single_file_task"

def build_judge(executor:, rng:)
    validator = Judge::Validator.new()

    cases = []
    cases << Judge::Case.new(args: [3, 4, 5], expect: Judge::Expect.equals(true))
    cases << Judge::Case.new(args: [1, 2, 3], expect: Judge::Expect.equals(false))
    cases << Judge::Case.new(args: [4, 5, 3], expect: Judge::Expect.equals(true))

    3.times do |i|
        scale = rng.rand(8..100)
        a = 3 * scale
        b = 4 * scale
        c = 5 * scale
        a, b, c = [a, b, c].shuffle
        cases << Judge::Case.new(args: [a, b, c], expect: Judge::Expect.equals(true))
    end

    3.times do |i|
        scale = rng.rand(8..100)
        a = 3 * scale
        b = 4 * scale
        c = 5 * scale
        a, b, c = [a, b, c].shuffle
        cases << Judge::Case.new(args: [a, b, c + rng.rand(1..200)], expect: Judge::Expect.equals(false))
    end

    Judge::FunctionTask.new(
        function_name: :pythagorean_triplet,
        cases: cases,
        validator: validator,
        executor: executor
    )
end

__END__
@@task.md
## Pythagoreisches Tripel überprüfen

Implementiere eine Funktion `pythagorean_triplet`, die überprüft, ob drei gegebene Zahlen ein pythagoreisches Tripel bilden. Ein pythagoreisches Tripel besteht aus drei positiven ganzen Zahlen a, b und c, die die Gleichung a² + b² = c² erfüllen. Achtung: Die Reihenfolge der Zahlen ist nicht festgelegt, d.h. es könnte auch a² + c² = b² oder b² + c² = a² sein.

**Beispiele:**

<div class='table-responsive'>
<table class='table table-sm'>
<tr><th>a</th><th>b</th><th>c</th><th>Rückgabewert</th></tr>
<tr><td>3</td><td>4</td><td>5</td><td>true</td></tr>
<tr><td>1</td><td>2</td><td>3</td><td>false</td></tr>
<tr><td>4</td><td>5</td><td>3</td><td>true</td></tr>
</table>
</div>

@@starter.rb
def pythagorean_triplet(a, b, c)
    #_
end

@@starter.py
def pythagorean_triplet(a, b, c):
    #_

@@starter.js
function pythagorean_triplet(a, b, c) {
    #_
}

@@meta.yaml
difficulty: 3