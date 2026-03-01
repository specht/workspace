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

**Beispiele:**

<div class='table-responsive'>
<table class='table table-sm'>
<tr><th>n</th><th></th><th>Rückgabewert</th></tr>
<tr><td>4</td><td>→</td><td>true</td></tr>
<tr><td>-3</td><td>→</td><td>false</td></tr>
<tr><td>0</td><td>→</td><td>true</td></tr>
<tr><td>-2</td><td>→</td><td>true</td></tr>
</table>
</div>

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

@@patch.rb
class Integer
    def even?
        raise "Use of Integer#even? is disabled for this exercise."
    end
end

@@meta.yaml
difficulty: 1