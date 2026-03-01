require_relative "../include/single_file_task"

def build_judge(executor:, rng:)
    validator = Judge::Validator.new()

    cases = []
    cases << Judge::Case.new(args: ["asdf"], expect: Judge::Expect.equals("fdsa"))
    cases << Judge::Case.new(args: ["racecar"], expect: Judge::Expect.equals("racecar"))
    cases << Judge::Case.new(args: ["STRESSED"], expect: Judge::Expect.equals("DESSERTS"))

    3.times do |i|
        str = (0...rng.rand(5..15)).map { (65 + rng.rand(26)).chr }.join
        cases << Judge::Case.new(args: [str], expect: Judge::Expect.equals(str.reverse))
    end

    Judge::FunctionTask.new(
        function_name: :reverse_string,
        cases: cases,
        validator: validator,
        executor: executor
    )
end

__END__
@@task.md
## String umkehren

Implementiere eine Funktion `reverse_string`, die einen gegebenen String umkehrt.

**Beispiele:**

<div class='table-responsive'>
<table class='table table-sm'>
<tr><th>s</th><th></th><th>Rückgabewert</th></tr>
<tr><td>asdf</td><td>→</td><td>fdsa</td></tr>
<tr><td>racecar</td><td>→</td><td>racecar</td></tr>
<tr><td>stressed</td><td>→</td><td>desserts</td></tr>
</table>
</div>

@@starter.rb
def reverse_string(s)
    #_
end

@@starter.py
def reverse_string(s):
    #_

@@starter.js
function reverse_string(s) {
    #_
end

@@patch.rb
class String
    def reverse
        raise "Use of String#reverse is disabled for this exercise."
    end
end

@@meta.yaml
difficulty: 1
