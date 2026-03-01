require_relative "../include/single_file_task"

def build_judge(executor:, rng:)
    validator = Judge::Validator.new()

    cases = []
    cases << Judge::Case.new(args: ["aeiou"], expect: Judge::Expect.equals(5))
    cases << Judge::Case.new(args: ["bcdfg"], expect: Judge::Expect.equals(0))
    cases << Judge::Case.new(args: ["The rain in Spain stays mainly in the plain."], expect: Judge::Expect.equals(13))
    cases << Judge::Case.new(args: [""], expect: Judge::Expect.equals(0))
    cases << Judge::Case.new(args: ["The quick brown fox jumps over the lazy dog"], expect: Judge::Expect.equals(11))

    3.times do |i|
        str = (0...rng.rand(5..40)).map { (['A'.ord, 'a'.ord].sample + rng.rand(26)).chr }.join
        cases << Judge::Case.new(args: [str], expect: Judge::Expect.equals(str.count("aeiouAEIOU")))
    end

    Judge::FunctionTask.new(
        function_name: :count_vowels,
        cases: cases,
        validator: validator,
        executor: executor
    )
end

__END__
@@task.md
## Vokale zählen

Implementiere eine Funktion `count_vowels`, die die Anzahl der Vokale in einem gegebenen String zählt. Als Vokale gelten die Buchstaben a, e, i, o und u (sowohl in Groß- als auch in Kleinbuchstaben).

**Beispiele:**

<div class='table-responsive'>
<table class='table table-sm'>
<tr><th>s</th><th></th><th>Rückgabewert</th></tr>
<tr><td>aeiou</td><td>→</td><td>5</td></tr>
<tr><td>bcdfg</td><td>→</td><td>0</td></tr>
<tr><td>Why so serious?</td><td>→</td><td>6</td></tr>
</table>
</div>

@@starter.rb
def count_vowels(s)
    #_
end

@@starter.py
def count_vowels(s):
    #_

@@starter.js
function count_vowels(s) {
    #_
}

@@meta.yaml
difficulty: 1
