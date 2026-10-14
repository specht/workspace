require 'rouge'

module Rouge
    module Lexers
        # Anaglyph scenes (```ini blocks; the viewer's own format, read by
        # include/script.js in the anaglyph repository): »key = value« lines,
        # whole-line comments with ; or #, group and end on their own. Rouge's
        # INI lexer took a colour code such as #4aa03f for a comment and then
        # misread the next line.
        class AnaglyphScene < RegexLexer
            title 'Anaglyph-Szene'
            desc 'Scenes of the Anaglyph viewer (scene.ini)'
            tag 'anaglyph-scene'

            state :root do
                rule %r/^[ \t]*[;#].*$/, Comment::Single
                rule %r/^([ \t]*)(group|end)([ \t]*)$/ do
                    groups Text::Whitespace, Keyword, Text::Whitespace
                end
                rule %r/^([ \t]*)([^\s=]+)([ \t]*)(=)([ \t]*)/ do
                    groups Text::Whitespace, Name::Property, Text::Whitespace, Punctuation, Text::Whitespace
                    push :value
                end
                rule %r/\n/, Text::Whitespace
                rule %r/[ \t]+/, Text::Whitespace
                rule %r/./, Text
            end

            state :value do
                rule %r/\n/, Text::Whitespace, :pop!
                rule %r/#\h+\b/, Literal::String
                rule %r/-?(?:\d+\.?\d*|\.\d+)/, Literal::Number
                rule %r/,/, Punctuation
                rule %r/[ \t]+/, Text::Whitespace
                rule %r/[^\s,]+/, Literal::String
            end
        end
    end
end
