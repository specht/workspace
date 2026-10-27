require 'rouge'

module Rouge
    module Lexers
        # Grammars in EBNF (```ebnf blocks), as on »Grammatiken« and in the
        # calculator: nonterminals (Name::Class) and terminals (Str::Double)
        # look different, the way the syntax diagrams set them in square and
        # round boxes, and the signs of the notation (= | { } [ ] ( ) and ⇒ of
        # a derivation) are operators. So a quoted "(" that lands in the word
        # never looks like a bracket of the notation.
        class EBNF < RegexLexer
            title 'EBNF'
            desc 'Grammars in EBNF for the Workspace pages'
            tag 'ebnf'

            IDENT = /[\p{L}_][\p{L}\p{N}_-]*/

            state :root do
                rule %r/\(\*.*?\*\)/m, Comment::Multiline
                rule %r/"[^"\n]*"/, Str::Double
                rule %r/'[^'\n]*'/, Str::Single
                # the name a rule defines, at the start of its line
                rule %r/^([ \t]*)(#{IDENT})(?=[ \t]*(?:=|::=))/ do
                    groups Text::Whitespace, Name::Function
                end
                rule IDENT, Name::Class
                rule %r/::=|⇒|→|[=|{}\[\]()*+?,;.]/, Operator
                rule %r/…/, Punctuation
                rule %r/\s+/, Text::Whitespace
                rule %r/./, Text
            end
        end
    end
end
