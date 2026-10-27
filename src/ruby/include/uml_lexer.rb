require 'rouge'

module Rouge
    module Lexers
        # Class diagrams as text (```uml blocks), the language of the
        # UML-Werkstatt (src/vscode-extensions/uml): keywords, class names, the
        # signs of visibility, members, types, multiplicities and the lines of
        # an association.
        class UML < RegexLexer
            title 'UML-Klassendiagramm'
            desc 'Class diagrams of the UML-Werkstatt (.uml)'
            tag 'uml'
            filenames '*.uml'

            IDENT = /[\p{L}_][\p{L}\p{N}_]*/
            TYPES = %w[int double long float boolean char String void List Map Set].freeze

            state :root do
                rule %r/#.*$/, Comment::Single
                rule %r/^(class)([ \t]+)(#{IDENT})/ do
                    groups Keyword, Text::Whitespace, Name::Class
                    push :header
                end
                rule %r/^(association)([ \t]+)(#{IDENT})([ \t]*)(:)/ do
                    groups Keyword, Text::Whitespace, Name::Function, Text::Whitespace, Punctuation
                    push :association
                end
                rule %r/^([ \t]+)([+-])([ \t]*)(#{IDENT}[?!]?)/ do
                    groups Text::Whitespace, Operator, Text::Whitespace, Name::Attribute
                    push :member
                end
                # Newlines alone: indentation must reach the member rule.
                rule %r/\n/, Text::Whitespace
                rule %r/[ \t]+/, Text::Whitespace
                rule %r/./, Text
            end

            state :header do
                rule %r/\n/, Text::Whitespace, :pop!
                rule %r/#.*$/, Comment::Single
                rule %r/\bextends\b/, Keyword
                rule %r/\(abstract\)/, Keyword::Pseudo
                rule IDENT, Name::Class
                rule %r/[ \t]+/, Text::Whitespace
                rule %r/./, Text
            end

            state :member do
                rule %r/\n/, Text::Whitespace, :pop!
                rule %r/#.*$/, Comment::Single
                rule %r/\b(?:#{TYPES.join('|')})\b/, Keyword::Type
                rule %r/\b[A-Z]\p{L}*\b/, Name::Class
                rule IDENT, Name::Variable
                rule %r/[(),:<>\[\]]/, Punctuation
                rule %r/[ \t]+/, Text::Whitespace
                rule %r/./, Text
            end

            state :association do
                rule %r/\n/, Text::Whitespace, :pop!
                rule %r/#.*$/, Comment::Single
                rule %r/(?:\d+\.\.(?:\d+|\*)|\*|\d+)/, Literal::Number
                rule %r/--|->/, Operator
                rule IDENT, Name::Class
                rule %r/[ \t]+/, Text::Whitespace
                rule %r/./, Text
            end
        end
    end
end
