require 'rouge'

module Rouge
    module Lexers
        # ER models as text (```er blocks), the same tokens as the ER-Werkstatt's
        # TextMate grammar (src/vscode-extensions/er/syntaxes/er.tmLanguage.json):
        # keywords, entity and relationship names, cardinalities, attribute
        # names, types and flags.
        class ER < RegexLexer
            title 'ER-Modell'
            desc 'ER models of the ER-Werkstatt (.er)'
            tag 'er'
            filenames '*.er'

            IDENT = /[\p{L}_][\p{L}\p{N}_]*/
            TYPES = %w[int text varchar decimal float date datetime bool].freeze

            state :root do
                rule %r/#.*$/, Comment::Single
                rule %r/^(entity)([ \t]+)(#{IDENT})/ do
                    groups Keyword, Text::Whitespace, Name::Class
                end
                rule %r/^(relationship)([ \t]+)(#{IDENT})([ \t]*)(:)/ do
                    groups Keyword, Text::Whitespace, Name::Function, Text::Whitespace, Punctuation
                    push :relationship
                end
                rule %r/^([ \t]+)(#{IDENT})([ \t]*)(:)?/ do
                    groups Text::Whitespace, Name::Attribute, Text::Whitespace, Punctuation
                    push :attribute
                end
                # Newlines alone: indentation must reach the attribute rule.
                rule %r/\n/, Text::Whitespace
                rule %r/[ \t]+/, Text::Whitespace
                rule %r/./, Text
            end

            state :relationship do
                rule %r/\n/, Text::Whitespace, :pop!
                rule %r/#.*$/, Comment::Single
                rule %r/\b[1nm]\s*:\s*[1nm]\b/, Literal::Number
                rule %r/\bas\b/, Keyword
                rule %r/,/, Punctuation
                rule IDENT, Name::Class
                rule %r/[ \t]+/, Text::Whitespace
                rule %r/./, Text
            end

            state :attribute do
                rule %r/\n/, Text::Whitespace, :pop!
                rule %r/#.*$/, Comment::Single
                rule %r/\b(?:#{TYPES.join('|')})\b/, Keyword::Type
                rule %r/\b(?:primary[ \t]+key|key)\b/, Keyword::Pseudo
                rule %r/\b(?:optional|unique)\b/, Keyword::Reserved
                rule %r/\d+/, Literal::Number
                rule %r/[,()]/, Punctuation
                rule %r/[ \t]+/, Text::Whitespace
                rule %r/./, Text
            end
        end
    end
end
