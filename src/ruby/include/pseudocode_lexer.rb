require 'rouge'

module Rouge
    module Lexers
        # The pseudocode of the Handbuch pages (```pseudocode blocks): English
        # keywords, function names, fields, ← and the comparisons, constants
        # such as empty and found, numbers, and German remarks in parentheses.
        # The Algo Stepper's code box colours the same tokens the same way
        # (algo_stepper.js, highlightPseudocode).
        class Pseudocode < RegexLexer
            title 'Pseudocode'
            desc 'The pseudocode of the Handbuch'
            tag 'pseudocode'

            KEYWORDS = %w[if else while for each in of return not and or repeat until break continue do then to from mod div].freeze
            CONSTANTS = %w[empty none null true false found done full error].freeze
            IDENT = /[\p{L}_][\p{L}\p{N}_]*/

            state :root do
                # a remark: three words or more in parentheses, no operators
                # (a tuple like (0, start) or (i - 1) is code)
                rule %r/\((?=[^()\n]*\s[^()\n]*\s)[^()\n+\-*\/=<>≤≥≠\[\]]*\)/, Comment::Single
                rule %r/←|≤|≥|≠|<|>|=|\+|-|\*|\/|%/, Operator
                rule %r/\d+(?:\.\d+)?/, Literal::Number::Integer
                rule %r/(\.)(#{IDENT})/ do
                    groups Punctuation, Name::Attribute
                end
                rule %r/not found\b/, Keyword::Constant
                # a call or definition takes its bracket with it, so that the
                # arguments are never taken for a remark
                rule %r/(#{IDENT})(\()/ do |m|
                    token KEYWORDS.include?(m[1]) ? Keyword : Name::Function, m[1]
                    token Punctuation, m[2]
                end
                rule IDENT do |m|
                    word = m[0]
                    if KEYWORDS.include?(word) then token Keyword
                    elsif CONSTANTS.include?(word) then token Keyword::Constant
                    else token Name
                    end
                end
                rule %r/[()\[\]{},:]/, Punctuation
                rule %r/\s+/, Text::Whitespace
                rule %r/./, Text
            end
        end
    end
end
