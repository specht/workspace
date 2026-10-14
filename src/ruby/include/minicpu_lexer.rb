require 'rouge'

module Rouge
    module Lexers
        # MiniCPU assembly for ```minicpu blocks (CPU Sandbox, Handbuch
        # »MiniCPU«), with the categories of the sandbox's own editor mode
        # (web/js/editor.js): mnemonics, registers, directives, label
        # definitions, numbers, characters and strings, comments.
        # src/scripts/minicpu-reference.mjs checks MNEMONICS against the
        # sandbox's instruction table.
        class MiniCPU < RegexLexer
            title 'MiniCPU'
            desc 'MiniCPU assembly (CPU Sandbox)'
            tag 'minicpu'

            MNEMONICS = %w[
                NOP HLT CLF LDI LDA STA MOV ADD SUB ADC SBC INC DEC SHL SHR
                AND OR XOR CMP PUSH POP CALL RET JMP JZ JNZ JC JNC IN OUT
            ].freeze
            REGISTERS = %w[A B C D X Y XL XH YL YH SP PC].freeze

            state :root do
                rule %r/\s+/, Text::Whitespace
                rule %r/;.*$/, Comment::Single
                rule %r/"(?:[^"\\\n]|\\.)*"?/, Str::Double
                rule %r/'(?:\\.|[^'\\\n])'/, Str::Char
                # .cpu 8s: the level is one word, not 8 and s.
                rule %r/(\.cpu)(\s+)(8[sx]?)\b/i do
                    groups Comment::Preproc, Text::Whitespace, Num::Integer
                end
                rule %r/\.[a-z]+\b/i, Comment::Preproc
                rule %r/0x[\da-f_]+/i, Num::Hex
                rule %r/0b[01_]+/i, Num::Bin
                rule %r/\d+/, Num::Integer
                rule %r/([a-z_]\w*)(\s*)(:)/i do
                    groups Name::Label, Text::Whitespace, Punctuation
                end
                rule %r/[a-z_]\w*/i do |m|
                    word = m[0].upcase
                    if MNEMONICS.include?(word)
                        token Keyword
                    elsif REGISTERS.include?(word)
                        token Name::Builtin
                    else
                        token Name
                    end
                end
                rule %r/[\[\],]/, Punctuation
                rule %r/<<|>>|[-+*\/&|]/, Operator
                rule %r/./, Text
            end
        end
    end
end
