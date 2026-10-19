require 'rouge'

module Rouge
    module Lexers
        # Rooms and levels of the Backrooms (```room blocks; the starter's own
        # format, read by engine/level.js in specht/backrooms-starter):
        # sections in square brackets, »key = value« lines, and in [map] the
        # floor plan, where every character counts: # wall, . floor, @ start,
        # a letter a lamp, block or statue, a digit a door. A # starts a
        # comment only outside the floor plan; at the end of a line only
        # with a space before and after it, as engine/level.js reads it
        # (0.08, #5a4626 is a height and a colour).
        #
        # Blocks on a page often show a few rows of a floor plan without
        # their [map] header. Such a row is told from a comment by what it
        # is made of: only # . @ letters, digits and spaces, never two
        # letters in a row (a comment has words).
        class BackroomsRoom < RegexLexer
            title 'Backrooms-Raum'
            desc 'Rooms and levels of the Backrooms (rooms/NAME.room, level.ini)'
            tag 'backrooms-room'

            MAP_ROW = %r/^(?=[ \t]*[#.@0-9A-Za-z][#.@0-9A-Za-z \t]*$)(?![^\n]*[A-Za-z]{2})(?=[^\n]*[#.])/

            state :root do
                rule %r/^([ \t]*)(\[)(map)(\])([ \t]*)$/ do
                    groups Text::Whitespace, Punctuation, Name::Namespace, Punctuation, Text::Whitespace
                    push :map
                end
                rule %r/^([ \t]*)(\[)([^\]\n]+)(\])/ do
                    groups Text::Whitespace, Punctuation, Name::Namespace, Punctuation
                end
                rule MAP_ROW do
                    push :row
                end
                rule %r/^[ \t]*[#;].*$/, Comment::Single
                rule %r/^([ \t]*)([a-z_]+)([ \t]*)(=)([ \t]*)/ do
                    groups Text::Whitespace, Name::Attribute, Text::Whitespace, Punctuation, Text::Whitespace
                    push :value
                end
                rule %r/\n/, Text::Whitespace
                rule %r/[ \t]+/, Text::Whitespace
                rule %r/.+/, Text
            end

            # one row of a floor plan, outside a [map] section
            state :row do
                mixin :cells
                rule %r/\n/, Text::Whitespace, :pop!
            end

            # [map] up to the next section: every line is a row
            state :map do
                rule %r/^(?=[ \t]*\[)/, Text, :pop!
                mixin :cells
                rule %r/\n/, Text::Whitespace
            end

            state :cells do
                rule %r/#+/, Keyword                # Wand
                rule %r/\.+/, Punctuation           # Boden
                rule %r/@/, Name::Label             # hier fängst du an
                rule %r/[0-9]/, Num::Integer        # Tür
                rule %r/[A-Za-z]/, Name::Constant   # Leuchte, Block oder Statue
                rule %r/[ \t]+/, Text::Whitespace   # nichts
            end

            state :value do
                rule %r/[ \t]+#(?=[ \t]|$).*$/, Comment::Single
                rule %r/#[0-9a-fA-F]{6}\b|#[0-9a-fA-F]{3}\b/, Num::Hex
                rule %r/-?\d+(\.\d+)?/, Num::Float
                rule %r/[A-Za-z_][\w.\/-]*/, Str
                rule %r/,/, Punctuation
                rule %r/[ \t]+/, Text::Whitespace
                rule %r/\n/, Text::Whitespace, :pop!
                rule %r/./, Text
            end
        end
    end
end
