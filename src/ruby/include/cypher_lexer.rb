require 'rouge'

module Rouge
    module Lexers
        # Cypher for ```cypher blocks, made for reading patterns: labels
        # (Name::Class) look like entities in the ER-Werkstatt, relationship
        # types (Name::Function) like relationships, property keys
        # (Name::Attribute) like attributes. Rouge's own Cypher lexer gives
        # `m:Movie` and `:Genre` different classes and labels and types the
        # same one, which is what a beginner needs to tell apart.
        class WorkspaceCypher < RegexLexer
            title 'Cypher'
            desc 'Cypher for the Workspace pages (Neo4j)'
            tag 'workspace-cypher'

            IDENT = /[\p{L}_][\p{L}\p{N}_]*|`[^`]+`/
            KEYWORDS = %w[
                match optional where return with order by limit skip distinct as
                and or xor not in is null true false starts ends contains
                create merge delete detach set remove on unwind call yield
                case when then else end union all use
                constraint constraints index indexes for require node key unique
                if exists show drop asc desc ascending descending
            ].freeze

            # What may appear anywhere: inside patterns, maps and expressions.
            state :expression do
                rule %r{//.*$}, Comment::Single
                rule %r/'(?:\\.|[^'\\])*'/, Str::Single
                rule %r/"(?:\\.|[^"\\])*"/, Str::Double
                rule %r/\d+(?:\.\d+)?(?:[eE][+-]?\d+)?/, Num
                rule %r/(\.)(#{IDENT})/ do
                    groups Punctuation, Name::Attribute
                end
                rule %r/(#{IDENT})([ \t]*)(\()/ do |m|
                    if KEYWORDS.include?(m[1].downcase)
                        token Keyword, m[1]
                        token Text::Whitespace, m[2]
                        token Punctuation, m[3]
                        push :node
                    else
                        groups Name::Builtin, Text::Whitespace, Punctuation
                        push :arguments
                    end
                end
                rule IDENT do |m|
                    token(KEYWORDS.include?(m[0].downcase) ? Keyword : Name)
                end
                rule %r/->|<-|<>|<=|>=|=~|\.\.|[-+*\/%=<>|^]/, Operator
                rule %r/[,;]/, Punctuation
                rule %r/\s+/, Text::Whitespace
            end

            state :root do
                rule %r/\(/, Punctuation, :node
                rule %r/\[/, Punctuation, :relationship
                rule %r/\{/, Punctuation, :map
                rule %r/[)\]}]/, Punctuation
                mixin :expression
                rule %r/./, Text
            end

            # (n:Label:Other {key: value})
            state :node do
                rule %r/\)/, Punctuation, :pop!
                rule %r/(:)([ \t]*)(#{IDENT})/ do
                    groups Punctuation, Text::Whitespace, Name::Class
                end
                rule %r/\(/, Punctuation, :node
                rule %r/\[/, Punctuation, :relationship
                rule %r/\{/, Punctuation, :map
                mixin :expression
                rule %r/./, Text
            end

            # [r:TYPE|OTHER *1..3 {key: value}]
            state :relationship do
                rule %r/\]/, Punctuation, :pop!
                rule %r/(:|\|:?)([ \t]*)(#{IDENT})/ do |m|
                    token(m[1] == ':' ? Punctuation : Operator, m[1])
                    token Text::Whitespace, m[2]
                    token Name::Function, m[3]
                end
                rule %r/\{/, Punctuation, :map
                mixin :expression
                rule %r/./, Text
            end

            # {key: value, …}
            state :map do
                rule %r/\}/, Punctuation, :pop!
                rule %r/(#{IDENT})([ \t]*)(:)/ do
                    groups Name::Attribute, Text::Whitespace, Punctuation
                end
                rule %r/\(/, Punctuation, :node
                rule %r/\[/, Punctuation, :relationship
                rule %r/\{/, Punctuation, :map
                mixin :expression
                rule %r/./, Text
            end

            # count(m), shortestPath((a)-[*..8]-(b)): the arguments may hold
            # patterns, so a nested ( is a node again.
            state :arguments do
                rule %r/\)/, Punctuation, :pop!
                rule %r/\(/, Punctuation, :node
                rule %r/\[/, Punctuation, :relationship
                rule %r/\{/, Punctuation, :map
                mixin :expression
                rule %r/./, Text
            end
        end
    end
end
