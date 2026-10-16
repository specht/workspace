# Ein Rechner für Ausdrücke: Musterlösung zu allen Abschnitten.

# --- Zerlegen ----------------------------------------------------------------

# Zerlegt einen Ausdruck in Tokens: Zahlen als Integer oder Float, Namen,
# Rechenzeichen und Klammern als Strings mit einem Zeichen.
def tokenize(text)
    tokens = []
    i = 0
    while i < text.length
        c = text[i]
        if c == ' '
            i += 1
        elsif c =~ /[0-9]/
            start = i
            i += 1 while i < text.length && text[i] =~ /[0-9]/
            if i + 1 < text.length && text[i] == '.' && text[i + 1] =~ /[0-9]/
                i += 1
                i += 1 while i < text.length && text[i] =~ /[0-9]/
                tokens << text[start...i].to_f
            else
                tokens << text[start...i].to_i
            end
        elsif c =~ /[a-z_]/i
            start = i
            i += 1 while i < text.length && text[i] =~ /[a-z_0-9]/i
            tokens << text[start...i]
        elsif '+-*/^()'.include?(c)
            tokens << c
            i += 1
        else
            raise ArgumentError, "Unbekanntes Zeichen »#{c}« an Stelle #{i + 1}"
        end
    end
    tokens
end

# --- Ein Baum aus Tokens -----------------------------------------------------
#
#   expression = term { ("+" | "-") term }
#   term       = factor { ("*" | "/") factor }
#   factor     = "-" factor | power
#   power      = atom [ "^" factor ]
#   atom       = Zahl | Name | "(" expression ")"

#
# Jede Methode nimmt vorn aus tokens, was zu ihr gehört, und gibt den Baum
# dafür zurück. tokens.first ist das nächste Token, tokens.shift nimmt es.

def parse(tokens)
    tokens = tokens.dup
    tree = expression(tokens)
    raise ArgumentError, "Unerwartet: #{tokens.first.inspect}" unless tokens.empty?

    tree
end

def expression(tokens)
    tree = term(tokens)
    tree = [tokens.shift, tree, term(tokens)] while ['+', '-'].include?(tokens.first)
    tree
end

def term(tokens)
    tree = factor(tokens)
    tree = [tokens.shift, tree, factor(tokens)] while ['*', '/'].include?(tokens.first)
    tree
end

def factor(tokens)
    if tokens.first == '-'
        tokens.shift
        return ['neg', factor(tokens)]
    end
    power(tokens)
end

def power(tokens)
    base = atom(tokens)
    return base unless tokens.first == '^'

    tokens.shift
    ['^', base, factor(tokens)]
end

def atom(tokens)
    token = tokens.shift
    if token.is_a?(Numeric) || name?(token)
        token
    elsif token == '('
        tree = expression(tokens)
        raise ArgumentError, 'Es fehlt eine schließende Klammer' unless tokens.shift == ')'

        tree
    elsif token.nil?
        raise ArgumentError, 'Der Ausdruck hört zu früh auf'
    else
        raise ArgumentError, "Unerwartet: #{token.inspect}"
    end
end

def name?(token)
    token.is_a?(String) && token.match?(/\A[a-z_]/i)
end

# --- Ausrechnen ---------------------------------------------------------------

def evaluate(tree, variables = {})
    return tree if tree.is_a?(Numeric)

    if name?(tree)
        raise ArgumentError, "Unbekannter Name: #{tree}" unless variables.key?(tree)

        return variables[tree]
    end

    op, *args = tree
    values = args.map { |arg| evaluate(arg, variables) }
    case op
    when '+' then values[0] + values[1]
    when '-' then values[0] - values[1]
    when '*' then values[0] * values[1]
    when '/' then values[0].to_f / values[1]
    when '^' then values[0]**values[1]
    when 'neg' then -values[0]
    end
end

def calculate(text, variables = {})
    evaluate(parse(tokenize(text)), variables)
end

variables = {}
loop do
    print '> '
    line = gets
    if line.nil?
        puts
        break
    end

    line = line.strip
    next if line.empty?

    begin
        if (m = line.match(/\A([a-z_][a-z_0-9]*)\s*=(.*)\z/i))
            variables[m[1]] = calculate(m[2], variables)
            puts "#{m[1]} = #{variables[m[1]]}"
        else
            puts calculate(line, variables)
        end
    rescue ArgumentError, ZeroDivisionError => e
        puts "Fehler: #{e.message}"
    end
end
