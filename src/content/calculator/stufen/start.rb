# Ein Rechner für Ausdrücke

# Zerlegt einen Ausdruck in Tokens: Zahlen, Rechenzeichen und Klammern.
def tokenize(text)
    []
end

# Macht aus Tokens einen Baum.
def parse(tokens)
    tokens.first
end

# Rechnet einen Baum aus.
def evaluate(tree, variables = {})
    tree
end

def calculate(text, variables = {})
    evaluate(parse(tokenize(text)), variables)
end

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
        p calculate(line)
    rescue ArgumentError => e
        puts "Fehler: #{e.message}"
    end
end
