# Nur für die Karte: welche Knoten bei einer Suche in welcher Reihenfolge
# dran waren. Deine Suchen rufen visit(node) auf, wenn ein Knoten dran ist;
# der Server und die Prüfungen lesen die Reihenfolge hier ab. Für die Suche
# selbst tut visit nichts. Hier musst du nichts ändern.
VISITS = []

# Nur für die Karte: zeigt, dass dieser Knoten dran ist
def visit(node)
    VISITS << node
end

# Die Knoten, die dran waren, während der Block lief, in ihrer Reihenfolge
def visits_of
    VISITS.clear
    yield
    VISITS.dup
end
