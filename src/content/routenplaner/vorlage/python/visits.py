# Nur für die Karte: welche Knoten bei einer Suche in welcher Reihenfolge
# dran waren. Deine Suchen rufen visit(node) auf, wenn ein Knoten dran ist;
# der Server und die Prüfungen lesen die Reihenfolge hier ab. Für die Suche
# selbst tut visit nichts. Hier musst du nichts ändern.
visits = []


# Nur für die Karte: zeigt, dass dieser Knoten dran ist
def visit(node):
    visits.append(node)


# Die Knoten, die dran waren, während search lief, in ihrer Reihenfolge
def visits_of(search):
    visits.clear()
    search()
    return list(visits)
