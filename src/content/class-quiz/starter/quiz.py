# Das Quiz der Klasse: Es spielt die Runden aus rounds.txt, jede Runde die
# Fragen einer Datei in questions/.
#
#   python3 quiz.py                      alle Runden, in der Reihenfolge von rounds.txt
#   python3 quiz.py questions/lena.txt   nur diese Datei, zum Ausprobieren
#   python3 quiz.py --check              prüft rounds.txt und alle Fragen
#
# Eine Datei mit Fragen:
#
#   # Fragen von Lena
#   ? Wie viele Bits hat ein Byte?
#   = 8
#
#   ? Welche Farbe entsteht im RGB-Modell aus Rot und Grün?
#   = Gelb
#
# Eine Frage beginnt mit »? «, die Antwort darunter mit »= «. Gelten mehrere
# Antworten, stehen sie durch » | « getrennt: »= 8 | acht«.

import os
import sys

CONFLICT = ('<<<<<<<', '=======', '>>>>>>>')


def read_lines(path):
    with open(path, encoding='utf-8') as f:
        return f.read().splitlines()


def read_questions(path):
    """Die Fragen einer Datei als Liste von (Frage, [Antworten]), dazu die
    Probleme, die in der Datei stehen."""
    questions = []
    problems = []
    for i, line in enumerate(read_lines(path)):
        where = f"{path}, Zeile {i + 1}"
        if line.strip() == '' or line.startswith('#'):
            continue
        elif line.startswith(CONFLICT):
            problems.append(f"{where}: Hier ist noch ein Konflikt nicht aufgelöst ({line[0:7]}).")
        elif line.startswith('? '):
            if questions and not questions[-1][1]:
                problems.append(f"{where}: Die Frage davor hat keine Antwort.")
            questions.append((line[2:].strip(), []))
        elif line.startswith('= '):
            if not questions or questions[-1][1]:
                problems.append(f"{where}: Zu dieser Antwort gehört keine Frage.")
            else:
                questions[-1][1].extend(a.strip() for a in line[2:].split('|') if a.strip())
        else:
            problems.append(f"{where}: Eine Zeile beginnt mit »? « (Frage), »= « (Antwort) oder »#« (Kommentar).")
    if questions and not questions[-1][1]:
        problems.append(f"{path}: Die letzte Frage hat keine Antwort.")
    if not questions:
        problems.append(f"{path}: Hier steht noch keine Frage.")
    return questions, problems


def read_rounds(root):
    """Die Dateien aus rounds.txt, dazu die Probleme in rounds.txt."""
    rounds = []
    problems = []
    for i, line in enumerate(read_lines(os.path.join(root, 'rounds.txt'))):
        where = f"rounds.txt, Zeile {i + 1}"
        name = line.strip()
        if name == '' or name.startswith('#'):
            continue
        elif name.startswith(CONFLICT):
            problems.append(f"{where}: Hier ist noch ein Konflikt nicht aufgelöst ({name[0:7]}).")
        elif name in rounds:
            problems.append(f"{where}: {name} steht schon weiter oben.")
        elif not os.path.isfile(os.path.join(root, name)):
            problems.append(f"{where}: Die Datei {name} gibt es nicht.")
        else:
            rounds.append(name)
    return rounds, problems


def problems_in(root):
    """Alles, was am Quiz noch nicht stimmt; leer, wenn alles stimmt."""
    rounds, problems = read_rounds(root)
    prefix = os.path.join(root, '')
    for name in rounds:
        problems.extend(p.replace(prefix, '', 1) for p in read_questions(os.path.join(root, name))[1])
    folder = os.path.join(root, 'questions')
    if os.path.isdir(folder):
        for file in sorted(os.listdir(folder)):
            name = f"questions/{file}"
            if file.endswith('.txt') and name not in rounds:
                problems.append(f"{name} steht nicht in rounds.txt.")
    return problems


def same_answer(given, answers):
    return any(given.strip().lower() == answer.lower() for answer in answers)


def play(paths):
    """Spielt die Fragen der Dateien und gibt die Punkte zurück."""
    points = 0
    total = 0
    for path in paths:
        print(f"\n== {os.path.basename(path)[:-4]} ==")
        for question, answers in read_questions(path)[0]:
            given = input(f"{question} ")
            total = total + 1
            if same_answer(given, answers):
                points = points + 1
                print('Richtig!')
            else:
                print(f"Leider nein: {answers[0]}")
    print(f"\n{points} von {total} Punkten.")
    return points


if __name__ == '__main__':
    root = os.path.dirname(os.path.abspath(__file__))
    if sys.argv[1:] == ['--check']:
        problems = problems_in(root)
        if problems:
            print('\n'.join(problems))
            sys.exit(1)
        rounds = read_rounds(root)[0]
        count = sum(len(read_questions(os.path.join(root, name))[0]) for name in rounds)
        print(f"Alles in Ordnung: {len(rounds)} {'Runde' if len(rounds) == 1 else 'Runden'} mit {count} {'Frage' if count == 1 else 'Fragen'}.")
    elif len(sys.argv) == 2:
        questions, problems = read_questions(sys.argv[1])
        if problems:
            sys.exit('\n'.join(problems))
        play([sys.argv[1]])
    else:
        problems = problems_in(root)
        if problems:
            sys.exit('Erst muss alles stimmen (python3 quiz.py --check):\n' + '\n'.join(problems))
        play([os.path.join(root, name) for name in read_rounds(root)[0]])
