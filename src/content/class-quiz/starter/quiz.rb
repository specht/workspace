# Das Quiz der Klasse: Es spielt die Runden aus rounds.txt, jede Runde die
# Fragen einer Datei in questions/.
#
#   ruby quiz.rb                      alle Runden, in der Reihenfolge von rounds.txt
#   ruby quiz.rb questions/lena.txt   nur diese Datei, zum Ausprobieren
#   ruby quiz.rb --check              prüft rounds.txt und alle Fragen
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

CONFLICT = /\A(<<<<<<<|=======|>>>>>>>)/

# Die Fragen einer Datei als Liste von [Frage, [Antworten]], dazu die
# Probleme, die in der Datei stehen.
def read_questions(path)
  questions = []
  problems = []
  File.readlines(path, chomp: true).each_with_index do |line, i|
    where = "#{path}, Zeile #{i + 1}"
    if line.strip.empty? || line.start_with?('#')
      next
    elsif line.match?(CONFLICT)
      problems << "#{where}: Hier ist noch ein Konflikt nicht aufgelöst (#{line[0, 7]})."
    elsif line.start_with?('? ')
      problems << "#{where}: Die Frage davor hat keine Antwort." if questions.any? && questions.last[1].empty?
      questions << [line[2..].strip, []]
    elsif line.start_with?('= ')
      if questions.empty? || questions.last[1].any?
        problems << "#{where}: Zu dieser Antwort gehört keine Frage."
      else
        questions.last[1].concat(line[2..].split('|').map(&:strip).reject(&:empty?))
      end
    else
      problems << "#{where}: Eine Zeile beginnt mit »? « (Frage), »= « (Antwort) oder »#« (Kommentar)."
    end
  end
  problems << "#{path}: Die letzte Frage hat keine Antwort." if questions.any? && questions.last[1].empty?
  problems << "#{path}: Hier steht noch keine Frage." if questions.empty?
  [questions, problems]
end

# Die Dateien aus rounds.txt, dazu die Probleme in rounds.txt.
def read_rounds(root)
  rounds = []
  problems = []
  File.readlines(File.join(root, 'rounds.txt'), chomp: true).each_with_index do |line, i|
    where = "rounds.txt, Zeile #{i + 1}"
    name = line.strip
    if name.empty? || name.start_with?('#')
      next
    elsif name.match?(CONFLICT)
      problems << "#{where}: Hier ist noch ein Konflikt nicht aufgelöst (#{name[0, 7]})."
    elsif rounds.include?(name)
      problems << "#{where}: #{name} steht schon weiter oben."
    elsif !File.file?(File.join(root, name))
      problems << "#{where}: Die Datei #{name} gibt es nicht."
    else
      rounds << name
    end
  end
  [rounds, problems]
end

# Alles, was am Quiz noch nicht stimmt; leer, wenn alles stimmt.
def problems_in(root)
  rounds, problems = read_rounds(root)
  rounds.each do |name|
    problems.concat(read_questions(File.join(root, name))[1].map { |p| p.sub(File.join(root, ''), '') })
  end
  Dir.glob('questions/*.txt', base: root).sort.each do |name|
    problems << "#{name} steht nicht in rounds.txt." unless rounds.include?(name)
  end
  problems
end

def same_answer?(given, answers)
  answers.any? { |answer| given.strip.downcase == answer.downcase }
end

# Spielt die Fragen der Dateien und gibt die Punkte zurück.
def play(paths)
  points = 0
  total = 0
  paths.each do |path|
    puts "\n== #{File.basename(path, '.txt')} =="
    read_questions(path)[0].each do |question, answers|
      print "#{question} "
      given = $stdin.gets.to_s
      total = total + 1
      if same_answer?(given, answers)
        points = points + 1
        puts 'Richtig!'
      else
        puts "Leider nein: #{answers.first}"
      end
    end
  end
  puts "\n#{points} von #{total} Punkten."
  points
end

if __FILE__ == $PROGRAM_NAME
  root = __dir__
  if ARGV == ['--check']
    problems = problems_in(root)
    if problems.empty?
      count = read_rounds(root)[0].sum { |name| read_questions(File.join(root, name))[0].size }
      rounds = read_rounds(root)[0].size
      puts "Alles in Ordnung: #{rounds} #{rounds == 1 ? 'Runde' : 'Runden'} mit #{count} #{count == 1 ? 'Frage' : 'Fragen'}."
    else
      puts problems
      exit 1
    end
  elsif ARGV.size == 1
    questions, problems = read_questions(ARGV[0])
    abort problems.join("\n") unless problems.empty?
    play([ARGV[0]]) if questions.any?
  else
    problems = problems_in(root)
    abort "Erst muss alles stimmen (ruby quiz.rb --check):\n#{problems.join("\n")}" unless problems.empty?
    play(read_rounds(root)[0].map { |name| File.join(root, name) })
  end
end
