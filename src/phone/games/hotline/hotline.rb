# Die Hotline des Hackschule Workspace: immer besetzt, mit Fragen, die zu
# nichts führen, und Musik in der Warteschlange. Jeder Weg führt zurück in
# die Schleife; wer den geheimen Ausgang findet (die Sterntaste), kommt raus.
require 'telefon'

HOLD = 'https://www.youtube.com/watch?v=-xRD8VmjBQM'   # Thimbleweed Park, »Hotel Elevator«

QUESTIONS = [
  ['Haben Sie schon versucht, den Workspace aus- und wieder einzuschalten?', 'Drücken Sie die 1 für ja, die 2 für nein.'],
  ['Welche Farbe hat Ihr Semikolon?', 'Drücken Sie die 1 für schwarz, die 2 für eine andere Farbe.'],
  ['Haben Sie Ihre Leerzeichen gezählt?', 'Drücken Sie die 1 für gerade, die 2 für ungerade.'],
  ['Ist Ihre Endlosschleife schon zu Ende?', 'Drücken Sie die 1 für ja, die 2 für noch nicht.'],
  ['Steht Ihr Computer gerade vor Ihnen oder hinter Ihnen?', 'Drücken Sie die 1 für vor mir, die 2 für hinter mir.'],
  ['Haben Sie den Fehler schon gefragt, warum er da ist?', 'Drücken Sie die 1 für ja, die 2 für er antwortet nicht.'],
  ['Ist Ihr Bildschirm eingeschaltet?', 'Wenn Sie das nicht sehen können, drücken Sie bitte die 1.'],
].freeze

REPLIES = [
  'Vielen Dank. Ihre Antwort wurde an die zuständige Abteilung weitergeleitet.',
  'Das haben wir befürchtet.',
  'Interessant. Das hat noch niemand geantwortet.',
  'Ihre Eingabe wurde gespeichert. Leider in der falschen Tabelle.',
  'Einen Moment, ich schaue nach. Hm.',
].freeze

$place = 3
$questions = []

# the music runs on in the waiting line; only the place gets worse
def hold
  say "Alle Leitungen sind belegt. Sie sind an Position #{$place} in der Warteschlange.", interrupt: true
  $place += [2, 5, 11, 37].sample
  sleep 6
  say 'Ihr Anruf ist uns wichtig. Bitte bleiben Sie dran.', mood: 'amused', interrupt: true
  sleep 5
end

def human
  say 'Sie möchten mit einem Menschen sprechen.'
  say 'Alle Menschen sind gerade in einer Besprechung über die Hotline.', mood: 'surprised'
  say 'Psst. Ich glaube, einer ist eingeschlafen.', mood: 'whisper'
end

def exit_found
  music nil
  sleep 1
  say 'Oh. Sie haben den geheimen Ausgang gefunden.', mood: 'surprised'
  say 'Vielen Dank für Ihre Geduld. Ihr Problem gilt hiermit als gelöst.'
  hangup
end

# every question once, in shuffled order, then again from the start
def question
  $questions = QUESTIONS.shuffle if $questions.empty?
  text, keys_hint = $questions.pop
  say text, interrupt: true
  say "#{keys_hint} Um mit einem Menschen zu sprechen, drücken Sie die 0.", interrupt: true
  key = keys(1)
  case key
  when '*' then exit_found
  when '0' then human
  when '' then say 'Wir haben keine Eingabe erhalten. Das werten wir als Ja.', mood: 'sleepy'
  else say REPLIES.sample
  end
end

music HOLD
say 'Herzlich willkommen bei der Hotline des Hackschule Workspace.'
loop do
  hold
  question
  say 'Wir verbinden Sie jetzt mit der nächsten freien Hotline. Das ist diese hier.', mood: 'amused'
end
