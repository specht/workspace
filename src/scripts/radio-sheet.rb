#!/usr/bin/env ruby
# The sheet of »Datenübertragung per Funk« (src/static/radio/blatt.pdf): one
# A4 page per person, the protocol above and the rounds below, where the
# teacher writes the word into »Gesendet« and the receiver what arrived into
# »Empfangen«. Run in the Ruby container, which has weasyprint and sees the
# fonts under /src:
#
#   docker exec workspace_ruby_1 ruby /src/scripts/radio-sheet.rb > src/static/radio/blatt.pdf
#
# With --html it prints the HTML instead, to look at it in a browser.
require 'open3'

FONTS = File.expand_path('../static/include/fonts', __dir__)
COLORS = %w[#73d216 #fad31c #e8352a #2d93e0 #c53ee0].freeze
ROUNDS = 8

def face(family, file, weight, style = 'normal')
    "@font-face { font-family: '#{family}'; font-weight: #{weight}; font-style: #{style}; src: url('file://#{FONTS}/#{file}'); }"
end

lamps = COLORS.each_with_index.map do |color, i|
    "<span class='lamp' style='background: #{color}'>#{i + 1}</span>"
end.join

questions = [
    'Wie wird ein Buchstabe übertragen?',
    'Woran erkennt die Empfängerseite, wo ein Buchstabe anfängt und aufhört, und wann das Wort zu Ende ist?',
    'Was tut ihr, wenn etwas schiefgeht, auf der Sender- und auf der Empfängerseite?',
    'Was tut ihr, wenn Lampen wegfallen?',
].map { |q| "<div class='question'><p>#{q}</p><div class='lines'><span></span><span></span><span></span></div></div>" }.join

rows = (1..ROUNDS).map do |n|
    "<tr><td class='n'>#{n}</td><td class='role'><span class='box'></span>S <span class='box'></span>E</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>"
end.join

html = <<~HTML
    <!DOCTYPE html>
    <html lang='de'>
    <head>
    <meta charset='utf-8'>
    <title>Datenübertragung per Funk</title>
    <style>
    #{face('IBM Plex Sans', 'IBM_Plex_Sans/IBMPlexSans-Regular.ttf', 400)}
    #{face('IBM Plex Sans', 'IBM_Plex_Sans/IBMPlexSans-SemiBold.ttf', 600)}
    #{face('IBM Plex Sans', 'IBM_Plex_Sans/IBMPlexSans-Italic.ttf', 400, 'italic')}
    #{face('0xProto', '0xProto/0xProto-Regular.ttf', 400)}
    @page { size: A4; margin: 1.2cm 1.3cm 1.1cm; }
    * { box-sizing: border-box; }
    body { margin: 0; font-family: 'IBM Plex Sans', sans-serif; font-size: 9.5pt; color: #222; }
    h1 { margin: 0; font-size: 17pt; font-weight: 600; }
    h2 { margin: 0.55cm 0 0.15cm; font-size: 11.5pt; font-weight: 600; }
    .head { display: flex; justify-content: space-between; align-items: flex-end; }
    .lamps { white-space: nowrap; }
    .lamp { display: inline-block; margin-left: 0.18cm; width: 0.62cm; height: 0.62cm; border-radius: 50%; border: 0.6pt solid #555;
            font-family: '0xProto', monospace; font-size: 9pt; line-height: 0.6cm; text-align: center; color: #222; }
    .fields { display: flex; gap: 0.5cm; margin-top: 0.45cm; }
    .field { flex: 1; border-bottom: 0.6pt solid #555; padding-bottom: 0.45cm; font-size: 8.5pt; color: #555; }
    .field.short { flex: 0 0 4.2cm; }
    .lead { margin: 0.1cm 0 0.2cm; color: #444; }
    .question p { margin: 0.25cm 0 0; font-weight: 600; }
    .lines span { display: block; height: 0.68cm; border-bottom: 0.5pt solid #999; }
    ol { margin: 0.1cm 0 0.25cm; padding-left: 0.5cm; }
    li { margin: 0.05cm 0; }
    table { width: 100%; border-collapse: collapse; table-layout: fixed; }
    th { font-size: 8.5pt; font-weight: 600; text-align: left; vertical-align: bottom; padding: 0.1cm 0.12cm; border-bottom: 0.9pt solid #333; }
    th small { display: block; font-weight: 400; color: #555; }
    td { height: 0.98cm; border: 0.5pt solid #999; padding: 0 0.12cm; }
    td.n { text-align: center; color: #555; }
    td.role { font-size: 8.5pt; white-space: nowrap; }
    .box { display: inline-block; width: 0.3cm; height: 0.3cm; border: 0.6pt solid #555; margin: 0 0.06cm 0 0.04cm; vertical-align: -0.04cm; }
        </style>
    </head>
    <body>
    <div class='head'><h1>Datenübertragung per Funk</h1><div class='lamps'>#{lamps}</div></div>
    <div class='fields'>
      <div class='field'>Name</div>
      <div class='field'>Partnerin oder Partner</div>
      <div class='field short'>Frequenz in kHz</div>
    </div>

    <h2>Unser Protokoll</h2>
    <p class='lead'>Schreibt es auf, bevor ihr euch trennt: Im anderen Raum könnt ihr nicht mehr nachfragen.</p>
    #{questions}

    <h2>Übertragungen</h2>
    <ol>
      <li>Die Senderseite (S) holt sich das Wort bei der Lehrkraft. Sie sagt es leise und schreibt es unter »Gesendet«.</li>
      <li>In euren Räumen schreibt die Empfängerseite (E) unter »Empfangen«, was ankommt, Buchstabe für Buchstabe. Wer nachfragen geht, hat die Runde nicht geschafft.</li>
      <li>Die Empfängerseite bringt ihr Blatt zur Lehrkraft. Erst danach vergleicht ihr und tragt die Dauer und die Probleme ein.</li>
    </ol>
    <table>
      <colgroup>
        <col style='width: 0.8cm'><col style='width: 1.7cm'><col style='width: 1.3cm'><col style='width: 3.9cm'><col style='width: 3.9cm'>
        <col style='width: 1.6cm'><col style='width: 1.5cm'><col>
      </colgroup>
      <tr><th>Nr.</th><th>Ich bin<small>S oder E</small></th><th>Lampen</th><th>Gesendet<small>schreibt die Lehrkraft</small></th>
          <th>Empfangen<small>schreibt die Empfängerseite</small></th><th>Dauer<small>in Minuten</small></th><th>Richtig?</th><th>Probleme</th></tr>
      #{rows}
    </table>
    </body>
    </html>
HTML

if ARGV.include?('--html')
    puts html
    exit
end

pdf, error, status = Open3.capture3('weasyprint', '--quiet', '-', '-', :stdin_data => html, :binmode => true)
abort "weasyprint failed: #{error[0, 500]}" unless status.success?
$stdout.binmode
$stdout.write(pdf)
