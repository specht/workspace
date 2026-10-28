# Das Logfile des Webservers, gelesen mit regulären Ausdrücken
lines = File.readlines('access.log', chomp: true)
line = lines[3]

puts line
p line =~ /HTTP/
p line =~ /" 404 /
p lines.count { |l| l =~ /" 404 / }

m = line.match(/"(GET|POST) (\S+) /)
p m[0]
p m[1]
p m[2]

# Der Status einer Zeile als Zahl, oder nil, wenn die Zeile keinen hat
def status(line)
    m = line.match(/" (\d{3}) /)
    return nil if m.nil?
    m[1].to_i
end

p lines.map { |l| status(l) }.tally
