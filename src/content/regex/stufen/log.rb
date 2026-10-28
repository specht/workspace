# Das Logfile des Webservers, gelesen mit regulären Ausdrücken
lines = File.readlines('access.log', chomp: true)
line = lines[3]

puts line
p line =~ /HTTP/
p line =~ /" 404 /
p lines.count { |l| l =~ /" 404 / }
