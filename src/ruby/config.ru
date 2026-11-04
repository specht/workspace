require File.expand_path('main', File.dirname(__FILE__))

# Slow and stuck requests and a status line every few minutes in the log
# (RequestDiagnostics, docs/entwicklung.md, »Wenn die Seite langsam ist«).
use RequestDiagnostics::Middleware
run Main
