#!/usr/bin/env ruby
# Checks, before the live import, that moving the invitation lists into Neo4j
# changes nothing (docs/teacher-access.md, »Einladungen importieren«). Reads
# a folder of invitation files, for example a copy of the production
# /invitations, and needs no server and no database:
#
#   src/scripts/check-invitation-import.rb ~/kopie-der-einladungen
#
# 1. The new parser (Roster) reads the files exactly as the old loader did:
#    every invitation, name, group, membership and teacher role, in order.
# 2. The records the import writes give the same roster again, without
#    order (what the import checks once more against Neo4j).
#
# Files starting with _ are left out: they are written by tools and stay
# files. Prints a summary and every difference; exits 1 if there is one.

require_relative '../ruby/include/roster'
require_relative '../ruby/test/support/legacy_invitations'

dir = ARGV.first
abort "Aufruf: #{$PROGRAM_NAME} ORDNER-MIT-EINLADUNGEN" unless dir && File.directory?(dir)

paths = Dir[File.join(dir, '*.txt')].sort.reject { |p| File.basename(p).start_with?('_') }
abort "Keine Einladungsdateien (*.txt) in #{dir}" if paths.empty?
texts = paths.map { |p| File.read(p) }

legacy = LegacyInvitations.load(texts, [])
parsed = Roster.parse_files(texts)
roster = Roster.build(:groups => parsed[:groups], :members => parsed[:members], :designated => parsed[:designated], :admins => [])

problems = []
legacy.each_key do |key|
    next if legacy[key] == roster[key] && legacy[key].to_a == roster[key].to_a

    problems << "Der neue Leser weicht beim alten Wert #{key} ab."
end

records = Roster.records_from_files(parsed, :school => 'home')
from = Roster.from_records(records)
after = Roster.build(:groups => from[:groups], :members => from[:members], :designated => from[:designated], :admins => [])
problems += Roster.differences(Roster.content(legacy), Roster.content(after))

puts "Dateien:      #{paths.map { |p| File.basename(p) }.join(', ')}"
puts "Gruppen:      #{records[:groups].size}"
puts "Eingeladen:   #{legacy[:invitations].size} (davon ohne Namen: #{records[:invitations].count { |i| i[:name].nil? }})"
puts "Mitgliedschaften: #{records[:memberships].size}"
puts "Lehrkräfte:   #{legacy[:teacher_emails].size}"
puts
if problems.empty?
    puts 'Keine Unterschiede: Der Import ändert nichts daran, wer sich anmelden kann, in welcher Gruppe ist und was betreut.'
else
    puts "#{problems.size} Unterschied(e):"
    problems.each { |line| puts "  #{line}" }
    exit 1
end
