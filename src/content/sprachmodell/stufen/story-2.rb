require_relative "model"

counts = follow_counts(words(File.read("maerchen.txt")))
3.times do
  puts text(generate(counts, "Es", 30))
  puts
end
