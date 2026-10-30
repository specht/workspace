require_relative "model"

pairs = pair_counts(words(File.read("maerchen.txt")))
3.times do
  puts text(generate_pairs(pairs, "Es", "war", 30))
  puts
end
