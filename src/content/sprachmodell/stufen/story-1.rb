require_relative "model"

counts = follow_counts(words(File.read("maerchen.txt")))
tokens = ["Es"]
30.times do
  tokens << most_likely(counts, tokens.last)
end
puts text(tokens)
