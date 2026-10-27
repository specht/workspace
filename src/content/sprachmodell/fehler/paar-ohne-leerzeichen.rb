# Ein winziges Sprachmodell: die Funktionen der Seite, wie sie am Ende in
# model.rb stehen.

# Der Text als Liste von Wörtern und Satzzeichen
def words(text)
  text.scan(/\p{L}+|[.,;:!?]/)
end

# Für jedes Wort: welche Wörter darauf folgen, und wie oft
def follow_counts(tokens)
  counts = {}
  (0...tokens.size - 1).each do |i|
    word = tokens[i]
    following = tokens[i + 1]
    counts[word] ||= {}
    counts[word][following] = (counts[word][following] || 0) + 1
  end
  counts
end

# Die n häufigsten Nachfolger von word, mit ihrer Anzahl
def top(counts, word, n)
  (counts[word] || {}).sort_by { |following, count| -count }.first(n)
end

# Der häufigste Nachfolger von word, nil für ein unbekanntes Wort
def most_likely(counts, word)
  return nil unless counts.key?(word)

  counts[word].max_by { |following, count| count }[0]
end

# Wörter als Text, ohne Leerzeichen vor einem Satzzeichen
def text(tokens)
  tokens.join(" ").gsub(/ ([.,;:!?])/, '\1')
end

# Ein Nachfolger von word, gezogen nach seiner Anzahl; nil für ein unbekanntes Wort
def next_word(counts, word)
  return nil unless counts.key?(word)

  r = rand(counts[word].values.sum)
  counts[word].each do |following, count|
    return following if r < count

    r -= count
  end
end

# start und bis zu n gezogene Wörter danach
def generate(counts, start, n)
  tokens = [start]
  n.times do
    following = next_word(counts, tokens.last)
    break if following.nil?

    tokens << following
  end
  tokens
end

# Für jedes Paar von Wörtern: welche Wörter darauf folgen, und wie oft
def pair_counts(tokens)
  counts = {}
  (0...tokens.size - 2).each do |i|
    pair = "#{tokens[i]}#{tokens[i + 1]}"
    following = tokens[i + 2]
    counts[pair] ||= {}
    counts[pair][following] = (counts[pair][following] || 0) + 1
  end
  counts
end

# first, second und bis zu n gezogene Wörter danach, mit zwei Wörtern Kontext
def generate_pairs(counts, first, second, n)
  tokens = [first, second]
  n.times do
    following = next_word(counts, tokens.last(2).join(" "))
    break if following.nil?

    tokens << following
  end
  tokens
end
