# Der Text als Liste von Wörtern und Satzzeichen
def words(text)
  text.scan(/\p{L}+|[.,;:!?]/)
end
