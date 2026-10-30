# Wie lange dauert das? Dieselbe Frage auf drei Arten, schnelle Hilfen und
# eine Rundfahrt durch Berlin.

# Wie viele verschiedene Namen? Mit einer Liste dessen, was man schon kennt.
def distinct_list(names)
  seen = []
  names.each do |name|
    seen << name unless seen.include?(name)
  end
  seen.size
end
