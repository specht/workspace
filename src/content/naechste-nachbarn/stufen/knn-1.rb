# Wie weit zwei Punkte auseinanderliegen, mit beliebig vielen Merkmalen
def distance(a, b)
  sum = 0.0
  a.each_index do |i|
    sum += (a[i] - b[i])**2
  end
  Math.sqrt(sum)
end
