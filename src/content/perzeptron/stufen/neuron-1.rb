# 1 oder 0: ist die gewichtete Summe der Merkmale plus Bias größer als 0?
def predict(weights, bias, features)
  sum = bias
  features.each_index do |i|
    sum += weights[i] * features[i]
  end
  sum > 0 ? 1 : 0
end
