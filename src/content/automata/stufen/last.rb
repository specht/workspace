# Ist die N-te Ziffer von hinten eine 1? Der NEA rät, wo diese 1 steht, und
# zählt danach die Ziffern bis zum Ende ab: In q2 hat er zwei gezählt.
N = 3
DIGITS = '01'
START = 'q0'
ACCEPTING = Set["q#{N}"]
TRANSITIONS = {
    ['q0', '0'] => Set['q0'],
    ['q0', '1'] => Set['q0', 'q1'],
}
(1...N).each do |i|
    TRANSITIONS[["q#{i}", '0']] = Set["q#{i + 1}"]
    TRANSITIONS[["q#{i}", '1']] = Set["q#{i + 1}"]
end
