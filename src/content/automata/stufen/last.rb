# Ist die N-te Ziffer von hinten eine 1? Der NEA rät, wo diese 1 steht, und
# zählt danach die Ziffern bis zum Ende ab.
N = 3
DIGITS = '01'
START = 0
ACCEPTING = Set[N]
TRANSITIONS = {
    [0, '0'] => Set[0],
    [0, '1'] => Set[0, 1],
}
(1...N).each do |i|
    TRANSITIONS[[i, '0']] = Set[i + 1]
    TRANSITIONS[[i, '1']] = Set[i + 1]
end
