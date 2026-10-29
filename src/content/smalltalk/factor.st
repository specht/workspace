| number divisor |
Transcript show: 'Enter a number: '.
number := stdin nextLine asNumber.
Transcript show: 'Factors: '.
divisor := 2.
"A divisor larger than the square root of what is left cannot divide it
 any more: what is left is then a prime factor itself."
[divisor * divisor <= number] whileTrue: [
    [(number \\ divisor) = 0] whileTrue: [
        Transcript show: divisor printString, ' '.
        number := number // divisor].
    divisor := divisor + 1].
number > 1 ifTrue: [Transcript show: number printString].
Transcript cr.
