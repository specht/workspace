| number factors |
Transcript show: 'Enter a number: '.
number := stdin nextLine asNumber.
Transcript show: 'Factors: '.
2 to: number do: [:i |
    [(number \\ i) = 0] whileTrue: [
        Transcript show: i asString, ' '.
        number := number // i
    ]
].
Transcript cr.