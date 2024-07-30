|item swap itemCount hasChanged|
item := #(1 4 5 6 10 8 7 61 0 -3) copy.

Transcript show: 'Original array: '.
item do: [:each | Transcript show: each printString, ' '].
Transcript cr.

swap :=
	[:indexOne :indexTwo|
	|temp|
	temp := item at: indexOne.
	item at: indexOne put: (item at: indexTwo).
	item at: indexTwo put: temp].

itemCount := item size.
[hasChanged := false.
itemCount := itemCount - 1.
1 to: itemCount do:
	[:index |
	(item at: index) > (item at: index + 1) ifTrue:
		[swap value: index value: index + 1.
		hasChanged := true]].
hasChanged] whileTrue.

Transcript show: 'Sorted array: '.
item do: [:each | Transcript show: each printString, ' '].
Transcript cr.
