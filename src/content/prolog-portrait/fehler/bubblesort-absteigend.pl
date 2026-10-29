:- initialization(main, main).

% bubble_sort(List, Sorted): swap a pair in the wrong order as long as
% there is one
bubble_sort(List, Sorted) :-
    swap(List, Swapped), !,
    bubble_sort(Swapped, Sorted).
bubble_sort(Sorted, Sorted).

% swap(List, Swapped): Swapped is List with the first pair X, Y swapped
% for which X > Y
swap([X, Y | Rest], [Y, X | Rest]) :-
    X < Y.
swap([Z | Rest], [Z | Swapped]) :-
    swap(Rest, Swapped).

random_number(X) :-
    random_between(0, 99, X).

main :-
    length(List, 10),
    maplist(random_number, List),
    format("Original list: ~w~n", [List]),
    bubble_sort(List, Sorted),
    format("Sorted list: ~w~n", [Sorted]).
