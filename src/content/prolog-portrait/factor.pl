:- initialization(main, main).

% factors(N, Fs): Fs are the prime factors of N, smallest first
factors(N, Fs) :-
    factors(N, 2, Fs).

factors(1, _, []) :- !.
factors(N, D, [N]) :-
    D * D > N, !.
factors(N, D, [D | Fs]) :-
    N mod D =:= 0, !,
    M is N // D,
    factors(M, D, Fs).
factors(N, D, Fs) :-
    E is D + 1,
    factors(N, E, Fs).

main :-
    writeln('Please enter the number to factorise:'),
    read_line_to_string(user_input, Line),
    number_string(N, Line),
    factors(N, Fs),
    format("Prime factors: ~w~n", [Fs]).
