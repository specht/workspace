% Listen: ein Kopf und ein Rest.

is_in(X, [X | _]).
is_in(X, [_ | Rest]) :- is_in(X, Rest).

size([], 0).
size([_ | Rest], N) :- size(Rest, M), N is M + 1.
