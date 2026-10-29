% Listen: ein Kopf und ein Rest.

is_in(X, [X | _]).

size([], 0).
size([_ | Rest], N) :- size(Rest, M), N is M + 1.

joined([], Ys, Ys).
joined([X | Xs], Ys, [X | Zs]) :- joined(Xs, Ys, Zs).
