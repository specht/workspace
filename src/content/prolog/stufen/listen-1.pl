% Listen: ein Kopf und ein Rest.

is_in(X, [X | _]).
is_in(X, [_ | Rest]) :- is_in(X, Rest).
