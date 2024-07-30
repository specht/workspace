-module(divs).
-export([divs/1]).

divs(0) -> [];
divs(1) -> [];
divs(N) -> lists:sort(divisors(1,N))++[N].

divisors(1,N) -> 
     [1] ++ divisors(2,N,math:sqrt(N)).

divisors(K,_N,Q) when K > Q -> [];
divisors(K,N,_Q) when N rem K =/= 0 -> 
    [] ++ divisors(K+1,N,math:sqrt(N));
divisors(K,N,_Q) when K * K  == N -> 
    [K] ++ divisors(K+1,N,math:sqrt(N));
divisors(K,N,_Q) ->
    [K, N div K] ++ divisors(K+1,N,math:sqrt(N)).

main(_) ->
    io:format("Enter a number: "),

    {ok, Number} = io:fread("", "~d"),
    IntegerNumber = list_to_integer(string:strip(Number, both, $\n)),
    io:format("~p", [divs(IntegerNumber)]).

% -module(factor).
% -export([main/0]).

% main() ->
%     io:format("Enter a number: "),
%     {ok, Number} = io:fread("~d"),
%     Factors = prime_factors(Number),
%     io:format("Prime factors: ~p~n", [Factors]).

% prime_factors(Number) ->
%     prime_factors(Number, 2, []).

% prime_factors(1, _, Acc) ->
%     lists:reverse(Acc);
% prime_factors(Number, Factor, Acc) when Number rem Factor =:= 0 ->
%     prime_factors(Number div Factor, Factor, [Factor | Acc]);
% prime_factors(Number, Factor, Acc) ->
%     NextFactor = case Factor of
%         2 -> 3;
%         _ -> Factor + 2
%     end,
%     prime_factors(Number, NextFactor, Acc).