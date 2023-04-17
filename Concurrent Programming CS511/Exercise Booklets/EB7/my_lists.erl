-module(my_lists).
-compile(nowarn_export_all).
-compile(export_all).
-author("P.R.").

sum(L) -> sum(L, 0).

sum([], Sum) -> Sum;
sum([H | T], Sum) -> sum(T, Sum + H).

maximum([H | T]) ->
    maximum(H, T).
maximum(N, []) ->
    N;
maximum(N, [H | T]) ->
    max(N, maximum(H, T)).

zip([], []) ->
    [];
zip([H | T], []) ->
    [{H, none}] ++ zip(T, []);
zip([], [H | T]) ->
    [{none, H}] ++ zip([], T);
zip([H1| T1], [H2 | T2]) ->
    [{H1, H2}] ++ zip(T1, T2).

append([H1 | T1], L2) ->
    [H1 | append(T1, L2)];
append([], [H2 | T2]) ->
    [H2 | append([], T2)];
append([], []) ->
    [].

reverse([]) ->
    [];
reverse([H1 | T1]) ->
    reverse(T1) ++ [H1].

evenL(L) ->
    lists:filter(fun(N) -> N rem 2 == 0 end, L).

take(0, _) ->
    [];
take(_, []) ->
    [];
take(N, [H | T]) ->
    [H | take(N-1, T)].

drop(_, []) ->
    [];
drop(0, L) ->
    L;
drop(N, [_ | T]) ->
    drop(N-1, T).

map(_, []) ->
    [];
map(F, [H | T]) ->
    [F(H) | map(F, T)].

filter(F, L) -> [E || E <- L, F(E)].

% Fold cannot be done without an initial value?
% Oh lol, use a helper function
fold(F, [H | T]) -> fold(F, H, T).
fold(_, Start, []) -> Start;
fold(F, Start, [H | T]) -> fold(F, F(H, Start), T).