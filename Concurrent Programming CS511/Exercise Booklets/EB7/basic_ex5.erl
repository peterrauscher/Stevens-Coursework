-module(basic_ex5).
-compile(nowarn_export_all).
-compile(export_all).
-author("P.R.").

% Exercise 5 in EB7

mult(A, B) ->
    A*B.
double(A) ->
    2*A.
distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2-X1, 2)+math:sqrt(Y2-Y1, 2)).

my_and(true, true) ->
    true;
my_and(_, _) ->
    false.

my_or(true, _) ->
    true;
my_or(_, true) ->
    true;
my_or(_, _) ->
    false.

my_not(false) ->
    true;
my_not(true) ->
    false.