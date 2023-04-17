-module(fib_ex6).
-compile(nowarn_export_all).
-compile(export_all).
-author("P.R.").

fibonacci(N) ->
    if N =< 1 ->
        N;
    true ->
        fibonacci(N-1) + fibonacci(N-2)
    end.

fibonacciTR(N) ->
    fibonacciTRHelper(N, 0, 1).

fibonacciTRHelper(0, C1, _) ->
    C1;
fibonacciTRHelper(C, C1, C2) ->
    fibonacciTRHelper(C-1, C1+C2, C1).
