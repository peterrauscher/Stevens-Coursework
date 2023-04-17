-module(bt).
-compile(nowarn_export_all).
-compile(export_all).
-author("P.R.").

sumTree({node,N,LT,RT}) ->
    sumTree({node,N,LT,RT}, 0).
sumTree({empty}, Sum) ->
    Sum;
sumTree({node,N,LT,RT}, Sum) ->
    sumTree(LT, Sum+N)