-module(sem).
-compile(nowarn_export_all).
-compile(export_all).

make(N) ->
    spawn(?MODULE,sem_loop,[N]).
    
acquire(S) ->
    S!{acquire,self()},
    receive
	{ok} ->
	  ok
    end.

release(S) ->
   S!{release}.

sem_loop(0) ->  %% no permits available
    receive
      {release} ->
	    sem_loop(1)
    end;
sem_loop(N) when N>0 ->  %% permits available
    receive
      {acquire,From} ->
	    From ! {ok},
	    sem_loop(N-1);
	{release} ->
	    sem_loop(N+1)
end.
