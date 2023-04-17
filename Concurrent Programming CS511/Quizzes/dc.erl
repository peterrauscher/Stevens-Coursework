-module(dc).
-compile(nowarn_export_all).
-compile(export_all).
% Peter Rauscher

dryCleanerLoop(Clean, Dirty) ->
    receive
        {From, pickUpOverall} when Clean > 0 ->
            From ! {okToClean},
            dryCleanerLoop(Clean - 1, Dirty);
        {dropOffOverall} ->
            dryCleanerLoop(Clean, Dirty + 1);
        {From, dryCleanItem} when Dirty > 0 ->
            From ! {okToPickup},
            dryCleanerLoop(Clean + 1, Dirty - 1)
    end.

employee(DC) ->
    DC ! {dropOffOverall},
    DC ! {self(), pickUpOverall},
    receive
        {okToPickup} ->
            okToPickup
    end.

dryCleanMachine(DC) ->
    DC ! {self(), dryCleanItem},
    receive
        {okToClean} ->
            okToClean
    end,
    timer:sleep(1000),
    dryCleanMachine(DC).

start(E, M) ->
    DC = spawn(?MODULE, dryCleanerLoop, [0, 0]),
    [spawn(?MODULE, employee, [DC]) || _ <- lists:seq(1, E)],
    [spawn(?MODULE, dryCleanMachine, [DC]) || _ <- lists:seq(1, M)].
