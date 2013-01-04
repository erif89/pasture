-module(dynamic).
-extends(entity).

-export([deleteFromPos/2]).

% Delete self from position Pos in the ets Grid
deleteFromPos(Grid, {X, Y}) ->
    L = ets:lookup(Grid, {X, Y}),
    % io:fwrite("lookup ~w \n", L),
    ets:delete(Grid, {X, Y}),
    frame ! {change_cell, X, Y, white},
    case lists:filter(fun({_,Pid}) -> Pid =/= self() end, L) of
        [] -> true;
        Others -> ets:insert(Grid, Others),
        lists:map(fun({P, Pid}) -> Pid ! update end, Others)
    end.