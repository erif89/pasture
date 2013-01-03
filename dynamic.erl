-module(dynamic).
-extends(entity).

-export([deleteFromPos/2]).

% Delete self from position Pos in the ets Grid
deleteFromPos(Grid, Pos) ->
    L = ets:lookup(Grid, Pos),
    % io:fwrite("lookup ~w \n", L),
    ets:delete(Grid, Pos),
    case lists:filter(fun({_,Pid}) -> Pid =/= self() end, L) of
        [] -> true;
        Others -> ets:insert(Grid, Others)
    end.