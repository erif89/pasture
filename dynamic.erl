-module(dynamic).
-extends(entity).

-export([deleteFromPos/2]).

% Delete self from position Pos in the ets Grid
deleteFromPos(Grid, Pos) ->
    L = ets:lookup(Grid, Pos),
    ets:delete(Grid, Pos),
    case lists:filter(fun({Pos,Pid}) -> Pid =/= self(), L) of
        [] -> true;
        Others -> ets:insert(Grid, Others)
    end.