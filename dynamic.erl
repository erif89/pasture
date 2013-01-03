-module(dynamic).
-extends(entity).

% -export([init/0]).

% init() -> spawn(fun() -> live(default) end).

deleteFromPos(Grid, Pos) ->
    L = ets:lookup(Grid, Pos),
    ets:delete(Grid, Pos),
    case lists:filter(fun({Pos,Pid}) -> Pid =/= self(), L) of
        [] -> true;
        Others -> ets:insert(Grid, Others)
    end.