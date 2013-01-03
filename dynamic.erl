-module(dynamic).
-extends(entity).

% -export([init/0]).

% init() -> spawn(fun() -> live(default) end).

deleteFromPos(Grid, Pos) ->
    L = ets:lookup(Grid, Pos),
    case lists:filter(fun({Pos,Pid}) -> Pid =:= self(), L) of
        [X] -> ets:delete_object(Grid, X);
        _ -> error
    end.