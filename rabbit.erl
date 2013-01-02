-module(rabbit).
-extends(animal).

-export([init/1]).

init(Grid) -> spawn(fun() -> live(Grid, default) end).


live(Grid, State) ->
    receive
        {update, Info} -> live(Grid, State);
        {destroy} -> State
    end.

update({X, Y}, Grid) ->
    if
        ((X > 30) or (Y > 30)) ->
            ok;
        true ->
            base:move({X, Y}, {X-1, Y-1}, blue)
    end.