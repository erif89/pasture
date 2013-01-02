-module(fox).
-extends(animal).

-export([init/0]).

init() -> spawn(fun() -> live(default) end).


live(Grid, {X, Y, State}) ->
    receive
        {update, Info} -> live(Grid, State);
        {destroy} -> State
    end.

update(Grid) ->
    