-module(plant).
-extends(dynamic).

-export([init/0]).

init() -> spawn(fun() -> live(default) end).


live(State) ->
    receive
        update -> live(State);
        destroy -> ok
    end.
