-module(fence).
-extends(fixed_object).

-export([init/1]).

init(Grid) -> spawn(fun() -> tick(Grid, default) end).


tick(Grid, State) ->
    receive
        update -> tick(Grid, State);
        destroy -> ok
    end.
