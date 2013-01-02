-module(fence).
-extends(fixed_object).

-export([init/0]).

init() -> spawn(fun() -> live(default) end).


live(Grid, State) ->
    receive
        {update, Info} -> live(Grid, State);
        {destroy} -> State
    end.
