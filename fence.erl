-module(fence)
-extends(fixed_object).

-export([init/0])

init() -> spawn(fun() -> live(default) end).


live(State) ->
    receive
        {update, Info} -> live(State);
        {destroy} -> State
    end.
