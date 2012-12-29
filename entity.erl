-module(entity)

-export([init/0])

init() -> spawn(fun() -> live(default) end).


live(State) ->
    receive
        {update, Info} -> live(State);
        {kill} -> State
    end.
