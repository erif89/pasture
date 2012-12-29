-module(entity)

-export([init/0])

init() -> spawn(fun() -> runOut(ScriptPort, ServerPort) end).


live(State) ->
    receive
        {update, Info} -> live(State);
        {kill} -> State
    end.
