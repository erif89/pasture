-module(grass).
-extends(plant).

-export([init/2]).

init(Grid, Pos) -> spawn(fun() -> live(Grid, Pos) end).

live(Grid, Pos) ->
    receive
        update ->
            NewPos = update(Grid, Pos),
            live(Grid, NewPos);
        destroy -> Pos;
        _ -> 
            io:put_chars("Unknown message\n")
    end.

update(_Grid, Pos) ->
    ?BASE_MODULE:updateGraphics(Pos, Pos, green).