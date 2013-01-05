-module(grass).
-extends(plant).

-export([init/2]).

init(Grid, Pos) -> spawn(fun() -> live(Grid, Pos) end).

live(Grid, Pos) ->
    receive
        update ->
            update(Grid, Pos),
            live(Grid, Pos);
        {ping, Pid} ->
            Pid ! {pong, grass, Pos},
            live(Grid, Pos);
        destroy -> Pos;
        _ -> 
            io:put_chars("Unknown message\n")
    end.

update(_Grid, Pos) ->
    ?BASE_MODULE:updateGraphics(Pos, Pos, green).