-module(fixed).
-extends(entity).

-export([init/2]).

init(Grid, Pos) -> spawn(fun() -> start(Grid, Pos) end).

start(Grid, Pos) ->
    ?BASE_MODULE:updateGraphics(Pos, Pos, black),
    tick(Grid, Pos).

tick(Grid, Pos) ->
    receive
        update -> ?BASE_MODULE:updateGraphics(Pos, Pos, black),
            tick(Grid, Pos);
        {ping, Pid} ->
            Pid ! {pong, fixed, Pos},
            tick(Grid, Pos);
        destroy -> ok
    end.
