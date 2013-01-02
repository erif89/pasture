-module(rabbit).
-extends(animal).

% cd("C:/users/carl/pasture").

-export([init/2]).

init(Grid, Pos) -> spawn(fun() -> live(Grid, Pos) end).


live(Grid, Pos) ->
    io:put_chars("rabbit is alive\n"),
    receive
        update -> 
            io:put_chars("rabbit attempting update\n"),
            NewPos = update(Grid, Pos),
            live(Grid, NewPos);
        destroy -> Pos;
        _ -> 
            io:put_chars("Unknown message\n")
    end.

update(Grid, {X, Y}) when ((X > 30) or (Y > 30)) ->
        {X, Y};
update(Grid, {X, Y}) ->
    ?BASE_MODULE:move({X, Y}, {X+1, Y+1}, blue).