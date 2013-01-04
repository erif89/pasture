-module(fox).
-extends(animal).

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

update(Grid, {X, Y}) ->
    ?BASE_MODULE:move(
        Grid, 
        {X, Y}, 
        {   
            X+random:uniform(2)-random:uniform(2), 
            Y+random:uniform(2)-random:uniform(2)
        }, 
        red).