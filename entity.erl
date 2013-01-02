-module(entity).

-export([init/0]).

init() -> spawn(fun() -> live(default) end).


live(Grid, State) ->
    receive
        {update, Info} -> live(Grid, State);
        {destroy} -> State
    end.

    
updateGraphics({X, Y}, Graphics) ->
    frame ! {change_cell, X, Y, Type}.
    
    
manhattan({X1, Y1}, {X2, Y2}) ->    
    abs(X1-X2) + abs(Y1-Y2).
    
distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(((X2-X1)*(X2-X1))+((Y2-Y1)*(Y2-Y1))).