-module(entity).

-export([updateGraphics/3]).

% init() -> spawn(fun() -> live(default) end).



    
updateGraphics({OldX, OldY}, {X, Y}, Graphics) ->
    % paint the old position with whatever is underneath
    frame ! {change_cell, X, Y, Graphics}.
    

% unused until vision is implemented    
manhattan({X1, Y1}, {X2, Y2}) ->    
    abs(X1-X2) + abs(Y1-Y2).

% unused until vision is implemented    
distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(((X2-X1)*(X2-X1))+((Y2-Y1)*(Y2-Y1))).