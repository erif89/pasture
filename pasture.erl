-module(pasture).

% Advanced Function Programming, Project - Pasture in Erlang
-author('Emil Wall <emwa3503@student.uu.se>').
-author('Carl Carenvall <caca7037@student.uu.se>').

-include_lib("eunit/include/eunit.hrl").

-export([init/0, start/0]).

% Returns the table identifier of the ets table
-spec init() -> integer().
init() -> 
    spawn_link(pasture,start,[]).


init_test_() ->
    Grid = init(),
    [?_assert(ets:lookup(Grid, {0,0}) =:= [])].


start() ->
    Grid = ets:new('grid', [bag]),
    ets:insert(Grid, [{{1,1},purple}, {{3,3},green}]),
    run(Grid).
    
run(Grid) ->
    % io:put_chars("pasture up and running\n"),
    receive
        after 100 ->
            %% update
            drawAll(Grid, ets:first(Grid)),
            run(Grid)
        end.

drawAll(Grid, '$end_of_table') -> ok;
drawAll(Grid, Curr) -> 
    draw(ets:lookup(Grid, Curr)),
    drawAll(Grid, ets:next(Grid, Curr)).
    

draw([{{X, Y},Type} | Rest]) ->
    % io:put_chars("Attempting to draw once\n"),
    frame ! {change_cell, X, Y, Type}.
    