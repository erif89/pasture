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

% Returns a list of entities with random positions
generate_entities(Grid, NumFoxes, NumRabbits, NumGrass, SizeX, SizeY) ->
    FoxPositions = [{Z + random:uniform(SizeX), Z + random:uniform(SizeY)} || Z <- lists:duplicate(NumFoxes, 0)],
    RabbitPositions = [{Z + random:uniform(SizeX), Z + random:uniform(SizeY)} || Z <- lists:duplicate(NumRabbits, 0)],
    GrassPositions = [{Z + random:uniform(SizeX), Z + random:uniform(SizeY)} || Z <- lists:duplicate(NumGrass, 0)],
    Foxes = lists:map(fun(Pos) -> {Pos, fox:init(Grid, Pos)} end, FoxPositions),
    Rabbits = lists:map(fun(Pos) -> {Pos, rabbit:init(Grid, Pos)} end, RabbitPositions),
    GrassTufts = lists:map(fun(Pos) -> {Pos, grass:init(Grid, Pos)} end, GrassPositions),
    Foxes ++ Rabbits ++ GrassTufts.

start() ->
    Grid = ets:new('grid', [bag, public]),
    ets:insert(Grid, generate_entities(Grid,1,1,1,4,4)),
    run(Grid).
    
run(Grid) ->
    % io:put_chars("pasture up and running\n"),
    receive
        after 100 ->
            %% update
            updateEntities(Grid, ets:first(Grid)),
            run(Grid)
        end.
        
        
updateEntities(Grid, '$end_of_table') -> ok;
updateEntities(Grid, CurrentKey) ->
    updateCell(ets:lookup(Grid, CurrentKey)),
    updateEntities(Grid, ets:next(Grid, CurrentKey)).
    
updateCell([]) -> ok;
updateCell([{{X, Y}, Pid} | Rest]) -> 
    % io:put_chars("Sending update message\n"),
    Pid ! update,
    updateCell(Rest).
    


drawAll(Grid, '$end_of_table') -> ok;
drawAll(Grid, Curr) -> 
    draw(ets:lookup(Grid, Curr)),
    drawAll(Grid, ets:next(Grid, Curr)).
    

draw([{{X, Y},Type, _} | Rest]) ->
    % io:put_chars("Attempting to draw once\n"),
    frame ! {change_cell, X, Y, Type}.
    