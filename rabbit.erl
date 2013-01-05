-module(rabbit).
-extends(animal).



-export([init/2]).

init(Grid, Pos) -> spawn(fun() -> live(Grid, Pos) end).


live(Grid, Pos) ->

    receive
        update ->
            NewPos = update(Grid, Pos),
            live(Grid, NewPos);
        {ping, Pid} ->
            Pid ! {pong, rabbit, Pos},
            live(Grid, Pos);
        destroy -> Pos;
        _ -> 
            io:put_chars("Unknown message\n")
    end.
    
navigate(Grid, X, Y) ->
    % List the possible moves
    Moves = [{X+1,Y},{X,Y+1},{X+1,Y+1},
        {X-1,Y-1},{X-1,Y},{X,Y-1},{X-1,Y+1},{X+1,Y-1}],
    % Send queries to entities at adjacent tiles
    NumMsgs = ?BASE_MODULE:sendQueries(Grid, Moves, 0),
    % Receive answers
    Receive = fun (_) -> receive {pong, Type, Pos} -> {Type, Pos} end end,
    Answers = lists:map(Receive, lists:seq(1, NumMsgs)),
    % Filter out the different entities
    IsType = fun (Atom) -> fun ({Type, _}) -> Type =:= Atom end end,
    Rabbits = lists:filter(IsType(rabbit), Answers),
    Foxes = lists:filter(IsType(fox), Answers),
    Fixed = lists:filter(IsType(fixed), Answers),
    Grass = lists:filter(IsType(grass), Answers),
    % Construct prioritized list of positions and take the first valid one
    % TODO shuffle the Moves list
    Candidates = lists:map(fun ({_, Pos}) -> Pos end, Grass) ++ Moves,
    ?BASE_MODULE:decide(Rabbits ++ Foxes ++ Fixed, {X, Y}, Candidates).

update(Grid, {X, Y}) ->
    ?BASE_MODULE:move(Grid, {X, Y}, navigate(Grid, X, Y), blue).





