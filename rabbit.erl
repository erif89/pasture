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
            Pid ! {pong, rabbit, Pos};
        destroy -> Pos;
        _ -> 
            io:put_chars("Unknown message\n")
    end.
    
navigate(Grid, X, Y) ->
    % List the possible moves
    Moves = [{X+1,Y},{X,Y+1},{X+1,Y+1},
        {X-1,Y-1},{X-1,Y},{X,Y-1},{X-1,Y+1},{X+1,Y-1}],
    % Send queries to entities at adjacent tiles
    NumMsgs = sendQueries(Grid, Moves, 0),
    % Receive answers
    Receive = fun (_) -> receive {pong, Type, Pos} -> {Type, Pos} end end,
    Answers = lists:map(Receive, lists:seq(1, NumMsgs)),
    IsType = fun (Atom) -> fun ({Type, _}) -> Type =:= Atom end end,
    Rabbits = lists:filter(IsType(rabbit), Answers),
    Foxes = lists:filter(IsType(fox), Answers),
    Fences = lists:filter(IsType(fence), Answers),
    Grass = lists:filter(IsType(grass), Answers),
    decide(Rabbits, Foxes, Fences, Grass, {X, Y}, Moves).

sendQueries(Grid, [], Acc) -> Acc;
sendQueries(Grid, [Pos|Rest], Acc) ->
    Stuff = ets:lookup(Grid, Pos),
    Send = fun (Pid, Acc) -> Pid ! {ping, self()}, (Acc + 1) end,
    NumMsgs = lists:mapfoldl(Send, 0, Stuff),
    sendQueries(Grid, Rest, Acc + NumMsgs).

decide(Rabbits, Foxes, Fences, [], OldPos, []) ->
    OldPos;
decide(Rabbits, Foxes, Fences, [], OldPos, [NewPos|Rest]) ->
    AtPos = fun ({_,Pos}) -> Pos =:= NewPos end,
    RabbitBlock = lists:any(AtPos, Rabbits),
    FoxBlock = lists:any(AtPos, Foxes),
    FenceBlock = lists:any(AtPos, Fences),
    if
        RabbitBlock or FoxBlock or FenceBlock ->
            decide(Rabbits, Foxes, Fences, [], OldPos, Rest);
        true ->
            NewPos
    end;
decide(Rabbits, Foxes, Fences, [{_,NewPos}|Rest], OldPos, Moves) ->
    AtPos = fun ({_,Pos}) -> Pos =:= NewPos end,
    RabbitBlock = lists:any(AtPos, Rabbits),
    FoxBlock = lists:any(AtPos, Foxes),
    FenceBlock = lists:any(AtPos, Fences),
    if
        RabbitBlock or FoxBlock or FenceBlock ->
            decide(Rabbits, Foxes, Fences, Rest, OldPos, Moves);
        true ->
            NewPos
    end.

update(Grid, {X, Y}) ->
    ?BASE_MODULE:move(Grid, {X, Y}, navigate(Grid, X, Y), blue).





