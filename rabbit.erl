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
    Candidates = lists:map(fun ({_, Pos}) -> Pos end, Grass) ++ Moves,
    decide(Rabbits ++ Foxes ++ Fences, {X, Y}, Candidates).

sendQueries(_Grid, [], Acc) -> Acc;
sendQueries(Grid, [Pos|Rest], Acc) ->
    Stuff = ets:lookup(Grid, Pos),
    Send = fun (Pid, Len) -> Pid ! {ping, self()}, (Len + 1) end,
    NumMsgs = lists:mapfoldl(Send, 0, Stuff),
    sendQueries(Grid, Rest, Acc + NumMsgs).

decide(_Obstacles, OldPos, []) ->
    OldPos;
decide(Obstacles, OldPos, [NewPos|Rest]) ->
    AtPos = fun ({_,Pos}) -> Pos =:= NewPos end,
    Invalid = lists:any(AtPos, Obstacles),
    if
        Invalid ->
            decide(Obstacles, OldPos, Rest);
        true ->
            NewPos
    end.

update(Grid, {X, Y}) ->
    ?BASE_MODULE:move(Grid, {X, Y}, navigate(Grid, X, Y), blue).





