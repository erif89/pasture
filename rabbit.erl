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
    % Filter out the different entities
    IsType = fun (Atom) -> fun ({Type, _}) -> Type =:= Atom end end,
    Rabbits = lists:filter(IsType(rabbit), Answers),
    Foxes = lists:filter(IsType(fox), Answers),
    Fences = lists:filter(IsType(fence), Answers),
    Grass = lists:filter(IsType(grass), Answers),
    % Construct prioritized list of positions and take the first valid one
    % TODO shuffle the Moves list
    Candidates = lists:map(fun ({_, Pos}) -> Pos end, Grass) ++ Moves,
    decide(Rabbits ++ Foxes ++ Fences, {X, Y}, Candidates).

% Sends ping msgs to all Pids in the list and returns its length
sendQueries(_Grid, [], Acc) -> Acc;
sendQueries(Grid, [Pos|Rest], Acc) ->
    Stuff = ets:lookup(Grid, Pos),
    Send = fun (Pid, Len) -> Pid ! {ping, self()}, (Len + 1) end,
    NumMsgs = lists:mapfoldl(Send, 0, Stuff),
    sendQueries(Grid, Rest, Acc + NumMsgs).

% Returns the first position in Candidates (3rd arg) that does not collide
decide(_Obstacles, OldPos, []) ->
    OldPos;
decide(Obstacles, OldPos, [NewPos|Rest]) ->
    AtPos = fun ({_,Pos}) -> Pos =:= NewPos end,
    case lists:any(AtPos, Obstacles) of
        true ->
            decide(Obstacles, OldPos, Rest);
        false  ->
            NewPos
    end.

update(Grid, {X, Y}) ->
    ?BASE_MODULE:move(Grid, {X, Y}, navigate(Grid, X, Y), blue).





