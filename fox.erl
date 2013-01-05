-module(fox).
-extends(animal).

-export([init/2]).

init(Grid, Pos) -> spawn(fun() -> live(Grid, Pos) end).

live(Grid, Pos) ->
    random:seed(erlang:now()),
    receive
        update ->
            NewPos = update(Grid, Pos),
            live(Grid, NewPos);
        {ping, Pid} ->
            Pid ! {pong, fox, Pos, Pid},
            live(Grid, Pos);
        destroy -> Pos;
        _ -> 
            io:put_chars("Unknown message\n")
    end.
    
navigate(Grid, X, Y) ->
    % List the possible moves
    Moves = [{X+1,Y},{X,Y+1},{X+1,Y+1},
        {X-1,Y-1},{X-1,Y},{X,Y-1},{X-1,Y+1},{X+1,Y-1}],
    % Rearrange them randomly
    L = [Z||{_,Z} <- lists:sort([{random:uniform(), Move} || Move <- Moves])],
    % Send queries to entities at adjacent tiles
    NumMsgs = ?BASE_MODULE:sendQueries(Grid, L, 0),
    % Receive answers
    Receive = fun (_) -> receive {pong, Type, Pos, Pid} -> {Type, Pos, Pid} end end,
    Answers = lists:map(Receive, lists:seq(1, NumMsgs)),
    % Filter out the relevant entities
    IsType = fun (Atom) -> fun ({Type, _}) -> Type =:= Atom end end,
    Rabbits = lists:filter(IsType(rabbit), Answers),
    Foxes = lists:filter(IsType(fox), Answers),
    Fixed = lists:filter(IsType(fixed), Answers),
    % Construct prioritized list of positions and take the first valid one
    Candidates = lists:map(fun ({_, Pos}) -> Pos end, Rabbits) ++ L,
    NewPos = ?BASE_MODULE:decide(Foxes ++ Fixed, {X, Y}, Candidates),
    AtPos = fun ({_,Pos}) -> Pos =:= NewPos end,
    Kill = fun ({_,_,Pid}) -> Pid ! destroy end,
    lists:foreach(Kill, lists:filter(AtPos, Rabbits)),
    NewPos.
    

update(Grid, {X, Y}) ->
    ?BASE_MODULE:move(Grid, {X, Y}, navigate(Grid, X, Y), red).

