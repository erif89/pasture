-module(rabbit).
-extends(animal).



-export([init/2]).

init(Grid, Pos) -> spawn(fun() -> live(Grid, Pos) end).


live(Grid, Pos) ->

    receive
        update ->
            NewPos = update(Grid, Pos),
            live(Grid, NewPos);
        {query, Pid} ->
            Pid ! {answer, rabbit, Pos};
        destroy -> Pos;
        _ -> 
            io:put_chars("Unknown message\n")
    end.
    
navigate(X, Y) ->
    % List the possible moves
    Moves = [{X+1,Y},{X,Y+1},{X+1,Y+1},
        {X-1,Y-1},{X-1,Y},{X,Y-1},{X-1,Y+1},{X+1,Y-1}],
    % Rearrange them randomly
    L = [X||{_,X} <- lists:sort([{random:uniform(), Move} || Move <- Moves])],
    % Send queries to entities at adjacent tiles
    NumMsgs = sendQueries(L, 0),
    % Receive answers
    Receive = fun (_) -> receive {answer, Type, Pos} -> {Type, Pos} end,
    Answers = lists:map(Receive, lists:seq(1, NumMsgs)),
    if
        Answers =:= [] ->
            {X+random:uniform(3)-3, Y+random:uniform(3)-3}; % No neighbours
        lists:any(fun (Type) -> Type =:= grass, Answers) ->
            navigate(Rest);
        lists:any(fun (Type) -> Type =:= rabbit, Answers) -> navigate(Rest);
    case  of
        unableToMove -> {X, Y};
        Pos -> Pos
    end.

sendQueries([], Acc) -> Acc;
sendQueries([Pos|Rest], Acc) ->
    Stuff = ets:lookup(Grid, Pos),
    Send = fun (Pid, Acc) -> Pid ! {query, self()}, Acc + 1 end,
    NumMsgs = lists:mapfoldl(Send, 0, Stuff),
    sendQueries(Rest, Acc + NumMsgs).

update(Grid, {X, Y}) ->
    ?BASE_MODULE:move(Grid, {X, Y}, navigate(X, Y), blue).