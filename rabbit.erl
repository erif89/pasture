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
    % Decide which move to make
    case navigate(L) of
        unableToMove -> {X, Y};
        Pos -> Pos
    end.

navigate([]) -> unableToMove;
navigate([Pos|Rest]) ->
    Stuff = ets:lookup(Grid, Pos),
    lists:foreach(fun (Pid) -> Pid ! {query, self()} end, Stuff),
    Answers = lists:map(fun (_) -> receive {answer, Type} -> Type end, Stuff), % TODO could use mapfoldr here?
    if
        Answers =:= [] ->
            Pos; % Should look for food
        lists:any(fun (Type) -> Type =:= fence, Answers) ->
            navigate(Rest);
        lists:any(fun (Type) -> Type =:= rabbit, Answers) -> navigate(Rest);

update(Grid, {X, Y}) ->
    ?BASE_MODULE:move(Grid, {X, Y}, navigate(X, Y), blue).