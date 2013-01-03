-module(animal).
-extends(dynamic).

-export([move/3]).

% init(Grid) -> spawn(fun() -> live(Grid, default) end).


    
    

update(Pos, Grid) ->
    ok.
    
inDanger(Pos, Grid, [], NearestDanger) -> NearestDanger;
inDanger(Pos, Grid, DangerList, NearestDanger) ->
    D = checkDanger(Pos, Grid, ets:first(Grid), hd(DangerList)),
    
    if
        D =:= Pos ->
            inDanger(Pos, Grid, tl(DangerList), NearestDanger);
        true ->
    %        if 
    %            false -> % replace with proper distance check
                    inDanger(Pos, Grid, tl(DangerList), D)
    %            true ->
    %                inDanger(Pos, Grid, tl(DangerList), NearestDanger)
    %        end;
    end.
    

checkDanger(Pos, Grid, '$end_of_table', Danger) -> Pos;
checkDanger({X,Y}, Grid, CurrKey, Danger) -> 
    {X,Y}.
    


% Change position of entity in the ets, by changing which key (position) is
% mapped to the PID of this animal.
move(Grid, OldPos, NewPos, Graphics) ->
    % TODO check that OldPos and NewPos is valid
    ?BASE_MODULE:deleteFromPos(Grid, OldPos),
    ets:insert(Grid, [{NewPos, self()}]),
    ?BASE_MODULE:updateGraphics(OldPos, NewPos, Graphics),
    NewPos.