-module(pasture).

% Advanced Function Programming, Project - Pasture in Erlang
-author('Emil Wall <emwa3503@student.uu.se>').
-author('Carl Carenvall <caca7037@student.uu.se>').

-include_lib("eunit/include/eunit.hrl").

-export([init/0]).

% Returns the table identifier of the ets table
-spec init() -> integer().
init() -> ets:new('grid', [bag]).


init_test_() ->
    Grid = init(),
    [?_assert(ets:lookup(Grid, {0,0}) =:= [])].
