%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2021 20:00
%%%-------------------------------------------------------------------
-module(cola_rooms).

%% API
-export([ init/0
        ]).

init() ->
  ets:new(?MODULE, [set, public, named_table]),
  ok.