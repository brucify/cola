%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2021 13:35
%%%-------------------------------------------------------------------
-module(cola_uuid).

%% API
-export([ new/0 ]).

new() ->
  uuid:to_string(uuid:uuid4()).