%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2021 15:29
%%%-------------------------------------------------------------------
-module(cola_conversion).

%% API
-export([ to_binary/1
        , to_list/1
        ]).

to_list(Value) ->
  unicode:characters_to_list(Value).

to_binary(Value) ->
  unicode:characters_to_binary(Value).