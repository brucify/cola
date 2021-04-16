%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2021 22:48
%%%-------------------------------------------------------------------
-module(cola_config).

%% API
-export([ get_env/1
        ]).

get_env(Key) ->
  {ok, Value} = application:get_env(cola, Key),
  Value.