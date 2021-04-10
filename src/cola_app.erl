%%%-------------------------------------------------------------------
%% @doc cola public API
%% @end
%%%-------------------------------------------------------------------

-module(cola_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  cola_http:start(),
  cola_sup:start_link().

stop(_State) ->
  cola_http:stop(),
  ok.

%% internal functions