%%%-------------------------------------------------------------------
%% @doc cola public API
%% @end
%%%-------------------------------------------------------------------

-module(cola_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  cola_bookings:init(),
  cola_http:start().

stop(_State) ->
  cola_http:stop(),
  ok.
