%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cola_sup).

-behaviour(supervisor).

-export([ start_link/0
        , init/1
        ]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Worker1  = #{ id        => cola_permission_worker
              , start     => {cola_permission_worker, start_link, []}
              , restart   => permanent
              , shutdown  => 2000
              , type      => worker
              , modules   => [cola_permission_worker]
              },
  Worker2 =  #{ id        => cola_crypto_worker
              , start     => {cola_crypto_worker, start_link, []}
              , restart   => permanent
              , shutdown  => 2000
              , type      => worker
              , modules   => [cola_crypto_worker]
              },
  {ok, { #{ strategy    => one_for_one
          , intensity   => 5
          , period      => 30
          }
       , [Worker1, Worker2]
       }}.
