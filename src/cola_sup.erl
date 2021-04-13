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
  Worker1  = #{ id        => cola_worker_permission
              , start     => {cola_worker_permission, start_link, []}
              , restart   => permanent
              , shutdown  => 2000
              , type      => worker
              , modules   => [cola_worker_permission]
              },
  Worker2 =  #{ id        => cola_worker_crypto
              , start     => {cola_worker_crypto, start_link, []}
              , restart   => permanent
              , shutdown  => 2000
              , type      => worker
              , modules   => [cola_worker_crypto]
              },
  {ok, { #{ strategy    => one_for_one
          , intensity   => 5
          , period      => 30
          }
       , [Worker1, Worker2]
       }}.
