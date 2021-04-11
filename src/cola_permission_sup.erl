%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cola_permission_sup).

-behaviour(supervisor).

-export([ start_link/0
        , init/1
        ]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Worker = #{ id        => cola_permission_worker
            , start     => {cola_permission_worker, start_link, []}
            , restart   => permanent
            , shutdown  => 2000
            , type      => worker
            , modules   => [cola_permission_worker]
            },
  {ok, { #{ strategy    => one_for_one
          , intensity   => 5
          , period      => 30
          }
       , [Worker]
       }}.
