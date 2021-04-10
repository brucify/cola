%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asdasdasd).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(asdasdasd_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #asdasdasd_state{}}.

handle_call(_Request, _From, State = #asdasdasd_state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #asdasdasd_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #asdasdasd_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #asdasdasd_state{}) ->
  ok.

code_change(_OldVsn, State = #asdasdasd_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
