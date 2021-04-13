%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cola_worker_permission).

-behaviour(gen_server).

-export([ start_link/0
        , public_mode/0
        , private_mode/0
        , current_mode/0
        , reset/0
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(permission_worker_state, { mode = private :: atom() }).

-type mode() :: public | private.

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec reset() -> ok.
reset() ->
  private_mode().

-spec current_mode() -> mode().
current_mode() ->
  {ok, Mode} = gen_server:call(?MODULE, current_mode),
  Mode.

-spec public_mode() -> ok.
public_mode() ->
  gen_server:cast(?MODULE, public).

-spec private_mode() -> ok.
private_mode() ->
  gen_server:cast(?MODULE, private).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #permission_worker_state{}}.

handle_call(current_mode, _From, #permission_worker_state{mode = Mode}=State) ->
  {reply, {ok, Mode}, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(public,  State) ->
  {noreply, State#permission_worker_state{mode = public}};
handle_cast(private, State) ->
  {noreply, State#permission_worker_state{mode = private}};
handle_cast(_Request, State = #permission_worker_state{}) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
