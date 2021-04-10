%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2021 22:40
%%%-------------------------------------------------------------------
-module(assadsadd).
-author("bruce").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(assadsadd_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #assadsadd_state{}} | {ok, State :: #assadsadd_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #assadsadd_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #assadsadd_state{}) ->
  {reply, Reply :: term(), NewState :: #assadsadd_state{}} |
  {reply, Reply :: term(), NewState :: #assadsadd_state{}, timeout() | hibernate} |
  {noreply, NewState :: #assadsadd_state{}} |
  {noreply, NewState :: #assadsadd_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #assadsadd_state{}} |
  {stop, Reason :: term(), NewState :: #assadsadd_state{}}).
handle_call(_Request, _From, State = #assadsadd_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #assadsadd_state{}) ->
  {noreply, NewState :: #assadsadd_state{}} |
  {noreply, NewState :: #assadsadd_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #assadsadd_state{}}).
handle_cast(_Request, State = #assadsadd_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #assadsadd_state{}) ->
  {noreply, NewState :: #assadsadd_state{}} |
  {noreply, NewState :: #assadsadd_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #assadsadd_state{}}).
handle_info(_Info, State = #assadsadd_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #assadsadd_state{}) -> term()).
terminate(_Reason, _State = #assadsadd_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #assadsadd_state{},
    Extra :: term()) ->
  {ok, NewState :: #assadsadd_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #assadsadd_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
