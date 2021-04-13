%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cola_worker_crypto).

-behaviour(gen_server).

-export([ start_link/0
        , sign/1
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-include_lib("public_key/include/OTP-PUB-KEY.hrl").

-record(crypto_worker_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec sign(Data::binary()) -> Signature::binary().
sign(Data) ->
  {ok, Sig} = gen_server:call(?MODULE, {sign, Data}),
  Sig.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #crypto_worker_state{}}.

handle_call({sign, Data}, _From, #crypto_worker_state{}=State) ->
  Sig = public_key:sign(Data, sha256, key()),
  {reply, {ok, Sig}, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State = #crypto_worker_state{}) ->
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

-spec key() -> public_key:ec_private_key().
key() ->
  {ok, Key} = file:read_file(filename:join(code:priv_dir(cola),"server_ec.key")),
  public_key:pem_entry_decode(hd(public_key:pem_decode(Key))).