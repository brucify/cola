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
        , recalc_merkle/1
        , root_hash/1
        , merkle/1
        , gen_proof/2
        , verify_proof/3
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-include_lib("public_key/include/OTP-PUB-KEY.hrl").

-record(crypto_worker_state, { merkle_trees=[] :: [{Room::string(), Hash::binary()}] }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec sign(Data::binary()) -> Signature::binary().
sign(Data) ->
  {ok, Sig} = gen_server:call(?MODULE, {sign, Data}),
  Sig.

-spec merkle(Room) -> Merkle
  when Room   :: string(),
       Merkle :: merkerl:merkle().
merkle(Room) when is_list(Room) ->
  {ok, Merkle} = gen_server:call(?MODULE, {merkle, Room}),
  Merkle.

-spec gen_proof(BookingId, Client) -> Proof
  when BookingId   :: string(),
       Client      :: atom(),
       Proof       :: merkerl:proof() | not_found.
gen_proof(BookingId, Client) when is_list(BookingId) ->
  {ok, Proof} = gen_server:call(?MODULE, {gen_proof, BookingId, Client}),
  Proof.

-spec verify_proof(Hash, Proof, Room) -> Result
  when Hash   :: binary(),
       Proof  :: binary(),
       Room   :: string(),
       Result :: ok | {error, Reason},
       Reason :: root_hash_mismatch | invalid_proof.
verify_proof(Hash, Proof, Room) ->
  {ok, Result} = gen_server:call(?MODULE, {verify_proof, Hash, Proof, Room}),
  Result.

-spec root_hash(Room) -> Hash
  when Room :: string(),
       Hash :: binary().
root_hash(Room) when is_list(Room) ->
  {ok, Hash} = gen_server:call(?MODULE, {root_hash, Room}),
  Hash.

-spec recalc_merkle(Room::string()) -> ok.
recalc_merkle(Room) ->
  gen_server:cast(?MODULE, {recalc_merkle, Room}).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #crypto_worker_state{}}.

handle_call({sign, Data},                      _From, State)                                         ->
  Sig = base64:encode(public_key:sign(Data, sha256, key())),
  {reply, {ok, Sig}, State};
handle_call({merkle, Room},                    _From, #crypto_worker_state{merkle_trees=List}=State) ->
  Merkle = proplists:get_value(Room, List),
  {reply, {ok, Merkle}, State};
handle_call({gen_proof, Id, Client},           _From, #crypto_worker_state{merkle_trees=List}=State) ->
  Result = do_gen_proof(Id, Client, List),
  {reply, {ok, Result}, State};
handle_call({verify_proof, Hash, Proof, Room}, _From, #crypto_worker_state{merkle_trees=List}=State) ->
  Merkle = proplists:get_value(Room, List),
  Result = do_verify_proof( Hash, Merkle, Proof),
  {reply, {ok, Result}, State};
handle_call({root_hash, Room},                 _From, #crypto_worker_state{merkle_trees=List}=State) ->
  io:format(user, "Getting root hash for: ~p~n", [Room]),
  Hash = case proplists:get_value(Room, List) of
           undefined -> <<>>;
           Merkle    -> io:format(user, "Merkle tree: ~p~n", [Merkle]),
                        base64:encode(merkerl:root_hash(Merkle))
         end,
  {reply, {ok, Hash}, State};
handle_call(_Request,                          _From, State)                                         ->
  {reply, ok, State}.


handle_cast({recalc_merkle, Room}, #crypto_worker_state{merkle_trees=List0}=State) ->
  io:format(user, "Recalculating merkle tree: ~p~n", [Room]),
  Bookings = cola_bookings:lookup_by_room(Room),
  Merkle = merkerl:new(Bookings, fun merkerl:hash_value/1),
  List1 = case proplists:get_value(Room, List0) of
            undefined -> [{Room, Merkle} | List0];
            _         -> lists:keyreplace(Room, 1, List0, {Room, Merkle})
          end,
  io:format(user, "New tree: ~p~n", [{Room, Merkle}]),
  {noreply, State#crypto_worker_state{merkle_trees = List1}};
handle_cast(_Request,              State)                                          ->
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

do_gen_proof(Id, Client, List) ->
  io:format(user, "Getting root hash for: ~p~n", [Id]),
  case cola_bookings:lookup_booking(Id, Client) of
    undefined -> not_found;
    {_, Room, _, _, _} = Booking ->
      Merkle = proplists:get_value(Room, List),
      Proof  = merkerl:gen_proof(merkerl:hash_value(Booking), Merkle),
      io:format(user, "Proof: ~p~n", [Proof]),
      base64:encode(term_to_binary(Proof))
  end.

do_verify_proof(Hash0, Merkle, Proof0) ->
  Proof1 = binary_to_term(base64:decode(Proof0)),
  Hash1  = base64:decode(Hash0),
  merkerl:verify_proof(Hash1, Merkle, Proof1).