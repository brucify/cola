%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2021 20:00
%%%-------------------------------------------------------------------
-module(cola_bookings).

%% API
-export([ init/0
        , insert_new/4
        , is_free/4
        , delete_booking/2
        , delete_all/0
        , lookup_booking/2
        , lookup_by_room/1
        , maybe_insert_new/4
        , to_rfc3339/1
        , all_rooms/1
        ]).

-export([ room/1
        , start_time/1
        , end_time/1
        , booking_id/1
        , hash_value/1
        ]).

-ifdef(TEST).
-export([ check_is_free/2 ]).
-endif.

-type booking()          :: { BookingId   :: string()
                            , Room        :: string()
                            , StartTime   :: string()
                            , EndTime     :: string()
                            , Client      :: client()
                            }.
-type booking_db_entry() :: { BookingId   :: string()
                            , Room        :: string()
                            , StartTime   :: integer()
                            , EndTime     :: integer()
                            , Client      :: client()
                            }.
-type client()  :: coke | pepsi.

-define(OWNERSHIP,
  [ {"C01", coke}
  , {"C02", coke}
  , {"C03", coke}
  , {"C04", coke}
  , {"C05", coke}
  , {"C06", coke}
  , {"C07", coke}
  , {"C08", coke}
  , {"C09", coke}
  , {"C10", coke}
  , {"P01", pepsi}
  , {"P02", pepsi}
  , {"P03", pepsi}
  , {"P04", pepsi}
  , {"P05", pepsi}
  , {"P06", pepsi}
  , {"P07", pepsi}
  , {"P08", pepsi}
  , {"P09", pepsi}
  , {"P10", pepsi}
  ]
).

%%%===================================================================
%%% API
%%%===================================================================

init() ->
  ets:new(?MODULE, [named_table, ordered_set, public]),
  ok.

delete_all() ->
  ets:delete_all_objects(?MODULE).

-spec maybe_insert_new(Room, StartTime, EndTime, Client) -> Result
  when Room      :: string(),
       StartTime :: string(),
       EndTime   :: string(),
       Client    :: client(),
       Result    :: {true, booking()} | false.
maybe_insert_new(Room0, StartTime0, EndTime0, Client) ->
  case is_free(Room0, StartTime0, EndTime0, Client) of
    false -> false;
    true ->
      case insert_new(Room0, StartTime0, EndTime0, Client) of
        false      -> false;
        {true, Id} -> {true, lookup(Id)}
      end
  end.

-spec insert_new(Room, StartTime, EndTime, Client) -> Result
  when Room      :: string(),
       StartTime :: string(),
       EndTime   :: string(),
       Client    :: client(),
       Result    :: {true, Id::string()} | false.
insert_new(Room, StartTime0, EndTime0, Client) ->
  case (is_allowed(Room, Client) orelse is_public_mode())
    andalso lists:keymember(Room, 1, ?OWNERSHIP) of
    false -> false;
    true ->
      Id = cola_uuid:new(),
      StartTime1 = calendar:rfc3339_to_system_time(StartTime0),
      EndTime1   = calendar:rfc3339_to_system_time(EndTime0),
      case ets:insert_new(?MODULE, {Id, Room, StartTime1, EndTime1, Client}) of
        false -> false;
        true  -> cola_worker_crypto:recalc_merkle(Room),
                 {true, Id}
      end
  end.

-spec is_free(Room, StartTime, EndTime, Client) -> Result
  when Room      :: string(),
       StartTime :: string(),
       EndTime   :: string(),
       Client    :: client(),
       Result    :: boolean().
is_free(Room, StartTime, EndTime, Client) ->
  case (is_allowed(Room, Client) orelse is_public_mode())
    andalso lists:keymember(Room, 1, ?OWNERSHIP) of
    false -> false;
    true ->
      Bookings = lists:keysort(3, lookup_by_room(Room)), % sort by start_time
      check_is_free({StartTime, EndTime}, Bookings)
  end.

-spec lookup_booking(Id, Client) -> Booking
  when Id       :: string(),
       Client   :: client(),
       Booking  :: booking() | undefined.
lookup_booking(Id, Client) ->
  case lookup(Id) of
    undefined                  -> undefined;
    {Id,_,_,_,Client}=Booking  -> Booking;
    _                          -> undefined
  end.

-spec delete_booking(Id, Client) -> true
  when Id       :: string(),
       Client   :: client().
delete_booking(Id, Client) ->
  case lookup(Id) of
    undefined         -> true;
    {Id,_,_,_,Client} -> ets:delete(?MODULE, Id);
    _                 -> true
  end.

-spec all_rooms(Owner) -> Result
  when Owner  :: client(),
       Result :: [string()].
all_rooms(Client) ->
  [Room || {Room, Owner} <- ?OWNERSHIP, Owner =:= Client orelse is_public_mode()].

-spec room(booking()) -> string().
room({_Id, Room, _StartTime, _EndTime, _Client}=Booking) ->
  Room.

-spec start_time(booking()) -> string().
start_time({_Id, _Room, StartTime, _EndTime, _Client}=Booking) ->
  StartTime.

-spec end_time(booking()) -> string().
end_time({_Id, _Room, _StartTime, EndTime, _Client}=Booking) ->
  EndTime.

-spec booking_id(booking()) -> string().
booking_id({Id, _Room, _StartTime, _EndTime, _Client}=Booking) ->
  Id.

-spec hash_value(booking()) -> binary().
hash_value(Booking) ->
  base64:encode(merkerl:hash_value(Booking)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec lookup(Id) -> Booking
  when Id       :: string(),
       Booking  :: booking() | undefined.
lookup(Id) ->
  case ets:lookup(?MODULE, Id) of
    []            -> undefined;
    [{I,R,S,E,C}] -> {I,R,to_rfc3339(S), to_rfc3339(E),C}
  end.

-spec lookup_by_room(Room) -> Bookings
  when Room     :: string(),
       Bookings :: [booking()].
lookup_by_room(Room) ->
  case ets:match_object(?MODULE, {'_', Room, '_', '_', '_'}) of
    []       -> [];
    Bookings -> [{I,R,to_rfc3339(S), to_rfc3339(E),C} || {I,R,S,E,C} <- Bookings]
  end.

-spec check_is_free({StartTime, EndTime}, Bookings) -> Result
  when StartTime :: string(),
       EndTime   :: string(),
       Bookings  :: [booking_db_entry()],
       Result    :: boolean().
check_is_free({StartTime0, EndTime0}, Bookings) ->
  StartTime1 = calendar:rfc3339_to_system_time(StartTime0),
  EndTime1   = calendar:rfc3339_to_system_time(EndTime0),
  check_is_free({StartTime1, EndTime1}, Bookings, {0, 0}).

check_is_free({NewStart, NewEnd}, _,  _) when NewStart >= NewEnd ->
  false;
check_is_free({NewStart, NewEnd}, _,  _) when NewEnd - NewStart > 3600 ->
  false;
check_is_free(_,                  [], {0, 0}) ->
  true;
check_is_free({_, NewEnd},        [], {0, Lower}) ->
  NewEnd < Lower;
check_is_free({NewStart, _},      [], {Upper, 0}) ->
  Upper < NewStart;
check_is_free({NewStart, NewEnd}, [], {Upper, Lower}) ->
  Upper < NewStart andalso NewEnd < Lower;
check_is_free({NewStart, NewEnd}, [{_, _, Start, End, _} | Rest],   {_, Lower}) when Start < NewStart ->
  check_is_free({NewStart, NewEnd}, Rest, {End, Lower});
check_is_free({NewStart, NewEnd}, [{_, _, Start, _End, _} | _Rest], {Upper, _}) ->
  check_is_free({NewStart, NewEnd}, [], {Upper, Start}).

-spec is_allowed(Room, Client) -> boolean()
  when Room   :: string(),
       Client :: client().
is_allowed(Room, Client) ->
  case proplists:get_value(Room, ?OWNERSHIP) of
    Client -> true;
    _      -> false
  end.

-spec is_public_mode() -> boolean().
is_public_mode() ->
  public =:= cola_worker_permission:current_mode().

%% to "2021-04-10T21:15:31Z"
to_rfc3339(Time) when is_integer(Time) ->
  calendar:system_time_to_rfc3339(Time, [{unit, second}, {time_designator, $T}, {offset, "Z"}]).
