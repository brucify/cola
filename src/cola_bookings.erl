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
        , lookup_booking/2
        , to_rfc3339/1
        , all_bookings/1
        , all_rooms/1
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
  init_rooms(),
  ok.

-spec insert_new(Room, StartTime, EndTime, Client) -> Result
  when Room      :: string(),
       StartTime :: string(),
       EndTime   :: string(),
       Client    :: client(),
       Result    :: {true, Id::string()} | false.
insert_new(Room, StartTime0, EndTime0, Client) ->
  case (is_allowed(Room, Client) orelse is_public_mode())
    andalso ets:member(?MODULE, Room) of
    false -> false;
    true ->
      Id = cola_uuid:new(),
      StartTime1 = calendar:rfc3339_to_system_time(StartTime0),
      EndTime1   = calendar:rfc3339_to_system_time(EndTime0),
      case ets:insert_new(?MODULE, {Id, Room, StartTime1, EndTime1, Client}) of
        true  -> {true, Id};
        false -> false
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
    andalso ets:member(?MODULE, Room) of
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
    {Id,Room,Start,End,Client} -> {Id, Room, to_rfc3339(Start), to_rfc3339(End), Client};
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

-spec all_bookings(Room) -> Result
  when Room   :: string(),
       Result :: [booking()].
all_bookings(Room) ->
  [ { BookingId
    , Room
    , to_rfc3339(Start)
    , to_rfc3339(End)
    , Client
    }
    || {BookingId, _, Start, End, Client} <- lookup_by_room(Room)
  ].

-spec all_rooms(Owner) -> Result
  when Owner  :: client(),
       Result :: [string()].
all_rooms(Client) ->
  [Room || {Room, Owner} <- ?OWNERSHIP, Owner =:= Client orelse is_public_mode()].

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_rooms() ->
  ets:insert_new(?MODULE, {"C01", []}),
  ets:insert_new(?MODULE, {"C02", []}),
  ets:insert_new(?MODULE, {"C03", []}),
  ets:insert_new(?MODULE, {"C04", []}),
  ets:insert_new(?MODULE, {"C05", []}),
  ets:insert_new(?MODULE, {"C06", []}),
  ets:insert_new(?MODULE, {"C07", []}),
  ets:insert_new(?MODULE, {"C08", []}),
  ets:insert_new(?MODULE, {"C09", []}),
  ets:insert_new(?MODULE, {"C10", []}),
  ets:insert_new(?MODULE, {"P01", []}),
  ets:insert_new(?MODULE, {"P02", []}),
  ets:insert_new(?MODULE, {"P03", []}),
  ets:insert_new(?MODULE, {"P04", []}),
  ets:insert_new(?MODULE, {"P05", []}),
  ets:insert_new(?MODULE, {"P06", []}),
  ets:insert_new(?MODULE, {"P07", []}),
  ets:insert_new(?MODULE, {"P08", []}),
  ets:insert_new(?MODULE, {"P09", []}),
  ets:insert_new(?MODULE, {"P10", []}).

-spec lookup(Id) -> Booking
  when Id       :: string(),
       Booking  :: booking_db_entry() | undefined.
lookup(Id) ->
  case ets:lookup(?MODULE, Id) of
    []                    -> undefined;
    [{_,_,_,_,_}=Booking] -> Booking
  end.

-spec lookup_by_room(Room) -> Bookings
  when Room     :: string(),
       Bookings :: [booking_db_entry()].
lookup_by_room(Room) ->
  case ets:match_object(?MODULE, {'_', Room, '_', '_', '_'}) of
    []       -> [];
    Bookings -> Bookings
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
  public =:= cola_permission_worker:current_mode().

%% to "2021-04-10T21:15:31Z"
to_rfc3339(Time) when is_integer(Time) ->
  calendar:system_time_to_rfc3339(Time, [{unit, second}, {time_designator, $T}, {offset, "Z"}]).
