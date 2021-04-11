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
        , insert_new/3
        , insert_new/4
        , is_free/3
        , to_rfc3339/1
        , all_bookings/1
        , all_rooms/0
        , all_rooms/1
        ]).

-ifdef(TEST).
-export([ check_is_free/2 ]).
-endif.

-type booking() :: {Room::binary(), BookingId::binary(), StartTime::binary(), EndTime::binary()}.
-type client()  :: coke | pepsi.

-define(OWNERSHIP,
  [ {<<"C01">>, coke}
  , {<<"C02">>, coke}
  , {<<"C03">>, coke}
  , {<<"C04">>, coke}
  , {<<"C05">>, coke}
  , {<<"C06">>, coke}
  , {<<"C07">>, coke}
  , {<<"C08">>, coke}
  , {<<"C09">>, coke}
  , {<<"C10">>, coke}
  , {<<"P01">>, pepsi}
  , {<<"P02">>, pepsi}
  , {<<"P03">>, pepsi}
  , {<<"P04">>, pepsi}
  , {<<"P05">>, pepsi}
  , {<<"P06">>, pepsi}
  , {<<"P07">>, pepsi}
  , {<<"P08">>, pepsi}
  , {<<"P09">>, pepsi}
  , {<<"P10">>, pepsi}
  ]
).

%%%===================================================================
%%% API
%%%===================================================================

init() ->
  ets:new(?MODULE, [named_table, ordered_set, public]),
  init_rooms(),
  ok.

-spec insert_new(Client, Room, StartTime, EndTime) -> Result
  when Client    :: client(),
  Room      :: binary(),
  StartTime :: binary(),
  EndTime   :: binary(),
  Result    :: {true, Id::binary()} | false.
insert_new(Client, Room, StartTime, EndTime) ->
  case proplists:get_value(Room, ?OWNERSHIP) of
    Client -> insert_new(Room, StartTime, EndTime);
    _      -> false
  end.

-spec insert_new(Room, StartTime, EndTime) -> Result
  when Room      :: binary(),
       StartTime :: binary(),
       EndTime   :: binary(),
       Result    :: {true, Id::binary()} | false.
insert_new(Room, StartTime0, EndTime0) ->
  case ets:member(?MODULE, Room) of
    false -> false;
    true ->
      Id = unicode:characters_to_binary(cola_uuid:new()),
      Bookings0 = lookup_by_room(Room),
      Bookings1 = add_one(Room, Id, StartTime0, EndTime0, Bookings0),
      case ets:update_element(?MODULE, Room, {2, Bookings1}) of
        true -> {true, Id};
        false -> false
      end
  end.

-spec is_free(Room, StartTime, EndTime) -> Result
  when Room      :: binary(),
       StartTime :: binary(),
       EndTime   :: binary(),
       Result    :: boolean().
is_free(Room, StartTime, EndTime) ->
  case ets:member(?MODULE, Room) of
    false -> false;
    true ->
      Bookings = lists:keysort(2, lookup_by_room(Room)),
      check_is_free({StartTime, EndTime}, Bookings)
  end.

-spec all_bookings(Room) -> Result
  when Room   :: binary(),
       Result :: [booking()].
all_bookings(Room) ->
  [ { Room
    , BookingId
    , unicode:characters_to_binary(to_rfc3339(Start))
    , unicode:characters_to_binary(to_rfc3339(End))
    }
    || {_, BookingId, Start, End} <- lookup_by_room(Room)
  ].

-spec all_rooms() -> Result
  when Result :: [binary()].
all_rooms() ->
  [Room || {Room, _Owner} <- ?OWNERSHIP].

-spec all_rooms(Owner) -> Result
  when Owner  :: client(),
       Result :: [binary()].
all_rooms(Client) ->
  [Room || {Room, Owner} <- ?OWNERSHIP, Owner =:= Client].

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_rooms() ->
  ets:insert_new(?MODULE, {<<"C01">>, []}),
  ets:insert_new(?MODULE, {<<"C02">>, []}),
  ets:insert_new(?MODULE, {<<"C03">>, []}),
  ets:insert_new(?MODULE, {<<"C04">>, []}),
  ets:insert_new(?MODULE, {<<"C05">>, []}),
  ets:insert_new(?MODULE, {<<"C06">>, []}),
  ets:insert_new(?MODULE, {<<"C07">>, []}),
  ets:insert_new(?MODULE, {<<"C08">>, []}),
  ets:insert_new(?MODULE, {<<"C09">>, []}),
  ets:insert_new(?MODULE, {<<"C10">>, []}),
  ets:insert_new(?MODULE, {<<"P01">>, []}),
  ets:insert_new(?MODULE, {<<"P02">>, []}),
  ets:insert_new(?MODULE, {<<"P03">>, []}),
  ets:insert_new(?MODULE, {<<"P04">>, []}),
  ets:insert_new(?MODULE, {<<"P05">>, []}),
  ets:insert_new(?MODULE, {<<"P06">>, []}),
  ets:insert_new(?MODULE, {<<"P07">>, []}),
  ets:insert_new(?MODULE, {<<"P08">>, []}),
  ets:insert_new(?MODULE, {<<"P09">>, []}),
  ets:insert_new(?MODULE, {<<"P10">>, []}).

-spec lookup_by_room(Room) -> Bookings
  when Room     :: binary(),
       Bookings :: [{Room::binary(), Id::binary(), Start::integer(), End::integer()}].
lookup_by_room(Room) ->
  case ets:lookup(?MODULE, Room) of
    []                 -> [];
    [{Room, Bookings}] -> Bookings
  end.

add_one(Room, Id, StartTime0, EndTime0, Bookings0) ->
  StartTime1 = calendar:rfc3339_to_system_time(to_list(StartTime0)),
  EndTime1   = calendar:rfc3339_to_system_time(to_list(EndTime0)),
  [ {Room, Id, StartTime1, EndTime1} | Bookings0].

check_is_free({StartTime0, EndTime0}, Bookings) ->
  StartTime1 = calendar:rfc3339_to_system_time(to_list(StartTime0)),
  EndTime1   = calendar:rfc3339_to_system_time(to_list(EndTime0)),
  check_is_free({StartTime1, EndTime1}, Bookings, {0, 0}).

check_is_free({NewStart, NewEnd}, _, _) when NewStart >= NewEnd ->
  false;
check_is_free(_,                  [], {0, 0}) ->
  true;
check_is_free({_, NewEnd},        [], {0, Lower}) ->
  NewEnd < Lower;
check_is_free({NewStart, _},      [], {Upper, 0}) ->
  Upper < NewStart;
check_is_free({NewStart, NewEnd}, [], {Upper, Lower}) ->
  Upper < NewStart andalso NewEnd < Lower;
check_is_free({NewStart, NewEnd}, [{_, _, Start, End} | Rest],   {_, Lower}) when Start < NewStart ->
  check_is_free({NewStart, NewEnd}, Rest, {End, Lower});
check_is_free({NewStart, NewEnd}, [{_, _, Start, _End} | _Rest], {Upper, _}) ->
  check_is_free({NewStart, NewEnd}, [], {Upper, Start}).

%% to "2021-04-10T21:15:31Z"
to_rfc3339(Time) when is_integer(Time) ->
  calendar:system_time_to_rfc3339(Time, [{unit, second}, {time_designator, $T}, {offset, "Z"}]).

to_list(Value) ->
  unicode:characters_to_list(Value).