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
        , lookup/1
        , all_rooms/0
        , all_rooms/1
        ]).

-type booking() :: {Room::binary(), StartTime::binary(), EndTime::binary()}.
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
  Result    :: boolean().
insert_new(Client, Room, StartTime, EndTime) ->
  case proplists:get_value(Room, ?OWNERSHIP) of
    Client -> insert_new(Room, StartTime, EndTime);
    _      -> false
  end.

-spec insert_new(Room, StartTime, EndTime) -> Result
  when Room      :: binary(),
       StartTime :: binary(),
       EndTime   :: binary(),
       Result    :: boolean().
insert_new(Room, StartTime0, EndTime0) ->
  case ets:member(?MODULE, Room) of
    false -> false;
    true ->
      Bookings0 = lookup(Room),
      StartTime1 = calendar:rfc3339_to_system_time(to_list(StartTime0)),
      EndTime1   = calendar:rfc3339_to_system_time(to_list(EndTime0)),
      Bookings1 = [ {Room, StartTime1, EndTime1} | Bookings0],
      ets:update_element(?MODULE, Room, {2, Bookings1})
  end.

-spec is_free(Room, StartTime, EndTime) -> Result
  when Room      :: binary(),
       StartTime :: binary(),
       EndTime   :: binary(),
       Result    :: boolean().
is_free(Room, StartTime, EndTime) ->
  true.

-spec lookup(Room) -> Bookings
  when Room     :: binary(),
       Bookings :: [booking()].
lookup(Room) ->
  case ets:lookup(?MODULE, Room) of
    []                 -> [];
    [{Room, Bookings}] -> [ { Room
                            , unicode:characters_to_binary(to_rfc3339(Start))
                            , unicode:characters_to_binary(to_rfc3339(End))
                            }
                          || {Room, Start, End} <- Bookings
                          ]
  end.

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

%% "2021-04-10T21:15:31Z"
to_rfc3339(Time) ->
  calendar:system_time_to_rfc3339(Time, [{unit, second}, {time_designator, $T}, {offset, "Z"}]).

to_list(Value) ->
  unicode:characters_to_list(Value).