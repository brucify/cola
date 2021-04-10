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
        , lookup/1
        ]).

-type booking() :: map().

%%%===================================================================
%%% API
%%%===================================================================

init() ->
  ets:new(?MODULE, [named_table, ordered_set, public]),
  init_rooms(),
  ok.

-spec insert_new(Room, StartTime, EndTime) -> Result
  when Room      :: binary(),
       StartTime :: binary(),
       EndTime   :: binary(),
       Result    :: boolean().
insert_new(Room, StartTime, EndTime) ->
  case ets:member(?MODULE, Room) of
    false -> false;
    true ->
      Bookings0 = lookup(Room),
      Bookings1 = [ #{ start_time => StartTime
                     , end_time => EndTime
                     }
                  | Bookings0
                  ],
      ets:update_element(?MODULE, Room, {2, Bookings1})
  end.

-spec lookup(Room) -> Bookings
  when Room     :: binary(),
       Bookings :: [booking()].
lookup(Room) ->
  case ets:lookup(?MODULE, Room) of
    []                 -> [];
    [{Room, Bookings}] -> Bookings
  end.

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