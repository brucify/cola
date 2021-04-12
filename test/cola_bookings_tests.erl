%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2021 10:35
%%%-------------------------------------------------------------------
-module(cola_bookings_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST(Fun), {??Fun, Fun}).

cola_bookings_test_() ->
  {"Tests for cola_bookings",
    { setup
    , fun setup_per_suite/0
    , fun cleanup_per_suite/1
    , { foreach
      , fun setup/0
      , fun cleanup/1
      , [ ?TEST(fun test_check_is_free/0)
        , ?TEST(fun test_insert_new_private_mode/0)
        , ?TEST(fun test_insert_new_public_mode/0)
        ]
      }
    }
  }.

setup_per_suite() ->
  cola_sup:start_link(),
  cola_bookings:init().

cleanup_per_suite(_) ->
  ok.

setup() ->
  ok.

cleanup(_) ->
  cola_permission_worker:reset(),
  cola_bookings:delete_all().

test_insert_new_private_mode() ->
  ok = cola_permission_worker:private_mode(),
  ?assertMatch({true, _}, cola_bookings:insert_new("C01", "2021-04-09T08:40:38Z", "2021-04-09T09:40:38Z", coke)),
  ?assertEqual(false,     cola_bookings:insert_new("C01", "2021-04-09T08:40:38Z", "2021-04-09T09:40:38Z", pepsi)),
  ?assertMatch({true, _}, cola_bookings:insert_new("P01", "2021-04-09T08:40:38Z", "2021-04-09T09:40:38Z", pepsi)),
  ?assertEqual(false,     cola_bookings:insert_new("P01", "2021-04-09T08:40:38Z", "2021-04-09T09:40:38Z", coke)).

test_insert_new_public_mode() ->
  ok = cola_permission_worker:public_mode(),
  ?assertMatch({true, _}, cola_bookings:insert_new("C01", "2021-04-09T08:40:38Z", "2021-04-09T09:40:38Z", coke)),
  ?assertMatch({true, _}, cola_bookings:insert_new("C01", "2021-04-09T08:40:38Z", "2021-04-09T09:40:38Z", pepsi)),
  ?assertMatch({true, _}, cola_bookings:insert_new("P01", "2021-04-09T08:40:38Z", "2021-04-09T09:40:38Z", pepsi)),
  ?assertMatch({true, _}, cola_bookings:insert_new("P01", "2021-04-09T08:40:38Z", "2021-04-09T09:40:38Z", coke)).

test_check_is_free() ->
  Bookings = [ {cola_uuid:new(), "C01", 1618078958, 1618082558, coke} % "2021-04-10T18:22:38Z" -> "2021-04-10T19:22:38Z"
             , {cola_uuid:new(), "C01", 1618089758, 1618093358, coke} % "2021-04-10T21:22:38Z" -> "2021-04-10T22:22:38Z"
             , {cola_uuid:new(), "C01", 1618093838, 1618094438, coke} % "2021-04-10T22:30:38Z" -> "2021-04-10T22:40:38Z"
             , {cola_uuid:new(), "C01", 1618105238, 1618119638, coke} % "2021-04-11T01:40:38Z" -> "2021-04-11T05:40:38Z"
             , {cola_uuid:new(), "C01", 1618216838, 1618224038, coke} % "2021-04-12T08:40:38Z" -> "2021-04-12T10:40:38Z"
             ],
  % same start and end
  ?assertEqual(false, cola_bookings:check_is_free({"2021-04-09T08:40:38Z", "2021-04-09T08:40:38Z"}, [])),
  % reversed start and end
  ?assertEqual(false, cola_bookings:check_is_free({"2021-04-09T09:40:38Z", "2021-04-09T08:40:38Z"}, [])),
  % duplicate
  ?assertEqual(false, cola_bookings:check_is_free({"2021-04-10T18:22:38Z", "2021-04-10T19:22:38Z"}, Bookings)),
  %% ends before 1st booking starts
  ?assertEqual(true,  cola_bookings:check_is_free({"2021-04-09T08:40:38Z", "2021-04-09T09:40:38Z"}, Bookings)),
  %% ends inside 1st booking
  ?assertEqual(false, cola_bookings:check_is_free({"2021-04-10T17:40:38Z", "2021-04-10T18:40:38Z"}, Bookings)),
  %% starts inside 1st booking, ends before 2nd booking
  ?assertEqual(false, cola_bookings:check_is_free({"2021-04-10T19:00:38Z", "2021-04-10T19:40:38Z"}, Bookings)),
  %% starts inside 1st booking, ends inside 2nd booking
  ?assertEqual(false, cola_bookings:check_is_free({"2021-04-10T19:00:38Z", "2021-04-10T21:30:38Z"}, Bookings)),
  %% starts inside 1st booking, ends after 2nd booking
  ?assertEqual(false, cola_bookings:check_is_free({"2021-04-10T19:00:38Z", "2021-04-10T23:40:38Z"}, Bookings)),
  %% starts after 1st booking, ends before 2nd booking
  ?assertEqual(true,  cola_bookings:check_is_free({"2021-04-10T19:23:38Z", "2021-04-10T20:23:38Z"}, Bookings)),
  %% starts after 1st booking, ends inside 2nd booking
  ?assertEqual(false, cola_bookings:check_is_free({"2021-04-10T19:23:38Z", "2021-04-10T21:23:38Z"}, Bookings)),
  %% starts after 1st booking, ends after 2nd booking
  ?assertEqual(false, cola_bookings:check_is_free({"2021-04-10T19:23:38Z", "2021-04-10T22:23:38Z"}, Bookings)),
  %% starts inside 2nd booking, ends before 3rd booking
  ?assertEqual(false, cola_bookings:check_is_free({"2021-04-10T21:40:38Z", "2021-04-10T22:00:38Z"}, Bookings)),
  %% starts after 2nd booking, ends before 3rd booking
  ?assertEqual(true,  cola_bookings:check_is_free({"2021-04-10T22:23:38Z", "2021-04-10T22:29:38Z"}, Bookings)),
  %% starts after 2nd booking, ends inside 3rd booking
  ?assertEqual(false, cola_bookings:check_is_free({"2021-04-10T20:40:38Z", "2021-04-10T21:40:38Z"}, Bookings)),
  %% starts inside last booking
  ?assertEqual(false, cola_bookings:check_is_free({"2021-04-12T10:30:38Z", "2021-04-12T11:30:38Z"}, Bookings)),
  %% starts after last booking ends
  ?assertEqual(true,  cola_bookings:check_is_free({"2021-04-13T08:40:38Z", "2021-04-13T09:40:38Z"}, Bookings)),
  %% more than 60 min
  ?assertEqual(false, cola_bookings:check_is_free({"2021-04-13T08:40:38Z", "2021-04-13T09:41:38Z"}, Bookings)),
  ok.
