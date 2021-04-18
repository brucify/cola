-module(cola_api_SUITE).

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-ifdef(TEST).
-compile(nowarn_export_all).
-compile(export_all).
-endif.

-define(API_URL, "https://127.0.0.1:8443").

%%%===================================================================
%%% ct_suite callbacks
%%%===================================================================

all() -> lists:flatten([ group(rooms)
                       , group(bookings)
                       ]).

init_per_suite(Config) ->
  application:ensure_all_started(cola),
  application:ensure_all_started(katt),
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

%%%===================================================================
%%% Tests
%%%===================================================================

group(rooms) ->
  [ get_rooms_pepsi
  , get_rooms_coke
  ];
group(bookings) ->
  [ post_bookings_coke
  ].

get_rooms_coke(Config0) ->
  Config1 = [ coke(Config0) | Config0],
  run("get_rooms_coke.apib", Config1).

get_rooms_pepsi(Config0) ->
  Config1 = [ pepsi(Config0) | Config0],
  run("get_rooms_pepsi.apib", Config1).

post_bookings_coke(Config0) ->
  Config1 = [ coke(Config0) | Config0],
  _       = run("get_rooms_coke.apib", Config1),
  Config2 = run("post_bookings_coke.apib", Config1),
  _       = run("get_rooms_coke_2.apib", Config2),
  _       = run("get_bookings_id_coke.apib", Config2),
  ?assertEqual(true, uuid:is_valid(proplists:get_value(booking_id, Config2))).


%%%===================================================================
%%% Internal functions
%%%===================================================================

run(BlueprintFilePath, Config0) ->
  Config1 = [X || {_K,V}=X <- Config0, not is_function(V), not is_reference(V), not is_pid(V)],
  DataDir = proplists:get_value(data_dir, Config1),
  Path = filename:join(DataDir, BlueprintFilePath),
  Config2 = [ {base_url, ?API_URL} | Config1],
  case katt:run(Path, Config2) of
    {error, _Reason, _Details}=RunError                                  -> ct:fail(RunError);
    {fail, _Filename, _Config, _FinalConfig, _TransactionResults}=Result -> ct:fail(Result);
    {pass, _Filename, _Config, FinalConfig, _TransactionResults}         -> [{list_to_atom(K),V}||{K,V} <- FinalConfig]
                                                                            ++ (Config0 -- Config1)
  end.

coke(Config) -> {hackney_options, hackney_options(coke, Config)}.

pepsi(Config) -> {hackney_options, hackney_options(pepsi, Config)}.

hackney_options(coke, Config) ->
  [ {ssl_options, [ {certfile, coke_cert(Config)}
                  , {keyfile,  coke_key(Config)}
                  ]}
  , {insecure, false}
  , {pool,     false}
  ];
hackney_options(pepsi, Config) ->
  [ {ssl_options, [ {certfile, pepsi_cert(Config)}
                  , {keyfile,  pepsi_key(Config)}
                  ]}
  , {insecure, false}
  , {pool,     false}
  ].

coke_cert(Config) ->
  filename:join(proplists:get_value(data_dir, Config),"coke.crt").

coke_key(Config) ->
  filename:join(proplists:get_value(data_dir, Config),"coke.key").

pepsi_cert(Config) ->
  filename:join(proplists:get_value(data_dir, Config),"pepsi.crt").

pepsi_key(Config) ->
  filename:join(proplists:get_value(data_dir, Config),"pepsi.key").