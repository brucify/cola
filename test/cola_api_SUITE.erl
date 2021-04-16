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

%%%===================================================================
%%% ct_suite callbacks
%%%===================================================================
all() -> lists:flatten([ group(1)
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

group(1) ->
  [ get_rooms_coke
  ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

run(BlueprintFilePath, Config0) ->
  Config1 = [X || {_K,V}=X <- Config0, not is_function(V), not is_reference(V), not is_pid(V)],
  DataDir = proplists:get_value(data_dir, Config1),
  Config2 = [ {base_url, "https://127.0.0.1:8443"} | Config1],
  Path = filename:join(DataDir, BlueprintFilePath),
  case katt:run(Path, Config2) of
    {error, _Reason, _Details}=RunError                                  -> ct:fail(RunError);
    {fail, _Filename, _Config, _FinalConfig, _TransactionResults}=Result -> ct:fail(Result);
    {pass, _Filename, _Config, FinalConfig, _TransactionResults}         -> FinalConfig
  end.

%%%===================================================================
%%% Tests
%%%===================================================================

get_rooms_coke(Config0) ->
  Config1 = [ {request_options, [{ssl_options, [ {certfile, coke_cert(Config0)}
                                               , {keyfile,  coke_key(Config0)}
                                               ]}]}
              | Config0
            ],
  run("get_rooms_coke.apib", Config1).

coke_cert(Config) ->
  filename:join(proplists:get_value(data_dir, Config),"coke.crt").

coke_key(Config) ->
  filename:join(proplists:get_value(data_dir, Config),"coke.key").