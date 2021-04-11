-module(cola_http).

%% API
-export([ start/0
        , stop/0
        , routes/0
        ]).

-define(HTTP_LISTENER, my_http_listener).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
  Dispatch = compile_swagger_dispatch(),
  {ok, _} = cowboy:start_tls(?HTTP_LISTENER,
                             [ {port,        https_port()}
                             , {certfile,    server_certfile()}
                             , {keyfile,     server_keyfile()}
                             , {verify,      verify_peer}
                             , {cacertfile,  server_certfile()} % here we trust ourselves as CA
                             ],
                             #{env => #{dispatch => Dispatch}}).

stop() ->
  cowboy:stop_listener(?HTTP_LISTENER).

routes() ->
  {ok, Routes} = file:consult(filename:join(code:priv_dir("cola"),"routes.src")),
  Routes.

%%%===================================================================
%%% Internal functions
%%%===================================================================

https_port() ->
  case application:get_env(https_port) of
    {ok, Value} -> Value;
    undefined   -> 8443
  end.

server_certfile() ->
  filename:join(code:priv_dir("cola"),"server.crt").

server_keyfile() ->
  filename:join(code:priv_dir("cola"),"server.key").

compile_swagger_dispatch() ->
  Trails = trails:trails([ cola_controller_bookings
                         , cola_controller_bookings_id
                         , cola_controller_rooms
                         , cowboy_swagger_handler
                         ]),
  trails:store(Trails),
  trails:single_host_compile(Trails).
