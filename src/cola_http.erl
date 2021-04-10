-module(cola_http).

%% API
-export([ start/0
        , stop/0
        ]).

-define(HTTP_LISTENER, my_http_listener).

start() ->
  {ok, Routes} = file:consult(filename:join(code:priv_dir("cola"),"routes.src")),

  Dispatch = cowboy_router:compile([
    {'_', Routes}
  ]),

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

https_port() ->
  case application:get_env(https_port) of
    {ok, Value} -> Value;
    undefined   -> 8443
  end.

server_certfile() ->
  filename:join(code:priv_dir("cola"),"server.crt").

server_keyfile() ->
  filename:join(code:priv_dir("cola"),"server.key").