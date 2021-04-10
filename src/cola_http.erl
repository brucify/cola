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
    [ {port, 8443}
    , {certfile, filename:join(code:priv_dir("cola"),"server.crt")}
    , {keyfile, filename:join(code:priv_dir("cola"),"server.key")}
    , {verify, verify_peer}
    , {cacertfile, filename:join(code:priv_dir("cola"),"server.crt")} % here we trust ourselves as CA
    ],
    #{env => #{dispatch => Dispatch}}
  ).

stop() ->
  cowboy:stop_listener(?HTTP_LISTENER).