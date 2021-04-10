-module(cola_controller_bookings).

%% API
-export([ get/2
        , post/2
        ]).

-export([ trails/0 ]).

-behaviour(trails_handler).

swagger_doc_get() ->
  #{ tags => ["bookings"]
   , description => "Gets all bookings"
   , responses =>
       #{ <<"200">> =>
            #{ description => "200 OK"
             , content =>
                 #{ 'application/json' =>
                      #{schema => cowboy_swagger:schema(<<"schema_todo">>)
                       }
                  }
             }
        }
   }.
get(_Params, _State) ->
  Result = <<"hello world">>,
  {continue, Result}.

swagger_doc_post() ->
  #{ tags        => ["bookings"]
   , description => "Creates a new booking"
   , requestBody =>
       #{ description => "Updates the config"
        , content =>
            #{ 'application/json' =>
                 #{ schema => cowboy_swagger:schema(<<"schema_todo">>)
                  }
             }
        }
   , responses =>
       #{ <<"200">> =>
            #{ description => "200 OK"}
        }
   }.
post(_Params, _State) ->
  Result = <<"hello world">>,
  {continue, Result}.

trails() ->
  ok = cowboy_swagger:add_definition_array(<<"schema_todo">>,
    #{ key1 => #{type => "string"}
     , key2 => #{type => "object"
     , properties =>
         #{ key3 => #{ type => "string"}
          , key4 => #{ type => "string"}
          }
    }
    }
  ),
  Metadata = #{ post => swagger_doc_post()
              },
  {Path, Handler, Params} = lists:keyfind("/bookings", 1, cola_http:routes()),
  [trails:trail(Path, Handler, Params, Metadata)].