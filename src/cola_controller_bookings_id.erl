-module(cola_controller_bookings_id).

%% API
-export([ get/2
        , post/2
        , delete/2
        ]).

-export([ trails/0 ]).

-behaviour(trails_handler).

%%%===================================================================
%%% API
%%%===================================================================

swagger_doc_get() ->
  #{ tags => ["bookings"]
   , description => "Gets all bookings"
   , parameters =>
      [ #{ name => "id"
         , in => "path"
         , description => "The booking ID. UUID v4."
         , required => true
         , schema => #{ type => string }
         , example => "226e6fcf-fac8-4f33-81dd-ff4c60351cc1"
         }
      ]
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
   , description => "Updates a booking"
   , parameters =>
      [ #{ name => "id"
         , in => "path"
         , description => "The booking ID. UUID v4."
         , required => true
         , schema => #{ type => string }
         , example => "226e6fcf-fac8-4f33-81dd-ff4c60351cc1"
         }
      ]
    , requestBody =>
      #{ description => "Updates a booking"
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

swagger_doc_delete() ->
  #{ tags        => ["bookings"]
   , description => "Deletes a new booking"
   , parameters =>
      [ #{ name => "id"
         , in => "path"
         , description => "The booking ID. UUID v4."
         , required => true
         , schema => #{ type => string }
         , example => "226e6fcf-fac8-4f33-81dd-ff4c60351cc1"
         }
      ]
   , requestBody =>
      #{ description => "Deletes a new booking"
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
delete(_Params, _State) ->
  Result = <<"hello world">>,
  {continue, Result}.


%%%===================================================================
%%% Swagger hook
%%%===================================================================

trails() ->
  Metadata = #{ get => swagger_doc_get()
              , post => swagger_doc_post()
              , delete => swagger_doc_delete()
              },
  {Path, Handler, Params} = lists:keyfind("/bookings/:id", 1, cola_http:routes()),
  [trails:trail(Path, Handler, Params, Metadata)].

%%%===================================================================
%%% Internal functions
%%%===================================================================