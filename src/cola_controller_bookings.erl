-module(cola_controller_bookings).

%% API
-export([ get/2
        , post/2
        ]).

-export([ trails/0 ]).

-behaviour(trails_handler).

-include("cola_default_handler.hrl").

%%%===================================================================
%%% API
%%%===================================================================

swagger_doc_get() ->
  #{ tags => ["bookings"]
   , description => "Gets all bookings"
   , responses =>
       #{ <<"200">> =>
            #{ description => "200 OK"
             , content => #{'application/json' => #{schema => cowboy_swagger:schema(<<"schema_todo">>)}}
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
        #{ description => "Deletes a new booking"
         , content => #{ 'application/json' => #{ schema => cowboy_swagger:schema(<<"post_bookings_request">>)}}
         }
%%    , parameters =>
%%        [ #{ name => "room"
%%           , in => "query"
%%           , description => "Room name"
%%           , required => true
%%           , schema => #{ type => string, example => "C01" }
%%           }
%%        , #{ name => "start_time"
%%           , in => "query"
%%           , description => "Start of the booking. An RFC 3339 timestamp described by RFC 3339."
%%           , required => true
%%           , schema => #{ type => string, example => "2021-04-10T18:24:31Z" }
%%           }
%%        , #{ name => "end_time"
%%           , in => "query"
%%           , description => "End of the booking. An RFC 3339 timestamp described by RFC 3339."
%%           , required => true
%%           , schema => #{ type => string, example => "2021-04-10T18:24:31Z" }
%%           }
%%        ]
   , responses =>
       #{ <<"200">> =>
            #{ description => "200 OK"
             , content => #{'application/json' => #{schema => cowboy_swagger:schema(<<"post_bookings_response">>)}}
             }
        }
   }.
post(Params, #state{client = Client}) ->
  Room      = proplists:get_value(room,       Params),
  StartTime = proplists:get_value(start_time, Params),
  EndTime   = proplists:get_value(end_time,   Params),
  Created = case cola_bookings:is_free(Room, StartTime, EndTime) of
              true  -> cola_bookings:insert_new(Client, Room, StartTime, EndTime);
              false -> false
            end,
  Result = case Created of
             {true, Id} -> #{ created          => true
                            , room             => Room
                            , start_time       => StartTime
                            , end_time         => EndTime
                            , booking_id       => Id
                            };
             false      -> #{ created          => false }
           end,
  {continue, Result}.

%%%===================================================================
%%% Swagger hook
%%%===================================================================

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
  ok = cowboy_swagger:add_definition(<<"post_bookings_request">>,
    #{ room       => #{ type => "string", example => "C01"}
     , start_time => #{ type => "string", example => "2021-04-10T18:24:31Z"}
     , end_time   => #{ type => "string", example => "2021-04-10T18:24:31Z"}
     }
  ),
  ok = cowboy_swagger:add_definition(<<"post_bookings_response">>,
    #{ room       => #{ type => "string", required => "false", example => "C01"}
     , start_time => #{ type => "string", required => "false", example => "2021-04-10T18:24:31Z"}
     , end_time   => #{ type => "string", required => "false", example => "2021-04-10T18:24:31Z"}
     , booking_id => #{ type => "string",  required => "false"}
     , created    => #{ type => "boolean", required => "true"}
     }
  ),
  Metadata = #{ post => swagger_doc_post()
              },
  {Path, Handler, Params} = lists:keyfind("/bookings", 1, cola_http:routes()),
  [trails:trail(Path, Handler, Params, Metadata)].

%%%===================================================================
%%% Internal functions
%%%===================================================================
