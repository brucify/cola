-module(cola_controller_bookings).

%% API
-export([ post/2
        ]).

-export([ trails/0 ]).

-behaviour(trails_handler).

-include("cola_default_handler.hrl").

%%%===================================================================
%%% API
%%%===================================================================

swagger_doc_post() ->
  #{ tags        => ["bookings"]
   , description => "Creates a new booking"
    , requestBody =>
        #{ description => "Creates a new booking"
         , content => #{ 'application/json' => #{ schema => cowboy_swagger:schema(<<"post_bookings_request">>)}}
         }
   , responses =>
       #{ <<"200">> =>
            #{ description => "200 OK"
             , content => #{'application/json' => #{schema => cowboy_swagger:schema(<<"post_bookings_response">>)}}
             }
        }
   }.
post(Params, #state{client = Client}) ->
  Room      = cola_conversion:to_list(proplists:get_value(room,       Params)),
  StartTime = cola_conversion:to_list(proplists:get_value(start_time, Params)),
  EndTime   = cola_conversion:to_list(proplists:get_value(end_time,   Params)),
  case check_time([StartTime, EndTime]) of
    false ->
      {400, <<"Bad request">>, #{}};
    true ->
      Created = case cola_bookings:is_free(Room, StartTime, EndTime, Client) of
                  true  -> cola_bookings:insert_new(Room, StartTime, EndTime, Client);
                  false -> false
                end,
      Result = case Created of
                 {true, Id} -> #{ created          => true
                                , room             => cola_conversion:to_binary(Room)
                                , start_time       => cola_conversion:to_binary(StartTime)
                                , end_time         => cola_conversion:to_binary(EndTime)
                                , booking_id       => cola_conversion:to_binary(Id)
                                };
                 false      -> #{ created          => false }
               end,
      {continue, Result}
  end.

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
    #{ booking_id => #{ type => "string", required => "false", example => "bf6a5633-e503-47a6-babe-de3b2c464b86"}
     , room       => #{ type => "string", required => "false", example => "C01"}
     , start_time => #{ type => "string", required => "false", example => "2021-04-10T18:24:31Z"}
     , end_time   => #{ type => "string", required => "false", example => "2021-04-10T18:24:31Z"}
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

check_time(List) ->
  try
    [calendar:rfc3339_to_system_time(Time) || Time <- List],
    true
  catch _:_ ->
    false
  end.