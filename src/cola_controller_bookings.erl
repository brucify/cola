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
  Room0      = cola_conversion:to_list(proplists:get_value(room,       Params)),
  StartTime0 = cola_conversion:to_list(proplists:get_value(start_time, Params)),
  EndTime0   = cola_conversion:to_list(proplists:get_value(end_time,   Params)),
  case check_time([StartTime0, EndTime0]) of
    false ->
      {400, <<"Bad request">>, #{}};
    true ->
      Result =
        case cola_bookings:maybe_insert_new(Room0, StartTime0, EndTime0, Client) of
          false ->
            #{ created => false };
          {true, Booking} ->
            Id0       = cola_bookings:booking_id(Booking),
            Id        = cola_conversion:to_binary(Id0),
            StartTime = cola_conversion:to_binary(StartTime0),
            EndTime   = cola_conversion:to_binary(EndTime0),
            Room      = cola_conversion:to_binary(Room0),
            Data = <<Room/binary, StartTime/binary, EndTime/binary, Id/binary>>,
            Sig = cola_worker_crypto:sign(Data),
            #{ created          => true
             , room             => Room
             , start_time       => StartTime
             , end_time         => EndTime
             , booking_id       => Id
             , signature        => Sig
             , hash_value       => cola_bookings:hash_value(Booking)
             }
        end,
      {continue, Result}
  end.

%%%===================================================================
%%% Swagger hook
%%%===================================================================

trails() ->
  ok = cowboy_swagger:add_definition_array(<<"schema_todo">>,
    #{ <<"key1">> => #{type => "string"}
     , <<"key2">> => #{ type => "object"
                      , properties =>
                         #{ <<"key3">> => #{ type => "string"}
                          , <<"key4">> => #{ type => "string"}
                          }
                      }
    }
  ),
  ok = cowboy_swagger:add_definition(<<"post_bookings_request">>,
    #{ <<"room">>       => #{ type => "string", example => "C01"}
     , <<"start_time">> => #{ type => "string", example => "2021-04-10T18:24:31Z"}
     , <<"end_time">>   => #{ type => "string", example => "2021-04-10T18:24:31Z"}
     }
  ),
  ok = cowboy_swagger:add_definition(<<"post_bookings_response">>,
    #{ <<"booking_id">> => #{ type => "string", required => "false", example => "bf6a5633-e503-47a6-babe-de3b2c464b86"}
     , <<"room">>       => #{ type => "string", required => "false", example => "C01"}
     , <<"start_time">> => #{ type => "string", required => "false", example => "2021-04-10T18:24:31Z"}
     , <<"end_time">>   => #{ type => "string", required => "false", example => "2021-04-10T18:24:31Z"}
     , <<"created">>    => #{ type => "boolean", required => "true"}
     , <<"signature">>  => #{ type => "string", required => "false"
                            , example => "MEYCIQDQ8WNIH2wkiArOz75/Y3YE1hmIDejQQhymHcDICf4o+wIhALEMQJ4/v/qwhDuW2kfgkFLLabncw5jZjGJ/W7LC7PkR"
                            , description => "A valid ECDSA signature (ecdsa-with-SHA256 1.2.840.10045.4.3.2) of the concatenated values of the room, start_time, end_time, and id keys. Base64 encoded as a string."
                            }
     , <<"hash_value">> => #{ type => "string", required => "false", example => "41wLXtIvtVSeJxGkfr0kGeDeruPh1Vi0WBOtNP4LT9k="}
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