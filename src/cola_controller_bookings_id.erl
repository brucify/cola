-module(cola_controller_bookings_id).

%% API
-export([ get/2
        , delete/2
        ]).

-export([ trails/0 ]).

-behaviour(trails_handler).

-include("cola_default_handler.hrl").

%%%===================================================================
%%% API
%%%===================================================================

swagger_doc_get() ->
  #{ tags => ["bookings"]
   , description => "Gets a booking by ID"
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
           , content => #{'application/json' =>
              #{schema => cowboy_swagger:schema(<<"get_bookings_id_response">>)}}
           }
       , <<"404">> =>
          #{ description => "404 Not Found"}
       }
   }.
get(Params, #state{client = Client}) ->
  Id = cola_conversion:to_list(proplists:get_value(id, Params)),
  case cola_bookings:lookup_booking(Id, Client) of
    undefined                      -> {404, <<>>, #{}};
    {Id, Room, Start, End, Client} -> Result = #{ booking_id => cola_conversion:to_binary(Id)
                                                , room       => cola_conversion:to_binary(Room)
                                                , start_time => cola_conversion:to_binary(Start)
                                                , start_end  => cola_conversion:to_binary(End)
                                                },
                                      {continue, Result}
  end.

swagger_doc_delete() ->
  #{ tags        => ["bookings"]
   , description => "Deletes a booking"
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
                 #{schema => cowboy_swagger:schema(<<"delete_bookings_id_response">>)}}
           }
       }
   }.
delete(Params, #state{client = Client}) ->
  Id     = cola_conversion:to_list(proplists:get_value(id, Params)),
  Result = #{ result => cola_bookings:delete_booking(Id, Client)},
  {continue, Result}.


%%%===================================================================
%%% Swagger hook
%%%===================================================================

trails() ->
  ok = cowboy_swagger:add_definition(<<"get_bookings_id_response">>,
    #{ booking_id => #{ type => "string", required => "false", example => "bf6a5633-e503-47a6-babe-de3b2c464b86"}
     , room       => #{ type => "string", required => "false", example => "C01"}
     , start_time => #{ type => "string", required => "false", example => "2021-04-10T18:24:31Z"}
     , end_time   => #{ type => "string", required => "false", example => "2021-04-10T18:24:31Z"}
     }
  ),
  ok = cowboy_swagger:add_definition(<<"delete_bookings_id_response">>,
    #{ result     => #{ type => "boolean", required => "true" }
     }
  ),
  Metadata = #{ get => swagger_doc_get()
              , delete => swagger_doc_delete()
              },
  {Path, Handler, Params} = lists:keyfind("/bookings/:id", 1, cola_http:routes()),
  [trails:trail(Path, Handler, Params, Metadata)].

%%%===================================================================
%%% Internal functions
%%%===================================================================