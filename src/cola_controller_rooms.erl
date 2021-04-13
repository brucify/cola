-module(cola_controller_rooms).

%% API
-export([ get/2
        ]).

-export([ trails/0 ]).

-behaviour(trails_handler).

-include("cola_default_handler.hrl").

%%%===================================================================
%%% API
%%%===================================================================

swagger_doc_get() ->
  #{ tags => ["rooms"]
   , description => "Gets meeting rooms availability"
   , responses =>
       #{ <<"200">> =>
          #{ description => "200 OK"
           , content => #{ 'application/json' => #{schema => cowboy_swagger:schema(<<"all_rooms">>)}}
           }
        }
   }.
get(_Params, #state{client=Client}) ->
  Rooms = cola_bookings:all_rooms(Client),
  Result = lists:map(
    fun(Room) ->
      Bookings = cola_bookings:lookup_by_room(Room),
      Occupied = [ #{ start_time => cola_conversion:to_binary(StartTime)
                    , end_time   => cola_conversion:to_binary(EndTime)
                    }
                   || {_, _, StartTime, EndTime, _} <- Bookings
                 ],
      #{ name     => cola_conversion:to_binary(Room)
       , occupied => Occupied
       }
    end,
    Rooms
  ),
  {continue, Result}.

%%%===================================================================
%%% Swagger hook
%%%===================================================================

trails() ->
  ok = cowboy_swagger:add_definition_array(<<"all_rooms">>,
    #{ <<"name">>       => #{type => "string"}
     , <<"occupied">>   => cowboy_swagger:schema(<<"occupied_slots">>)
     }
  ),
  ok = cowboy_swagger:add_definition_array(<<"occupied_slots">>,
    #{ <<"start_time">> => #{type => "string", example => "2021-04-10T18:24:31Z"}
     , <<"end_time">>   => #{type => "string", example => "2021-04-10T18:24:31Z"}
     }
  ),
  Metadata = #{ get => swagger_doc_get()
              },
  {Path, Handler, Params} = lists:keyfind("/rooms", 1, cola_http:routes()),
  [trails:trail(Path, Handler, Params, Metadata)].

%%%===================================================================
%%% Internal functions
%%%===================================================================
