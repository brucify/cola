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
      Bookings = cola_bookings:all_bookings(Room),
      Occupied = [#{ start_time => StartTime, end_time => EndTime} || {_R, StartTime, EndTime} <- Bookings],
      #{ name     => Room
       , occupied => Occupied
       }
    end,
    Rooms
  ),
%%  Result =
%%    [ #{ name => <<"C01">>
%%       , occupied =>
%%          [ #{ start_time => <<"2021-04-10T18:24:31Z">>
%%             , end_time => <<"2021-04-10T19:24:31Z">>
%%             }
%%          ]
%%       }
%%    , #{ name => <<"C02">>
%%       , occupied =>
%%          [ #{ start_time => <<"2021-04-10T18:24:31Z">>
%%             , end_time => <<"2021-04-10T19:24:31Z">>
%%             }
%%          , #{ start_time => <<"2021-04-10T19:24:31Z">>
%%             , end_time => <<"2021-04-10T20:24:31Z">>
%%             }
%%          ]
%%       }
%%    , #{ name => <<"P01">>
%%       , occupied => []
%%       }
%%    ],
  {continue, Result}.

%%%===================================================================
%%% Swagger hook
%%%===================================================================

trails() ->
  ok = cowboy_swagger:add_definition_array(<<"all_rooms">>,
    #{ name       => #{type => "string"}
     , occupied   => cowboy_swagger:schema(<<"occupied_slots">>)
     }
  ),
  ok = cowboy_swagger:add_definition_array(<<"occupied_slots">>,
    #{ start_time => #{type => "string", example => "2021-04-10T18:24:31Z"}
     , end_time   => #{type => "string", example => "2021-04-10T18:24:31Z"}
     }
  ),
  Metadata = #{ get => swagger_doc_get()
              },
  {Path, Handler, Params} = lists:keyfind("/rooms", 1, cola_http:routes()),
  [trails:trail(Path, Handler, Params, Metadata)].

%%%===================================================================
%%% Internal functions
%%%===================================================================
