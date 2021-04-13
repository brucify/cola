-module(cola_controller_merkle).

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
  #{ tags => ["merkle"]
   , description => "Gets the merkle tree root hash of all bookings for a room"
   , requestBody =>
       #{ description => "Gets the merkle tree root hash of all bookings for a room"
        , content => #{ 'application/json' => #{ schema => cowboy_swagger:schema(<<"post_merkle_request">>)}}
        }
   , responses =>
       #{ <<"200">> =>
          #{ description => "200 OK"
           , content => #{ 'application/json' => #{schema => cowboy_swagger:schema(<<"post_merkle_response">>)}}
           }
        }
   }.
post(Params, #state{}) ->
  Room = cola_conversion:to_list(proplists:get_value(room, Params)),
  Result = #{ root_hash => cola_worker_crypto:root_hash(Room) },
  {continue, Result}.

%%%===================================================================
%%% Swagger hook
%%%===================================================================

trails() ->
  ok = cowboy_swagger:add_definition(<<"post_merkle_request">>,
    #{ <<"room">> => #{ type => "string", required => "true", example => "C01" }}
  ),
  ok = cowboy_swagger:add_definition(<<"post_merkle_response">>,
    #{ <<"root_hash">> => #{ type => "string", required => "true"}}
  ),
  Metadata = #{ post => swagger_doc_post()
              },
  {Path, Handler, Params} = lists:keyfind("/merkle", 1, cola_http:routes()),
  [trails:trail(Path, Handler, Params, Metadata)].
