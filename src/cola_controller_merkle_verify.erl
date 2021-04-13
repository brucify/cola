-module(cola_controller_merkle_verify).

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
   , description => "Verifies a merkle proof"
   , requestBody =>
       #{ description => "Verifies a merkle proof"
        , content => #{ 'application/json' => #{ schema => cowboy_swagger:schema(<<"post_merkle_verify_request">>)}}
        }
   , responses =>
       #{ <<"200">> =>
          #{ description => "200 OK"
           , content => #{ 'application/json' => #{schema => cowboy_swagger:schema(<<"post_merkle_verify_response">>)}}
           }
        }
   }.
post(Params, #state{}) ->
  Room  = cola_conversion:to_list(proplists:get_value(room, Params)),
  Proof = cola_conversion:to_list(proplists:get_value(proof, Params)),
  Hash  = cola_conversion:to_list(proplists:get_value(hash, Params)),
  Result = case cola_worker_crypto:verify_proof(Hash, Proof, Room) of
             ok -> true;
             _  -> false
           end,
  {continue, #{ result => Result }}.

%%%===================================================================
%%% Swagger hook
%%%===================================================================

trails() ->
  ok = cowboy_swagger:add_definition(<<"post_merkle_verify_request">>,
    #{ <<"room">>  => #{ type => "string", required => "true", example => "C01" }
     , <<"proof">> => #{ type => "string", required => "true" }
     , <<"hash">>  => #{ type => "string", required => "true" }
     }
  ),
  ok = cowboy_swagger:add_definition(<<"post_merkle_verify_response">>,
    #{ <<"result">> => #{ type => "boolean", required => "true"}}
  ),
  Metadata = #{ post => swagger_doc_post()
              },
  {Path, Handler, Params} = lists:keyfind("/merkle/verify", 1, cola_http:routes()),
  [trails:trail(Path, Handler, Params, Metadata)].
