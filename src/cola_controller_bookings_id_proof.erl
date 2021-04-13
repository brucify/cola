-module(cola_controller_bookings_id_proof).

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
  #{ tags => ["bookings"]
   , description => "Gets the merkle proof for a booking ID"
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
           , content => #{ 'application/json' => #{schema => cowboy_swagger:schema(<<"get_bookings_id_proof_response">>)}}
           }
        }
   }.
get(Params, #state{client = Client}) ->
  Id = cola_conversion:to_list(proplists:get_value(id, Params)),
  Result = #{ proof => cola_worker_crypto:gen_proof(Id, Client) },
  {continue, Result}.

%%%===================================================================
%%% Swagger hook
%%%===================================================================

trails() ->
  ok = cowboy_swagger:add_definition(<<"get_bookings_id_proof_response">>,
    #{ <<"proof">> => #{ type => "string", required => "true"}}
  ),
  Metadata = #{ get => swagger_doc_get()
              },
  {Path, Handler, Params} = lists:keyfind("/bookings/:id/proof", 1, cola_http:routes()),
  [trails:trail(Path, Handler, Params, Metadata)].
