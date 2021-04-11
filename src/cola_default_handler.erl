-module(cola_default_handler).

-behavior(cowboy_rest).

%% cowboy callbacks
-export([ init/2
        , allowed_methods/2
        , is_authorized/2
        , content_types_provided/2
        , content_types_accepted/2
        , delete_resource/2
        ]).

%% my callbacks
-export([ handle_get/2
        , handle_put/2
        , handle_post/2
        , handle_delete/2
        ]).

-export([ request_schema_name/2 ]).

-include("cola_default_handler.hrl").

%%%===================================================================
%%% Cowboy REST callbacks
%%%===================================================================

init(Req, [ControllerModule, AllowedMethods]) ->
  {cowboy_rest, Req, #state{ controller_module = ControllerModule
                           , allowed_methods = AllowedMethods
                           }}.

allowed_methods(Req, #state{allowed_methods = AllowedMethods}=State) ->
  {get_allowed_methods(AllowedMethods), Req, State}.


is_authorized(Req, State) ->
  case cola_authorization:check_cert(cowboy_req:cert(Req)) of
    {true, Client} ->
      io:format(user, "Client: ~p~n", [Client]),
      {true, Req, State#state{client = Client}};
    _ ->
      {{false, <<"Basic realm=\"cola\"">>}, Req, State}
  end.

%%
%% GET and HEAD methods
%%
content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_get}], Req, State}.

%%
%% PUT, POST and PATCH methods
%%
content_types_accepted(#{method:=<<"PUT">>}=Req, State) ->
  {[{<<"application/json">>, handle_put}], Req, State};
content_types_accepted(#{method:=<<"POST">>}=Req, State) ->
  {[ {<<"application/json">>, handle_post}
   , {<<"application/x-www-form-urlencoded">>, handle_post}
   , {<<"text/plain">>, handle_post}
   ], Req, State}.

%%
%% DELETE method
%%
delete_resource(Req0, #state{controller_module = _Controller}=State) ->
  handle_delete(Req0, State).

%%%===================================================================
%%% Custom Cowboy REST callbacks
%%%===================================================================

handle_get(Req, State)->
  fold_steps(Req, State, [ fun parse_path_params/2
                         , fun parse_query_params/2
                         , fun parse_body_params/2
                         , fun type_check_params/2
                         , fun call_controller/2
                         ]).

handle_put(#{path:=_Path}=Req, State)->
  fold_steps(Req, State, [ fun parse_path_params/2
                         , fun parse_query_params/2
                         , fun parse_body_params/2
                         , fun type_check_params/2
                         , fun call_controller/2
                         ]).

handle_post(#{path:=_Path}=Req, State)->
  fold_steps(Req, State, [ fun parse_path_params/2
                         , fun parse_query_params/2
                         , fun parse_body_params/2
                         , fun type_check_params/2
                         , fun call_controller/2
                         ]).

handle_delete(#{path:=_Path}=Req, State)->
  fold_steps(Req, State, [ fun parse_path_params/2
                         , fun parse_query_params/2
                         , fun parse_body_params/2
                         , fun type_check_params/2
                         , fun call_controller/2
                         ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec request_schema_name(Module, Method) -> Name
  when Module :: atom(),
       Method :: binary(),
       Name   :: binary().
request_schema_name(Module, Method) ->
  <<(atom_to_binary(Module,latin1))/binary, "_", Method/binary, "_request">>.

%%%===================================================================
%%% Steps
%%%===================================================================

call_controller(#{method := Method}= Req0, #state{ controller_module = Controller
                                                 , request_params = ReqParams
                                                 , request_body = Body
                                                 }=State) ->
  Fun = method_to_atom(Method),
  Params = maybe_add_plain_text_body(Body, ReqParams, Req0),

  try Controller:Fun(Params, State) of
    {continue, RespBody} ->
      EncodedBody = cola_json:encode(RespBody),
      Req1 = cowboy_req:set_resp_headers(#{}, Req0),
      case Method of
        <<"GET">> -> {EncodedBody, Req1, State};
        _Other    -> {true, cowboy_req:set_resp_body(EncodedBody, Req1), State}
      end;
    {Status, RespBody, RespHeaders} ->
      Req1 = do_cowboy_reply(Req0, Status, RespBody, RespHeaders),
      {stop, Req1, State}
  catch
    exit:400 ->
      Req1 = do_cowboy_reply(Req0, 400, <<"Bad request">>, #{}),
      {stop, Req1, State};
    exit:{Status, Msg} ->
      Req1 = do_cowboy_reply(Req0, Status, Msg, #{}),
      {stop, Req1, State}
  end.

type_check_params(#{method:=_Method}=Req, #state{ controller_module=_Mod
                                               , raw_request_body=_Json
                                               , request_params = Params
                                               }=State) ->
  {continue, Req, State#state{request_params = Params}}.

parse_body_params(#{}=Req0, #state{request_params = Params}=State) ->
  {ok, BodyBin, Req1} = read_body(Req0),
  case maybe_decode_body_json(BodyBin, Req1) of
    {error, _Reason} ->
      Req2 = do_cowboy_reply(Req1, 400, <<"Invalid json">>, #{}),
      {stop, Req2, State};
    {ok, DecodedBody} when is_map(DecodedBody) ->
      BodyParams = atomize_keys(maps:to_list(DecodedBody)),
      {continue, Req0, State#state{request_params = BodyParams++Params, raw_request_body = BodyBin}};
    {ok, DecodedBody} ->
      {continue, Req0, State#state{request_body = DecodedBody, raw_request_body = BodyBin}}
  end.

parse_path_params(#{bindings := Bindings}=Req0, #state{request_params = Params}=State) ->
  PathParams = atomize_keys(maps:to_list(Bindings)),
  {continue, Req0, State#state{request_params = PathParams++Params}}.

parse_query_params(Req, #state{request_params = Params}=State) ->
  AtomsQs = atomize_keys(cowboy_req:parse_qs(Req)),
  {continue, Req, State#state{request_params = AtomsQs++Params}}.

%%%===================================================================
%%% Internal
%%%===================================================================

fold_steps(Req0, State0, AllFuns) ->
  lists:foldl(
    fun(Fun, {continue, Req, State}) -> Fun(Req, State);
       ([],  {continue, Req, State}) -> {true, Req, State};
       (_Fun,          CowboyReturn) -> CowboyReturn
    end,
    {continue, Req0, State0},
    AllFuns
  ).

do_cowboy_reply(Req0, Status, RespBody, Headers) ->
  Req1 = cowboy_req:reply( Status
                         , Headers#{<<"content-type">> => <<"application/json">>}
                         , cola_json:encode(RespBody)
                         , Req0
                         ),
  Req1.

-spec get_allowed_methods([atom()]) -> [binary()].
get_allowed_methods(Atoms) ->
  lists:map(fun('GET')    -> <<"GET">>;
               ('POST')   -> <<"POST">>;
               ('PUT')    -> <<"PUT">>;
               ('DELETE') -> <<"DELETE">>
            end,
    Atoms).

method_to_atom(<<"GET">>)    -> get;
method_to_atom(<<"POST">>)   -> post;
method_to_atom(<<"PUT">>)    -> put;
method_to_atom(<<"DELETE">>) -> delete.

atomize_keys(PropList) ->
  lists:map(fun({K, V}) when is_atom(K) -> {K, V};
               ({K, V})                 -> {binary_to_existing_atom(K, latin1), V}
            end, PropList).

read_body(Req0) ->
  case read_body(Req0, <<>>) of
    {ok, <<>>, Req1}    -> {ok, <<"{}">>, Req1};
    {ok, BodyBin, Req1} -> {ok, BodyBin, Req1}
  end.

%% Read the entire body
read_body(Req0, Acc) ->
  case cowboy_req:read_body(Req0) of
    {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
    {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
  end.

-spec maybe_decode_body_json(binary(), cowboy_req:req()) -> {ok, map() | binary()} | {error, term()}.
maybe_decode_body_json(BodyBin, Req0) ->
  case cowboy_req:header(<<"content-type">>, Req0) of
    <<"application/json">> -> cola_json:decode(BodyBin);
    <<"text/plain">>       -> {ok, BodyBin};
    undefined              -> {ok, BodyBin}
  end.

maybe_add_plain_text_body(Body, ReqParams, Req0) ->
  case cowboy_req:header(<<"content-type">>, Req0) of
    undefined              -> ReqParams;
    <<"application/json">> -> ReqParams;
    <<"text/plain">>       -> [{body, Body}| ReqParams]
  end.