%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2021 14:30
%%%-------------------------------------------------------------------
-module(cola_authorization).

%% API
-export([ check_cert/1
        ]).

-include_lib("public_key/include/OTP-PUB-KEY.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec check_cert(CertDer) -> Result
  when CertDer :: public_key:der_encoded() | undefined,
       Result  :: {true, Client::atom()} | false.
check_cert(undefined) ->
  false;
check_cert(CertDer) ->
  Cert   = public_key:der_decode('Certificate', CertDer),
  PubKey = public_key(Cert),
  Hash   = unicode:characters_to_list(base64:encode(crypto:hash(sha256, PubKey))),
  io:format(user, "Hash: ~p~n", [Hash]),
  case lists:keyfind(Hash, 1, client_pubkey_hash()) of
    {_, Client} -> {true, Client};
    false       -> false
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

public_key(Cert) ->
  Cert#'Certificate'.tbsCertificate#'TBSCertificate'.subjectPublicKeyInfo#'SubjectPublicKeyInfo'.subjectPublicKey.

client_pubkey_hash() ->
  {ok, Certs} = application:get_env(cola, client_pubkey_hash),
  Certs.