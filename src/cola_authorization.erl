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

check_cert(CertDer) ->
  Cert = public_key:der_decode('Certificate', CertDer),
  PubKey = Cert#'Certificate'.tbsCertificate#'TBSCertificate'.subjectPublicKeyInfo#'SubjectPublicKeyInfo'.subjectPublicKey,
  Hash = base64:encode(crypto:hash(sha256, PubKey)),
  io:format(user, "Hash: ~p~n", [Hash]).