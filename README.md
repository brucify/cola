cola
=====

Co-located Organization Logistics API (COLA)

An Erlang application based on cowboy http server

Build
-----

    $ rebar3 compile

Running
-----

    $ rebar3 shell

The app will be running at https://localhost:8443 as configured in `config/dev/sys.config`. 

The Swagger UI will be served at https://localhost:8443/api-docs/. 

Testing against localhost using the curl scripts in `scripts`:

    HOST=https://localhost:8443 ./post_bookings_coke | jq
    HOST=https://localhost:8443 ID=fa293f3f-8fca-478e-b718-159ff669d85f ./get_bookings_id_coke | jq
    HOST=https://localhost:8443 ID=fa293f3f-8fca-478e-b718-159ff669d85f ./delete_bookings_id_coke | jq

Switching between public and private mode:

    1> cola_permission_worker:public_mode().
    ok
    2> cola_permission_worker:current_mode().
    public
    3> cola_permission_worker:reset().
    ok
    4> cola_permission_worker:current_mode().
    private

Running eunit tests 
-----

    $ rebar3 eunit

Certificates
-----

Generate and self-sign server certificate (RSA). This is used for hosting over HTTPS:

    $ openssl req -x509 -newkey rsa:2048 -keyout server.key -out server.crt -nodes -days 365 -subj "/CN=localhost/O=Client\ Certificate\ Demo"

Generate and client certificates (RSA) and sign using server's public key. These are used for sending the HTTP request: 

    $ openssl req -newkey rsa:2048 -keyout coke.key -out coke.csr
    $ openssl req -newkey rsa:2048 -keyout pepsi.key -out pepsi.csr
    $ openssl x509 -req -CA server.crt -CAkey server.key -in coke.csr -out coke.crt -set_serial 02 -days 365
    $ openssl x509 -req -CA server.crt -CAkey server.key -in pepsi.csr -out pepsi.crt -set_serial 02 -days 365

Wrap client certificates in PKCS12 to be used in browsers:

    $ openssl pkcs12 -export -clcerts -in coke.crt -inkey coke.key -out coke.p12
    $ openssl pkcs12 -export -clcerts -in pepsi.crt -inkey pepsi.key -out pepsi.p12

Server's EC key, used to issue ECDSA signature:

    $ openssl ecparam -genkey -name secp256r1 -noout -out server_ec.key
    $ openssl ec -in server_ec.key -pubout -out server_ec.pub
