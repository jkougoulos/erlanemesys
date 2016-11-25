erlanemesys
=====

An OTP application

Build
-----

    $ rebar3 compile

Because of procket you need to do the following:

setcap cap_net_raw=ep /usr/local/lib/erlang/erts-5.8.3/bin/beam.smp
