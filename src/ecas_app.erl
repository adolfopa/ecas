-module(ecas_app).
-author('Adolfo Perez Alvarez <adolfo.pa@gmail.com>').

-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    ecas_sup:start_link().

stop(_State) ->
    ok.
