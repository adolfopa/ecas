-module(ecas_auth).
-author('Adolfo Perez Alvarez <adolfo.pa@gmail.com>').

-export([login/3, succeed/2, fail/2]).

login({M, F}, Username, Password) ->
    erlang:apply(M, F, [Username, Password]).

succeed(_Username, _Password) ->
    true.

fail(_Username, _Password) ->
    false.
