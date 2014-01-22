-module(ecas_cookie).
-author('Adolfo Perez Alvarez <adolfo.pa@gmail.com>').

-export([get_tgc/1, set_tgc/3, delete/1]).

-define(COOKIE_NAME, "ecas_tgc").

get_tgc(ReqData) ->
    case wrq:get_cookie_value(?COOKIE_NAME, ReqData) of
        undefined ->
            undefined;
        Cookie ->
            ecas_ticket:fetch_tgc(Cookie)
    end.

set_tgc(ReqData, Username, Options) ->
    ecas_util:set_cookie(ReqData, ?COOKIE_NAME, ecas_ticket:new_tgc(Username), Options).

delete(ReqData) ->
    ecas_util:set_cookie(ReqData, ?COOKIE_NAME, "", [{max_age, 0}]).
