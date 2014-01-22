-module(logout_resource).
-author('Adolfo Perez Alvarez <adolfo.pa@gmail.com>').

-export([init/1, allowed_methods/2, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

allowed_methods(ReqData, State) ->
    {['GET'], ReqData, State}.

to_html(ReqData, State) ->
    URL = wrq:get_qs_value("url", "", ReqData),
    {ecas_util:render(logout_dtl, [{url, URL}]), ecas_cookie:delete(ReqData), State}.
