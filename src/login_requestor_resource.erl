-module(login_requestor_resource).
-author('Adolfo Perez Alvarez <adolfo.pa@gmail.com>').

-export([init/1, allowed_methods/2, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

allowed_methods(ReqData, State) ->
    {['GET', 'HEAD'], ReqData, State}.

to_html(ReqData, State) ->
    case {ecas_cookie:get_tgc(ReqData), renew(ReqData)} of
        {{_TGC, Username}, false} ->
            login_automatically(Username, ReqData, State);
        _ ->
            case wrq:get_qs_value("gateway", ReqData) of
                undefined ->
                    show_login_page(ReqData, State);
                _ ->
                    send_to_service(ReqData, State)
            end
    end.

renew(ReqData) ->
    wrq:get_qs_value("renew", ReqData) =/= undefined.

login_automatically(Username, ReqData, State) ->
   case wrq:get_qs_value("service", ReqData) of
        undefined ->
           {ecas_util:render(login_success_dtl), ReqData, State};
        ServiceURL->
           Ticket = ecas_ticket:new_st(ServiceURL, Username, sso),
           TargetURL = ecas_util:add_q(ServiceURL, ticket, Ticket),
           ecas_util:redirect(TargetURL, ReqData, State)
    end.

show_login_page(ReqData, State) ->
    Env = [{lt, ecas_ticket:new_lt()},
           {service, wrq:get_qs_value("service", "", ReqData)},
           {warn, wrq:get_qs_value("warn", "", ReqData)}],
    {ecas_util:render(login_dtl, Env), ReqData, State}.

send_to_service(ReqData, State) ->
    case wrq:get_qs_value("service", ReqData) of
        undefined ->
            show_login_page(ReqData, State);
        ServiceURL ->
            ecas_util:redirect(ServiceURL, ReqData, State)
    end.
