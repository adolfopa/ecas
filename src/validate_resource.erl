-module(validate_resource).
-author("Adolfo Perez Alvarez <adolfo.pa@gmail.com>").

-export([init/1, allowed_methods/2, content_types_provided/2, process_post/2, to_text/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

allowed_methods(ReqData, State) ->
    {['POST', 'GET'], ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"text/plain", to_text}], ReqData, State}.

process_post(ReqData, State) ->
    Params = ecas_util:parse_post_params(ReqData),
    Renew = ecas_util:get_param("renew", Params),
    Body = validate_ticket(ecas_util:get_param("service", Params), ecas_util:get_param("ticket", Params), Renew =/= "", ReqData, State),
    {true, wrq:append_to_response_body(Body, ReqData), State}.

to_text(ReqData, State) ->
    Renew = wrq:get_qs_value("renew", "", ReqData),
    Body = validate_ticket(wrq:get_qs_value("service", ReqData), wrq:get_qs_value("ticket", ReqData), Renew =/= "", ReqData, State),
    {Body, ReqData, State}.

validate_ticket(ServiceURL, Ticket, Renew, ReqData, _State) ->
    case ecas_ticket:validate_st(ServiceURL, Ticket) of
        {ok, {_Username, true}} when Renew ->
            "no\n\n";
        {ok, {Username, _SSO}} ->
            "yes\n" ++ Username ++ "\n";
        {error, invalid_st} ->
            "no\n\n"
    end.
            
