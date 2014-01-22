-module(service_validate_resource).
-author("Adolfo Perez Alvarez <adolfo.pa@gmail.com>").

-export([init/1, allowed_methods/2, content_types_provided/2, to_xml/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

allowed_methods(ReqData, State) ->
    {['GET', 'HEAD'], ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"application/xml", to_xml}], ReqData, State}.

to_xml(ReqData, State) ->
    [ServiceURL, Ticket, PgtURL, Renew] =
        lists:map(fun (X) -> wrq:get_qs_value(X, ReqData) end,
                  ["service", "ticket", "pgtUrl", "renew"]),
    case {ServiceURL, Ticket} of
        {undefined, _} ->
            error_response("INVALID_REQUEST", ReqData, State);
        {_, undefined} ->
            error_response("INVALID_REQUEST", ReqData, State);
        _ ->
            validate_st(ServiceURL, Ticket, ReqData, State)
    end.

error_response(Code, ReqData, State) ->
    XML = "<cas:serviceResponse xmlns:cas='http://www.yale.edu/tp/cas'>"
       ++ "<cas:authenticationFailure code='" ++ Code ++ "'>"
       ++ Code
       ++ "</cas:authenticationFailure>"
       ++ "</cas:serviceResponse>",
    {XML, ReqData, State}.

validate_st(ServiceURL, Ticket, ReqData, State) ->
    case ecas_ticket:validate_st(ServiceURL, Ticket) of
        {ok, {Username, SSO}} ->
            case {wrq:get_qs_value("renew", ReqData), SSO} of
                {undefined, _} ->
                    success_response(Username, ReqData, State);
                {_Any, true} ->
                    error_response("INVALID_TICKET", ReqData, State);
                _ ->
                    success_response(Username, ReqData, State)
            end;
        {error, invalid_st} ->
            error_response("INVALID_TICKET", ReqData, State)
    end.

success_response(Username, ReqData, State) ->
    XML = "<cas:serviceResponse xmlns:cas='http://www.yale.edu/tp/cas'>"
       ++ "<cas:authenticationSuccess>"
       ++ "<cas:user>" ++ Username ++ "</cas:user>"
       ++ "</cas:authenticationSuccess>"
       ++ "</cas:serviceResponse>",
    {XML, ReqData, State}.
