-module(login_acceptor_resource).
-author("Adolfo Perez Alvarez <adolfo.pa@gmail.com>").

-export([init/1, allowed_methods/2, process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, {auth, cookie_options}).

init([]) ->
    {ok, Auth} = application:get_env(auth_strategy),
    {ok, CookieOptions} = application:get_env(cookie),
    {ok, #state{auth=Auth, cookie_options=CookieOptions}}.

allowed_methods(ReqData, State) ->
    {['POST'], ReqData, State}.

process_post(ReqData, State) ->
    Params = ecas_util:parse_post_params(ReqData),
    case ecas_util:get_param("lt", Params) of
        "" ->
            show_login_page(Params, ReqData, State, [invalid_lt]);
        LoginTicket ->
            case ecas_ticket:invalidate_lt(LoginTicket) of
                ok ->
                    authenticate_user(Params, ReqData, State);
                {error, invalid_ticket} ->
                    show_login_page(Params, ReqData, State, [invalid_lt])
            end
    end.

authenticate_user(Params, ReqData, State=#state{auth=Auth, cookie_options=CookieOptions}) ->
    Principal = {ecas_util:get_param("username", Params), ecas_util:get_param("password", Params)},
    case Principal of
        {"", _} ->
            show_login_page(Params, ReqData, State, [invalid_principal]);
        {_, ""} ->
            show_login_page(Params, ReqData, State, [invalid_principal]);
        {Username, Password} ->
            case ecas_auth:login(Auth, Username, Password) of
                true ->
                    show_success_page(Params, Username, ecas_cookie:set_tgc(ReqData, Username, CookieOptions), State);
                false ->
                    show_login_page(Params, ReqData, State, [auth_fail])
            end
    end.

show_login_page(Params, ReqData, State, Errors) ->
    Env = [{lt, ecas_ticket:new_lt()},
           {service, ecas_util:get_param("service", Params)},
           {errors, lists:map(fun ecas_msg:get_msg/1, Errors)}],
    {true, ecas_util:render(login_dtl, Env, ReqData), State}.

show_success_page(Params, Username, ReqData, State) ->
    case {ecas_util:get_param("service", Params), ecas_util:get_param("warn", Params)} of
        {"", _Warn} ->
            {true, ecas_util:render(login_success_dtl, ReqData), State};
        {ServiceURL, Warn}->
            Ticket = ecas_ticket:new_st(ServiceURL, Username, credentials),
            TargetURL = ecas_util:add_q(ServiceURL, ticket, Ticket),
            case Warn of
                "" ->
                    ecas_util:redirect(TargetURL, ReqData, State);
                _ ->
                    {true, ecas_util:render(login_success_dtl, [{target, TargetURL}, {service, ServiceURL}, {warn, true}], ReqData), State}
            end
    end.

