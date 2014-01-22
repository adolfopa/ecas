-module(ecas_ticket).
-author('Adolfo Perez Alvarez <adolfo.pa@gmail.com>').

-export([new_lt/0, invalidate_lt/1, new_st/3, new_tgc/1, fetch_tgc/1, validate_st/2]).

new_lt() ->
    LoginTicket = new_ticket(<<"LT-">>),
    ecas_tstore_server:store(login_ticket, LoginTicket),
    LoginTicket.

invalidate_lt(Ticket) when is_binary(Ticket) ->
    ecas_tstore_server:delete(login_ticket, Ticket);
invalidate_lt(Ticket) when is_list(Ticket) ->
    invalidate_lt(list_to_binary(Ticket)).

new_st(ServiceURL, Username, LoginType) ->
    SSO = case LoginType of sso -> true; credentials -> false end,
    ServiceTicket = new_ticket(<<"ST-">>),
    ecas_tstore_server:store(service_ticket, ServiceTicket, [{service, ServiceURL}, {username, Username}, {sso, SSO}]),
    ServiceTicket.

validate_st(_ServiceURL, Ticket) when is_binary(Ticket) ->
    %% TODO: Validate that the service url is bound to the ticket
    case ecas_tstore_server:fetch(service_ticket, Ticket) of
        {ok, {Ticket, Props}} ->
            ecas_tstore_server:delete(service_ticket, Ticket),
            {ok, {proplists:get_value(username, Props), proplists:get_value(sso, Props)}};
        _ ->
            {error, invalid_st}
    end;
validate_st(ServiceURL, Ticket) when is_list(Ticket) ->
    validate_st(ServiceURL, list_to_binary(Ticket)).

new_tgc(Username) ->
    TGC = new_ticket(<<"TGC-">>),
    ecas_tstore_server:store(cookie, TGC, [{username, Username}]),
    TGC.

fetch_tgc(TGC) when is_binary(TGC) ->
    case ecas_tstore_server:fetch(cookie, TGC) of
        {ok, {TGC, Props}} ->
            {TGC, proplists:get_value(username, Props)};
        _ ->
            undefined
    end;
fetch_tgc(TGC) when is_list(TGC) ->
    fetch_tgc(list_to_binary(TGC)).

new_ticket(Prefix) ->
    {Num, Str} = {next_int(), list_to_binary(random_string())},
    <<Prefix/binary, Num/binary, "-", Str/binary >>.

-define(MAX_RAND_STRING_SIZE, 35).
-define(LETTERS_AND_DIGITS, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012345679").

random_string() ->
    ChLen = length(?LETTERS_AND_DIGITS),
    lists:map(fun (X) ->
		      lists:nth(X rem ChLen + 1, ?LETTERS_AND_DIGITS)
	      end,
	      binary_to_list(crypto:rand_bytes(?MAX_RAND_STRING_SIZE))).

next_int() ->
    list_to_binary(integer_to_list(ecas_seq_server:seq_next())).
