-module(ecas_msg).
-author("Adolfo Perez Alvarez <adolfo.pa@gmail.com>").

%% TODO: user gettext to manage the messages.
%% TODO: use the browser language to retrieve the correct message.

-export([get_msg/1]).

get_msg(MsgId) ->
    %% TODO
    %% This is a quick and dirty placeholder!
    case MsgId of
        invalid_principal ->
            "Both username and password are required fields.";
        invalid_lt ->
            "Found invalid login ticket.";
        auth_fail ->
            "No valid account found for the given username and password."
    end.
