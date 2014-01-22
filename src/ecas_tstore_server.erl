-module(ecas_tstore_server).
-author("Adolfo Perez Alvarez <adolfo.pa@gmail.com>").

-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

-export([start_link/0, store/2, store/3, delete/2, fetch/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

store(Kind, Ticket) ->
    store(Kind, Ticket, []).

store(Kind, Ticket, Props) ->
    gen_server:call(?SERVER, {store, {Kind, Ticket, Props}}).

delete(Kind, Ticket) ->
    gen_server:call(?SERVER, {delete, {Kind, Ticket}}).

fetch(Kind, Ticket) ->
    gen_server:call(?SERVER, {fetch, {Kind, Ticket}}).

-define(TABLE_NAME, tstore).
-define(INTERVAL, 10 * 60 * 1000). % Old tickets are purged each 10 minutes.

-record(state, {tid, ttl}).

init(_Args) ->
    TabId = ets:new(?TABLE_NAME, []),
    {ok, TTL} = application:get_env(ttl),
    erlang:send_after(?INTERVAL, self(), purge), % See handle_info below
    {ok, #state{tid=TabId, ttl=TTL}}.

handle_call({store, {Kind, Ticket, Props}}, _From, State=#state{tid=TabId, ttl=TTL}) ->
    true = ets:insert(TabId, {Ticket, {Kind, entry_ttl(Kind, TTL), Props}}),
    {reply, ok, State};
handle_call({delete, {_Kind, Ticket}}, _From, State=#state{tid=TabId}) ->
    Reply = case ets:lookup(TabId, Ticket) of
                [] ->
                    {error, invalid_ticket};
                _ -> 
                    true = ets:delete(TabId, Ticket),
                    ok
            end,
    {reply, Reply, State};
handle_call({fetch, {Kind, Ticket}}, _From, State=#state{tid=TabId}) ->
    case ets:match(TabId, {Ticket, {Kind, '_', '$1'}}) of
        [[X]] ->
            {reply, {ok, {Ticket, X}}, State};
        [] ->
            {reply, {error, no_tgc}, State};
        _ ->
            {reply, {error, duplicate_tgc}, State}
    end.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(purge, State=#state{tid=TabId}) ->
    Limit = current_micros(),
    ets:select_delete(TabId, ets:fun2ms(fun ({_, {_, T, _}}) when T =< Limit -> true end)),
    erlang:send_after(?INTERVAL, self(), purge),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

current_micros() ->
    {MegaSecs, Secs, USecs} = os:timestamp(), 
    (MegaSecs * 1000000 + Secs) * 1000000 + USecs.

entry_ttl(Kind, TTL) ->
    T = proplists:get_value(Kind, TTL),
    current_micros() + T.
    
