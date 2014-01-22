-module(ecas_seq_server).
-author("Adolfo Perez Alvarez <adolfo.pa@gmail.com>").

-behaviour(gen_server).

-export([start_link/0, seq_next/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(INITIAL_VALUE, 1).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, ?INITIAL_VALUE, []).

seq_next() ->
    {ok, NextVal} = gen_server:call(?SERVER, nextval),
    NextVal.

init(Args) ->
    {ok, Args}.

handle_call(nextval, _From, State) ->
    {reply, {ok, State}, State + 1}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
