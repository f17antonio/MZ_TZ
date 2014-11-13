-module(light_srv).
-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2]).
% Unused Funcs
-export([handle_info/2, terminate/2, code_change/3]).

start_link(_) ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    {ok, self()}.

handle_call(start_test, _From, State) ->
    io:format("Its WORK pid: ~p~n", [self()]),
    {reply, ok, State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
