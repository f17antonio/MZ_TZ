-module(mztz_web_callback).

-behaviour(elli_handler).

-include_lib("elli/include/elli.hrl").

-export([handle/2, handle_event/3]).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle(_Method, [], _) ->
    {ok, [], <<"Hello World !!!">>}.
    %{ok, [], to_json({ok, {[{ok, <<"Hello World !!!">>}]}})}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(request_complete, _Data, _Args) ->
    ok;
handle_event(request_closed, _Data, _Args) ->
    ok;
handle_event(request_timeout, _Data, _Args) ->
    ok;
handle_event(elli_startup, _Data, _Args) ->
    ok;
handle_event(Event, Data, Args) ->
    io:format("Event: ~p~nData: ~p~nArgs: ~p~n", [Event, Data, Args]),
    ok.

to_json({ok, Value}) ->
    jiffy:encode(Value);
to_json({error, Value}) ->
    jiffy:encode(Value).
