-module(mztz_web_callback).

-behaviour(elli_handler).

-include_lib("elli/include/elli.hrl").

-export([handle/2, handle_event/3]).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle(_Method, [<<"sequence">>, <<"create">>], _Req) ->
    Out = lights_manager_srv:create_light(),
    {ok, [], to_json(Out)};

handle(_Method, [<<"clear">>], _Req) ->
    Out = lights_manager_srv:clear(),
    {ok, [], to_json(Out)};

handle('POST', [<<"observation">>, <<"add">>], Req) ->
    Data = from_json(elli_request:body(Req)),
    Out = lights_manager_srv:add_observation(Data),
    {ok, [], to_json(Out)};

handle(_, _, _Req) ->
    {ok, [], to_json({error, <<"Invalid Path">>})}.

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

to_json({ok, Response}) ->
    jiffy:encode({[{status, <<"ok">>}, {response, Response}]});
to_json({error, Msg}) ->
    jiffy:encode({[{status, <<"error">>}, {msg, Msg}]}).

from_json(Json) ->
    try jiffy:decode(Json) of
        Result -> Result
    catch
        error:_ ->
            {error, <<"Invalid JSON">>}
    end.
