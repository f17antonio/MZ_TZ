-module(lights_manager_srv).
-behaviour(gen_server).

-export([start_link/0]).

-export([create_light/0, create/1]).

-export([init/1, handle_call/3, handle_cast/2]).
% Unused Funcs
-export([handle_info/2, terminate/2, code_change/3]).

-record(state, {
    sequences_map = maps:new(),
    light_sup
}).

create_light() ->
    gen_server:call(lights_manager_srv, create).

start_link() ->
    gen_server:start_link({local, lights_manager_srv}, lights_manager_srv, [], []).

init(_Args) ->
    {ok, LSup} = light_sup:start_link(),
    {ok, #state{light_sup = LSup}}.

handle_call(create, _From, State = #state{sequences_map = SequensesMap, light_sup = LSup}) ->
    {Sequense, Pid} = create(LSup),
    SequensesNewMap = maps:put(Sequense, Pid, SequensesMap),
    Response = {ok, {[{sequence, Sequense}]}},
    {reply, Response, State#state{sequences_map = SequensesNewMap}}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

create(LSup) ->
    Seq = list_to_binary(uuid:to_string(uuid:uuid1())),
    {ok, Pid} = light_sup:light_start(LSup, []),
    gen_server:call(Pid, start_test),
    {Seq, Pid}.
