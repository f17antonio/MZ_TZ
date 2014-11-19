-module(lights_manager_srv).
-behaviour(gen_server).

-export([start_link/0]).

-export([
    create_light/0,
    create/1,
    add_observation/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    sequences_map = maps:new(),
    light_sup
}).

add_observation({error, Reason}) ->
    {error, Reason};
add_observation({[
    {<<"observation">>, {[
        {<<"color">>, <<"green">>},
        {<<"numbers">>, Numbers}]
    }}, {<<"sequence">>, Seq}]}) ->
    gen_server:call(lights_manager_srv, {add, Seq, <<"green">>, Numbers});
add_observation({[{<<"observation">>, {[{<<"color">>, <<"red">>}]}}, {<<"sequence">>, Seq}]}) ->
    gen_server:call(lights_manager_srv, {add, Seq, <<"red">>, []});
add_observation(_) ->
    {error, <<"Invalid Response Data">>}.

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
    {reply, Response, State#state{sequences_map = SequensesNewMap}};

handle_call({add, Seq, Color, Numbers}, From, State = #state{sequences_map = SequensesMap}) ->
    LPidTuple = maps:find(Seq, SequensesMap),
    request_to_light_server(LPidTuple, Color, Numbers, From),
    {noreply, State}.

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
    {Seq, Pid}.

request_to_light_server(error, _, _, _) ->
    {error, <<"The sequence isn't found">>};
request_to_light_server({ok, LPid}, Color, Numbers, From) ->
    gen_server:cast(LPid, {add_observation, Color, Numbers, From}).
