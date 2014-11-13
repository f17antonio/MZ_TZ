-module(mztz_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).
-define(CHILDSUP(S), {S, {S, start_link, []}, transient, infinity, supervisor, [S]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    Elli   = webserver(),
    LightsManagerSrv = ?CHILD(lights_manager_srv),
    {ok, { {one_for_one, 5, 10}, [
        LightsManagerSrv,
        Elli
    ]}}.

webserver() ->
    MiddlewareConfig =
        [{mods, [
            {elli_access_log, []},
            {mztz_web_callback, []}
        ]}],

    {webserver,
        {elli, start_link, [[{port, 8040},
            {callback, elli_middleware},
            {callback_args, MiddlewareConfig},
            {name, {local, elli}}]]},
        permanent, 5000, worker, [elli]}.
