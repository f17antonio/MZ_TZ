-module(mztz_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(child(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).

-ifdef(TEST).
-define(elli_port, 8041).
-else.
-define(elli_port, 8040).
-endif.

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
    LightsManagerSrv = ?child(lights_manager_srv),
    DbSrv = ?child(db_srv),
    {ok, { {one_for_one, 5, 10}, [
        DbSrv,
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
        {elli, start_link, [[{port, ?elli_port},
            {callback, elli_middleware},
            {callback_args, MiddlewareConfig},
            {name, {local, elli}}]]},
        permanent, 5000, worker, [elli]}.
