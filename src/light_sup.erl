-module (light_sup).
-behaviour (supervisor).
-export ([start_link/0, light_start/2]).
-export ([init/1]).

-define(CHILD(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).

start_link() ->
    supervisor:start_link( ?MODULE, [] ).

init(_Args)  ->
    LightSrv = ?CHILD(light_srv),
    {ok, { {simple_one_for_one, 0, 1}, [
        LightSrv
        %{task, {?MODULE, task_start_link, [ M, F, A ]}, temporary, brutal_kill, worker, [?MODULE, M]}
    ] }}.

light_start( QSup, Args ) ->
    supervisor:start_child( QSup, [ Args ] ).
