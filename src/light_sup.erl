-module (light_sup).
-behaviour (supervisor).
-export ([start_link/0, light_start/2]).
-export ([init/1]).

-define(child(I), {I, {I, start_link, []}, temporary, 5000, worker, [I]}).

start_link() ->
    supervisor:start_link( ?MODULE, [] ).

init(_Args)  ->
    LightSrv = ?child(light_srv),
    {ok, { {simple_one_for_one, 0, 1}, [
        LightSrv
    ] }}.

light_start(QSup, Args) ->
    supervisor:start_child(QSup,  [Args]).
