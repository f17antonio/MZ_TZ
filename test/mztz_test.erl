-module(mztz_test).

-include_lib("eunit/include/eunit.hrl").
-include("./include/mztz_test.hrl").

run_test_()->
    {setup,
        fun mztz_setup/0, fun mztz_cleanup/1,
        {inorder,
            [
                {"create_light", fun create_light/0},
                {"repair_data_after_restart", fun repair_data/0},
                {"add_observation", fun add_observation/0},
                {"clear_data", fun clear_data/0},
                {"add_red_first", fun add_red_first/0},
                {"incorrect_sequense", fun incorrect_sequense/0},
                {"incorrect_nums", fun incorrect_nums/0},
                {"no_solution_found", fun no_solution_found/0},
                {"invalid_response", fun invalid_response/0},
                {"add_consecutive_observations", fun add_cons_observations/0}
            ]
        }
    }.

mztz_setup() ->
    ok = application:start(mztz).
mztz_cleanup(_) ->
    ok = application:stop(mztz).

create_light() ->
    ?assertNot(undefined == whereis(mztz_sup)),
    ?assertMatch({ok,{[{sequence, _Seq}]}}, lights_manager_srv:create_light()).

add_observation() ->
    {ok,{[{sequence, Seq}]}} = lights_manager_srv:create_light(),
    Result = lights_manager_srv:add_observation(?green_data_template([?num0, ?num8], Seq)),
    ?assertMatch({ok,{[{start, _DigList}, {missing, _MisList}]}}, Result).

clear_data() ->
    {ok,{[{sequence, Seq}]}} = lights_manager_srv:create_light(),
    {ok, _} = lights_manager_srv:add_observation(?green_data_template([?num0, ?num3], Seq)),
    {ok, _} = lights_manager_srv:clear(),
    Result = lights_manager_srv:add_observation(?green_data_template([?num0, ?num2], Seq)),
    ?assertEqual({error,<<"The sequence isn't found">>}, Result).

add_red_first() ->
    {ok,{[{sequence, Seq}]}} = lights_manager_srv:create_light(),
    Result = lights_manager_srv:add_observation(?red_data_template(Seq)),
    ?assertEqual({error,<<"There isn't enough data">>}, Result).

repair_data() ->
    {ok,{[{sequence, Seq}]}} = lights_manager_srv:create_light(),
    {ok, _} = lights_manager_srv:add_observation(?green_data_template([?num0, ?num1], Seq)),
    ok = application:stop(mztz),
    ok = application:start(mztz),
    Result = lights_manager_srv:add_observation(?red_data_template(Seq)),
    ?assertMatch({ok,{[{start, _DigList}, {missing, _MisList}]}}, Result).

incorrect_sequense() ->
    Seq = <<"incorrect sequense">>,
    Result = lights_manager_srv:add_observation(?green_data_template([?num0, ?num1], Seq)),
    ?assertEqual({error,<<"The sequence isn't found">>}, Result).

incorrect_nums() ->
    {ok,{[{sequence, Seq}]}} = lights_manager_srv:create_light(),
    Result = lights_manager_srv:add_observation(?green_data_template([?num0, <<"incorrect_num">>], Seq)),
    ?assertEqual({error,<<"There isn't enough data">>}, Result).

no_solution_found() ->
    {ok,{[{sequence, Seq}]}} = lights_manager_srv:create_light(),
    {ok, _} = lights_manager_srv:add_observation(?green_data_template([?num8, ?num8], Seq)),
    Result = lights_manager_srv:add_observation(?green_data_template([?num9, ?num9], Seq)),
    ?assertEqual({error,<<"No solutions found">>}, Result).

invalid_response() ->
    Result = lights_manager_srv:add_observation(<<"invalid_response">>),
    ?assertEqual({error,<<"Invalid Response Data">>}, Result).

add_cons_observations() ->
    {ok,{[{sequence, Seq}]}} = lights_manager_srv:create_light(),
    {ok, _} = lights_manager_srv:add_observation(?green_data_template([?num0, ?num8], Seq)),
    {ok, _} = lights_manager_srv:add_observation(?green_data_template([?num0, ?num7], Seq)),
    {ok, _} = lights_manager_srv:add_observation(?green_data_template([?num0, ?num6], Seq)),
    {ok, _} = lights_manager_srv:add_observation(?green_data_template([?num0, ?num5], Seq)),
    Result = lights_manager_srv:add_observation(?green_data_template([?num0, ?num4], Seq)),
    ?assertMatch({ok,{[{start, _DigList}, {missing, _MisList}]}}, Result).
