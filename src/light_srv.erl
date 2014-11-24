-module(light_srv).
-behaviour(gen_server).

-export([start_link/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(figures_associations, [
    {0, 2#1110111},
    {1, 2#0010010},
    {2, 2#1011101},
    {3, 2#1011011},
    {4, 2#0111010},
    {5, 2#1101011},
    {6, 2#1101111},
    {7, 2#1010010},
    {8, 2#1111111},
    {9, 2#1111011}
]).

-record(state, {
    seq = <<>>             :: bitstring(),
    iteration = 0          :: integer(),
    estimated_figures = [] :: list(),
    broken_sections = []   :: list(),
    first_bfigures = []    :: list()
}).

-define(db_table_name, light_states_data).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init([{seq, Seq}]) ->
    State = gen_server:call(db_srv, {select, ?db_table_name, Seq, #state{iteration = 1, seq = Seq}}),
    {ok, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({add_observation, <<"red">>, _, ReqPid}, State = #state{iteration = 1}) ->
    gen_server:reply(ReqPid, {error, <<"There isn't enough data">>}),
    {noreply, State};

handle_cast({add_observation, <<"red">>, _, ReqPid}, State = #state{iteration = I, first_bfigures = FBFs}) ->
    {Out, NewState} = get_light_data([I - 1], FBFs , 1, State),
    gen_server:reply(ReqPid, Out),
    {noreply, NewState};

handle_cast({add_observation, _, BFigures, ReqPid}, State = #state{iteration = 1}) ->
    Nums = lists:seq(1, lists:foldl(fun(X, Prod) -> X * Prod end, 1, lists:duplicate(length(BFigures), 10)) - 1),
    {Out, NewState} = get_light_data(Nums, BFigures, 1, State#state{first_bfigures = BFigures}),
    gen_server:reply(ReqPid, Out),
    {noreply, NewState};

handle_cast({add_observation, _, BFigures, ReqPid}, State = #state{iteration = I, estimated_figures = EFs} ) ->
    DecrEstimFigures = lists:map(fun(X) -> X - I + 1 end, EFs),
    {Out, NewState} = get_light_data(DecrEstimFigures, BFigures , I, State),
    gen_server:reply(ReqPid, Out),
    {noreply, NewState};

handle_cast(stop, State = #state{seq = Seq}) ->
    gen_server:cast(db_srv, {delete, ?db_table_name, Seq}),
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_light_data(DecrEstimFigures, BrokeFigures, I, State) ->
    case get_approach_figures(DecrEstimFigures, BrokeFigures) of
        {ok, NewDecrEstimFigures, CurBS} ->
            NewEFs = lists:map(fun(X) -> X + I - 1 end, NewDecrEstimFigures),
            FirstBS = get_first_bsecs(NewEFs, State#state.first_bfigures),
            NewBS = merge_bs(CurBS, FirstBS, State#state.broken_sections),
            OutNBS = lists:map(fun bs_to_out_format/1, NewBS),
            NewState = State#state{estimated_figures = NewEFs, iteration = I + 1, broken_sections = NewBS},
            gen_server:cast(db_srv, {insert, ?db_table_name, NewState#state.seq, NewState}),
            {{ok, {[{start, NewEFs}, {missing, OutNBS}]}}, NewState};
        {error, Reason} ->
            {{error, Reason}, State}
    end.

get_approach_figures(SNums, BFigures) ->
    BwBFigures = bfigures_to_bitwise(BFigures),
    get_approach_figures(SNums, BwBFigures, [], []).
get_approach_figures([], _, [], _) ->
    {error, <<"No solutions found">>};
get_approach_figures([], _, CorrectNums, BrokenSecs) ->
    {ok, lists:reverse(CorrectNums), BrokenSecs};
get_approach_figures([Num | NT], BwBFigures, CorrectNums, BrokenSecs) when is_list(BwBFigures) ->
    NumData = check_num(Num, BwBFigures),
    case NumData of
        {true, NumBS} ->
            NewBS = expel_bs(NumBS, BrokenSecs),
            get_approach_figures(NT, BwBFigures, [Num | CorrectNums], NewBS);
        {false, _} ->
            get_approach_figures(NT, BwBFigures, CorrectNums, BrokenSecs)
    end;
get_approach_figures(_, _, _, _) ->
    {error, <<"There isn't enough data">>}.

get_first_bsecs(NewEFs, BFigures) ->
    BwBFigures = bfigures_to_bitwise(BFigures),
    FirstRes = lists:duplicate(length(BFigures), 2#1111111),
    get_all_bsecs(NewEFs, BwBFigures, FirstRes).
get_all_bsecs([], _, Res) ->
    Res;
get_all_bsecs([Num | T], BwBFigures, Res) ->
    {true, NumBS} = check_num(Num, BwBFigures),
    NewAndBS = lists:zipwith(fun(X, Y) -> X band Y end, Res, NumBS),
    get_all_bsecs(T, BwBFigures, NewAndBS).

dig_to_figure(Dig) ->
    {Dig, Figure} = lists:keyfind(Dig, 1, ?figures_associations),
    Figure.

check_num(Num, BwBFigures) ->
    check_num(Num, BwBFigures, []).
check_num(_, [], BSecs) ->
    {true, BSecs};
check_num(Num, _, _) when Num < 0 ->
    {false, []};
check_num(Num, [BwBFigure | T], BSecs) ->
    BitwiseNum = dig_to_figure(Num rem 10),
    case BitwiseNum - BwBFigure == BitwiseNum bxor BwBFigure of
        true ->
            check_num(Num div 10, T, [BitwiseNum - BwBFigure | BSecs]);
        false ->
            {false, []}
    end.

bfigures_to_bitwise(BFigures) ->
    bfigures_to_bitwise(BFigures, []).
bfigures_to_bitwise([], Res) ->
    Res;
bfigures_to_bitwise([BFigure | T], Res) ->
    case  binary_to_bitwise_tuple(BFigure) of
        {ok, [BwBFigure], []} ->
            bfigures_to_bitwise(T, [BwBFigure | Res]);
        _Other ->
            {error, <<"There isn't enough data">>}
    end.

merge_bs(CurBS, _, []) ->
    CurBS;
merge_bs(CurBS, NotBrokenBS, PrevBS) ->
    lists:zipwith3(fun(X, Y, Z) -> X bor Y bor Z end, PrevBS, CurBS, NotBrokenBS).

expel_bs(CurBS, []) ->
    CurBS;
expel_bs(CurBS, BS) ->
    lists:zipwith(fun(X, Y) -> X band Y end, CurBS, BS).

bs_to_out_format(BS) ->
    list_to_binary(hd(io_lib:format("~7.2.0B", [BS]))).

binary_to_bitwise_tuple(Bin) ->
    io_lib:fread("~2u", binary_to_list(Bin)).
