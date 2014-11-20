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
    seq,
    iteration = 0,
    estimated_figures = [],
    broken_sections = [],
    first_bfigures = []
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
    SplitedMEFs = split_figures([I - 1], FBFs),
    {Out, NewState} = get_light_data(SplitedMEFs, FBFs , 1, State),
    gen_server:reply(ReqPid, Out),
    {noreply, NewState};

handle_cast({add_observation, _, BFigures, ReqPid}, State = #state{iteration = 1}) ->
    Nums = lists:duplicate(length(BFigures), lists:seq(0, 9)),
    {Out, NewState} = get_light_data(Nums, BFigures, 1, State#state{first_bfigures = BFigures}),
    gen_server:reply(ReqPid, Out),
    {noreply, NewState};

handle_cast({add_observation, _, BFigures, ReqPid}, State = #state{iteration = I, estimated_figures = EFs} ) ->
    MEFs = lists:map(fun(X) -> X - I + 1 end, EFs),
    SplitedMEFs = split_figures(MEFs, BFigures),
    {Out, NewState} = get_light_data(SplitedMEFs, BFigures , I, State),
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

get_light_data(SplitedMEFs, BrokeFigures, I, State) ->
    case get_approach_figures(SplitedMEFs, BrokeFigures) of
        {error, Reason} ->
            {{error, Reason}, State};
        {ok, NewMEFs, CurBS} ->
            NewBS = merge_bs(CurBS, State#state.broken_sections),
            NewEFs = lists:map(fun(X) -> X + I - 1 end, NewMEFs),
            OutNBS = lists:map(fun bs_to_out_format/1, NewBS),
            NewState = State#state{estimated_figures = NewEFs, iteration = I + 1, broken_sections = NewBS},
            gen_server:cast(db_srv, {insert, ?db_table_name, NewState#state.seq, NewState}),
            {{ok, {[{start, NewEFs}, {missing, OutNBS}]}}, NewState}
    end.

split_figures(Figures, BrokeFigures) ->
    split_figures(Figures, BrokeFigures, []).
split_figures(_, [], Res) ->
    Res;
split_figures(Figures, [_ | T], Res) ->
    NewFigures =  lists:usort(lists:map(fun(F) -> F div 10 end, Figures)),
    SplitedFigures = lists:usort(lists:map(fun(F) -> F rem 10 end, Figures)),
    split_figures(NewFigures, T, [SplitedFigures | Res]).

prep_estim_figures(Data) ->
    prep_estim_figures(Data, 1, [0]).
prep_estim_figures([], _, Res) ->
    lists:delete(0, Res);
prep_estim_figures([EstFig | T], M, Res) ->
    AllFigures =  [X * M + Y || X <- EstFig, Y <- Res],
    prep_estim_figures(T, M * 10, AllFigures).

get_approach_figures(SNums, BFigures) ->
    BwTupleBFIgures = lists:map(fun binary_to_bitwise_tuple/1, BFigures),
    get_approach_figures(SNums, BwTupleBFIgures, [], []).
get_approach_figures(_, [], CorrectNums, BrokenSecs) ->
    EstFigures = prep_estim_figures(CorrectNums),
    {ok, EstFigures, BrokenSecs};
get_approach_figures([Nums | NT], [{ok, [BwBFigure], []} | BFT], CorrectNums, BrokenSecs) ->
    BitwiseNums = lists:map(fun dig_to_figure/1, Nums),
    NumsData = [
        {figure_to_dig(Num), Num - BwBFigure} ||
        Num <- BitwiseNums,
        Num - BwBFigure == Num bxor BwBFigure
    ],
    case NumsData of
        [] ->
            {error, <<"No solutions found">>};
        NumsData ->
            {ApprDig, Diffs} = lists:unzip(NumsData),
            UnanbiguousBS = lists:foldl(fun(X, Y) -> X band Y end, 2#1111111, Diffs),
            get_approach_figures(NT, BFT, [ApprDig | CorrectNums], [UnanbiguousBS | BrokenSecs])
    end;
get_approach_figures(_, _, _, _) ->
    {error, <<"There isn't enough data">>}.

dig_to_figure(Dig) ->
    {Dig, Figure} = lists:keyfind(Dig, 1, ?figures_associations),
    Figure.

figure_to_dig(Figure) ->
    {Dig, Figure} = lists:keyfind(Figure, 2, ?figures_associations),
    Dig.

merge_bs(CurBS, []) ->
    CurBS;
merge_bs(CurBS, BS) ->
    merge_bs(CurBS, lists:reverse(BS), []).
merge_bs([], _, Res) ->
    Res;
merge_bs([CurBS | CBST], [BS | BST], Res) ->
    merge_bs(CBST, BST, [CurBS bor BS | Res]).

bs_to_out_format(BS) ->
    list_to_binary(hd(io_lib:format("~7.2.0B", [BS]))).

binary_to_bitwise_tuple(Bin) ->
    io_lib:fread("~2u", binary_to_list(Bin)).
