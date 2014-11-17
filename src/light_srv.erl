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
    {0, 1110111},
    {1, 0010010},
    {2, 1011101},
    {3, 1011011},
    {4, 0111010},
    {5, 1101011},
    {6, 1101111},
    {7, 1010010},
    {8, 1111111},
    {9, 1111011}
]).

-record(state, {
    iteration = 0,
    estimated_figures = [],
    broken_sections = [],
    first_bfigures = []
}).

% TODO: Check NULLs

start_link(_) ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    {ok, #state{iteration = 1}}.

handle_call({add_observation, <<"red">>, _}, _From, State = #state{iteration = 1}) ->
    {reply, {error, <<"There isn't enough data">>}, State};
handle_call({add_observation, <<"red">>, _}, _From, State = #state{iteration = I, first_bfigures = FBFs}) ->
    SplitedMEFs = split_figures([I - 1], FBFs),
    {Out, NewState} = get_light_data(SplitedMEFs, FBFs , 1, State),
    {reply, Out, NewState};
handle_call({add_observation, _, BFigures}, _From, State = #state{iteration = 1}) ->
    Nums = lists:duplicate(length(BFigures), lists:seq(0, 9)),
    {Out, NewState} = get_light_data(Nums, BFigures, 1, State),
    {reply, Out, NewState#state{first_bfigures = BFigures}};
handle_call({add_observation, _, BFigures}, _From, State = #state{iteration = I, estimated_figures = EFs} ) ->
    MEFs = lists:map(fun(X) -> X - I + 1 end, EFs),
    SplitedMEFs = split_figures(MEFs, BFigures),
    {Out, NewState} = get_light_data(SplitedMEFs, BFigures , I, State),
    {reply, Out, NewState#state{iteration = I + 1}};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

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
        {ok, NewMEFs, NBS} ->
            NEFs = lists:map(fun(X) -> X + I - 1 end, NewMEFs),
            OutNBS = lists:map(fun bs_to_out_format/1, NBS),
            NewState = State#state{estimated_figures = NEFs, iteration = I + 1},
            {{ok, {[{start, NEFs}, {missing, OutNBS}]}}, NewState}
    end.

split_figures(Figures, BrokeFigures) ->
    split_figures(Figures, BrokeFigures, []).
split_figures(_, [], Res) ->
    Res;
split_figures(Figures, [_ | T], Res) ->
    NewFigures =  lists:usort(lists:map(fun(F) -> F div 10 end, Figures)),
    SplitedFigures = lists:usort(lists:map(fun(F) -> F rem 10 end, Figures)),
    split_figures(NewFigures, T, [SplitedFigures | Res]).

check_correct_diff(Diff) when Diff < 0 ->
    false;
check_correct_diff(Diff) when Diff == 0 ->
    true;
check_correct_diff(Diff) when Diff rem 10 == 0; Diff rem 10 == 1 ->
    check_correct_diff(Diff div 10);
check_correct_diff(_) ->
    false.

get_unanbiguous_bs(Summ, Lenght) ->
    get_unanbiguous_bs(Summ, Lenght, 1, 0).
get_unanbiguous_bs(Summ, _, _, Result) when Summ == 0 ->
    Result;
get_unanbiguous_bs(Summ, Lenght, Acc, Result) when Summ rem 10 == Lenght ->
    get_unanbiguous_bs(Summ div 10, Lenght, Acc * 10, Result + Acc);
get_unanbiguous_bs(Summ, Lenght, Acc, Result) ->
    get_unanbiguous_bs(Summ div 10, Lenght, Acc * 10, Result).

prep_estim_figures(Data) ->
    prep_estim_figures(Data, 1, [0]).
prep_estim_figures([], _, Res) ->
    Res;
prep_estim_figures([EstFig | T], M, Res) ->
    AllFigures =  [X * M + Y || X <- EstFig, Y <- Res],
    prep_estim_figures(T, M * 10, AllFigures).

bs_to_out_format(BS) ->
    list_to_binary(io_lib:format("~7..0B", [BS])).

get_approach_figures(SNums, BFigures) ->
    get_approach_figures(SNums, BFigures, [], []).
get_approach_figures(_, [], CorrectNums, BrokenSecs) ->
    EstFigures = prep_estim_figures(CorrectNums),
    {ok, EstFigures, lists:reverse(BrokenSecs)};
get_approach_figures([Nums | NT], [BFigure | BFT], CorrectNums, BrokenSecs) ->
    IntBrokenFigure = list_to_integer(binary_to_list(BFigure)),
    NumsData = [compare_num(Dig, IntBrokenFigure) || Dig <- Nums],
    {ApprDig, Diffs} = lists:foldl(fun filter_num_data/2, {[], []}, NumsData),
    case Diffs of
        [] ->
            {error, <<"No solutions found">>};
        Diffs ->
            UBS = get_unanbiguous_bs(lists:sum(Diffs), length(Diffs)),
            get_approach_figures(NT, BFT, [ApprDig | CorrectNums], [UBS | BrokenSecs])
    end.

filter_num_data(error, PredData) -> PredData;
filter_num_data({Dig, Diff}, {Digs, Diffs}) -> {[Dig | Digs], [Diff | Diffs]}.

compare_num(Dig, _) when Dig < 0 ->
    error;
compare_num(Dig, IntBrokenFigure) ->
    {Dig, IntEtalonFigure} = lists:keyfind(Dig, 1, ?figures_associations),
    Diff = IntEtalonFigure - IntBrokenFigure,
    IsCorrectDiff = check_correct_diff(Diff),
    case IsCorrectDiff of
        false -> error;
        true -> {Dig, Diff}
    end.
