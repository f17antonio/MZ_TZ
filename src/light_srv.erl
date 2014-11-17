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
    {Out, NewState} = get_light_data([I - 1], FBFs , 1, State),
    {reply, Out, NewState};

handle_call({add_observation, _, BFigures}, _From, State = #state{iteration = 1}) ->
    {EstimFigures, BrokenSections} = get_start_figures(BFigures),
    {reply, {ok, {[{start, EstimFigures}, {missing, BrokenSections}]}}, State#state{
        first_bfigures = BFigures,
        estimated_figures = EstimFigures,
        iteration = 2}};

handle_call({add_observation, _, BFigures}, _From, State = #state{iteration = I, estimated_figures = EFs} ) ->
    {Out, NewState} = get_light_data(EFs, BFigures , I, State),
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


get_light_data(EstimatedFigures, BrokeFigures, I, State) ->
    MEFs = lists:map(fun(X) -> X - I + 1 end, EstimatedFigures),
    case get_approach_figures(MEFs, BrokeFigures) of
        {error, Reason} ->
            {{error, Reason}, State};
        {ok, NewMEFs, NBS} ->
            NEFs = lists:map(fun(X) -> X + I - 1 end, NewMEFs),
            OutNBS = lists:map(fun bs_to_out_format/1, NBS),
            NewState = State#state{estimated_figures = NEFs, iteration = I + 1},
            {{ok, {[{start, NEFs}, {missing, OutNBS}]}}, NewState}
    end.

get_start_figures(Figures) ->
    FiguresCompareData = lists:map(fun compare_figure/1, Figures),
    BrokenSections = [bs_to_out_format(BS) || {_, BS} <- FiguresCompareData],
    EstimatedFigures = prepare_estimated_figures(FiguresCompareData),
    {EstimatedFigures, BrokenSections}.

compare_figure(Figure) ->
    IntFigure = list_to_integer(binary_to_list(Figure)),
    {EstimatedFigures, BrokenSecs} = compare_figure(IntFigure, ?figures_associations, [], []),
    UBrokenSecs = get_unanbiguous_bs(lists:sum(BrokenSecs), length(BrokenSecs)),
    {EstimatedFigures, UBrokenSecs}.

compare_figure(_, [], EstimatedFigures, BrokenSections) ->
    {EstimatedFigures, BrokenSections};
compare_figure(IntFigure, [{Dig, IntEtalonFigure}  | T], EstFigures, BrokenSections) ->
    Diff = IntEtalonFigure - IntFigure,
    IsCorrectDiff = check_correct_diff(Diff),
    case IsCorrectDiff of
        true ->
            compare_figure(IntFigure, T, [Dig | EstFigures], [Diff | BrokenSections]);
        false ->
            compare_figure(IntFigure, T, EstFigures, BrokenSections)
    end.

check_correct_diff(Diff) when Diff < 0 ->
    false;
check_correct_diff(Diff) when Diff == 0 ->
    true;
check_correct_diff(Diff) when Diff rem 10 == 0; Diff rem 10 == 1 ->
    check_correct_diff(Diff div 10);
check_correct_diff(_) ->
    false.

get_unanbiguous_broken_secs(BSecs) ->
    get_unanbiguous_broken_secs(BSecs, []).
get_unanbiguous_broken_secs([], Result) ->
    lists:reverse(Result);
get_unanbiguous_broken_secs([BS | T], Result) ->
    UBS = get_unanbiguous_bs(lists:sum(BS), length(BS)),
    get_unanbiguous_broken_secs(T, [UBS | Result]).

get_unanbiguous_bs(Summ, Lenght) ->
    get_unanbiguous_bs(Summ, Lenght, 1, 0).
get_unanbiguous_bs(Summ, _, _, Result) when Summ == 0 ->
    Result;
get_unanbiguous_bs(Summ, Lenght, Acc, Result) when Summ rem 10 == Lenght ->
    get_unanbiguous_bs(Summ div 10, Lenght, Acc * 10, Result + Acc);
get_unanbiguous_bs(Summ, Lenght, Acc, Result) ->
    get_unanbiguous_bs(Summ div 10, Lenght, Acc * 10, Result).

prepare_estimated_figures(Data) ->
    prepare_estimated_figures(lists:reverse(Data), 1, [0]).
prepare_estimated_figures([], _, Res) ->
    Res;
prepare_estimated_figures([{EstFig, _} | T], M, Res) ->
    AllFigures =  [X * M + Y || X <- EstFig, Y <- Res],
    prepare_estimated_figures(T, M * 10, AllFigures).

get_broken_sections(Num, _) when Num < 0 ->
    {error, <<"Incorrect Number">>};
get_broken_sections(Num, BrokenFigures) ->
    get_broken_sections(Num, lists:reverse(BrokenFigures), []).
get_broken_sections(_, [], BrokenSec) ->
    BrokenSec;
get_broken_sections(Num, [BrokenFigure | T], BS) ->
    Dig = Num rem 10,
    InrBrokenFigure = list_to_integer(binary_to_list(BrokenFigure)),
    {Dig, IntEtalonFigure} = lists:keyfind(Dig, 1, ?figures_associations),
    Diff = IntEtalonFigure - InrBrokenFigure,
    IsCorrectDiff = check_correct_diff(Diff),
    case IsCorrectDiff of
        false ->
            {error, <<"Incorrect Diff">>};
        true ->
            get_broken_sections(Num div 10, T, [Diff | BS])
    end.

bs_to_out_format(BS) ->
    list_to_binary(io_lib:format("~7..0B", [BS])).

get_approach_figures(Nums, BFigures) ->
    EmptyBS = lists:duplicate(length(BFigures), []),
    get_approach_figures(Nums, BFigures, EmptyBS, []).
get_approach_figures([], _, _, []) ->
    {error, <<"No solutions found">>};
get_approach_figures([], _, BS, ValidNums) ->
    UBS = get_unanbiguous_broken_secs(BS),
    {ok, ValidNums, UBS};
get_approach_figures([Num | T], BFigures, BS, ValidNums) ->
    BrokenSections = get_broken_sections(Num, BFigures),
    case BrokenSections of
        {error, _} ->
            get_approach_figures(T, BFigures, BS, ValidNums);
        BrokenSections ->
            MergedBS = lists:zipwith(fun(X, Y) -> [X | Y] end, BrokenSections, BS),
            get_approach_figures(T, BFigures, MergedBS, [Num | ValidNums])
    end.
