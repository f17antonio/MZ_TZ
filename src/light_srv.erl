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
    iteration = 0,
    estimated_figures = [],
    broken_sections = [],
    first_bfigures = []
}).

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
        {ok, NewMEFs, CurBS} ->
            NewBS = merge_bs(CurBS, State#state.broken_sections),
            NEFs = lists:map(fun(X) -> X + I - 1 end, NewMEFs),
            OutNBS = lists:map(fun bs_to_out_format/1, NewBS),
            NewState = State#state{estimated_figures = NEFs, iteration = I + 1, broken_sections = NewBS},
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

prep_estim_figures(Data) ->
    prep_estim_figures(Data, 1, [0]).
prep_estim_figures([], _, Res) ->
    lists:delete(0, Res);
prep_estim_figures([EstFig | T], M, Res) ->
    AllFigures =  [X * M + Y || X <- EstFig, Y <- Res],
    prep_estim_figures(T, M * 10, AllFigures).

bs_to_out_format(BS) ->
    list_to_binary(hd(io_lib:format("~7.2.0B", [BS]))).

get_approach_figures(SNums, BFigures) ->
    get_approach_figures(SNums, BFigures, [], []).
get_approach_figures(_, [], CorrectNums, BrokenSecs) ->
    EstFigures = prep_estim_figures(CorrectNums),
    {ok, EstFigures, BrokenSecs};
get_approach_figures([Nums | NT], [BFigure | BFT], CorrectNums, BrokenSecs) ->
    {ok, [BwBFigure], []} = io_lib:fread("~2u", binary_to_list(BFigure)),
    BitwiseNums = lists:map(fun dig_to_figure/1, Nums),
    NumsData = [{figure_to_dig(Num), Num bxor BwBFigure} || Num <- BitwiseNums, Num - BwBFigure == Num bxor BwBFigure],
    case NumsData of
        [] ->
            {error, <<"No solutions found">>};
        NumsData ->
            {ApprDig, Diffs} = lists:unzip(NumsData),
            UnanbiguousBS = lists:foldl(fun(X, Y) -> X band Y end, 127, Diffs),
            get_approach_figures(NT, BFT, [ApprDig | CorrectNums], [UnanbiguousBS | BrokenSecs])
    end.

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
