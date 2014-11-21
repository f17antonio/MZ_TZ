-module(db_srv).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({select, TableName, Key, DefRes}, _From, State) ->
    dets:open_file(TableName, []),
    Result = case dets:lookup(TableName, Key) of
        [{Key, Res}] -> Res;
        _Other -> DefRes
    end,
    {reply, Result, State};
handle_call({select_all, TableName}, _From, State) ->
    Result = select_all(TableName, dets:is_dets_file(TableName)),
    {reply, Result, State}.

handle_cast({clear, TableName}, State) ->
    dets:open_file(TableName, []),
    dets:delete_all_objects(TableName),
    {noreply, State};
handle_cast({delete, TableName, Key}, State) ->
    dets:open_file(TableName, []),
    dets:delete(TableName, Key),
    {noreply, State};
handle_cast({insert, TableName, Key, Value}, State) ->
    dets:open_file(TableName, []),
    dets:insert(TableName, {Key, Value}),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


select_all(TableName, true) ->
    dets:open_file(TableName, []),
    ets:new(TableName, [named_table, set]),
    EtsTable = dets:to_ets(TableName, TableName),
    ets:tab2list(EtsTable);
select_all(_, _) ->
    [].
