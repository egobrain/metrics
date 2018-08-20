-module(metrics_srv).

-behaviour(gen_server).

%% API
-export([
    start/2,
    get_average/1,
    report/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).



-ifdef(TEST).
-export([timestamp/0]). %% Export for ct meck
-define(TIMESTAMP, ?MODULE:timestamp()).
-else.
-define(TIMESTAMP, timestamp()).
-compile(inline).
-endif.

%% =============================================================================
%%  API
%% =============================================================================

-spec start(Name :: term(), NamesT :: ets:tab()) ->
        {ok, pid()} | {error, already_exists}.
start(Name, NamesT) ->
    gen_server:start(?MODULE, [Name, NamesT], []).

-spec get_average(pid()) -> {ok, float()}.
get_average(Pid) ->
    gen_server:call(Pid, get).

-spec report(pid(), float()) -> ok.
report(Pid, V) ->
    gen_server:cast(Pid, {report, V}).

%% =============================================================================
%%  gen_server callbacks
%% =============================================================================

init([Name, NamesT]) ->
    case ets:insert_new(NamesT, {Name, self()}) of
        true ->
            {ok, TimeSpan} = application:get_env(metrics, time_span),
            ok = metrics_admin:monitor(Name, self()),
            {ok, metrics_queue:new(TimeSpan)};
        false ->
            {stop, already_exists}
    end.

handle_call(get, _From, Queue) ->
    {Total, Cnt} = metrics_queue:foldl(?TIMESTAMP, fun(V, {Total, Cnt}) ->
        {Total + V, Cnt + 1}
    end, {0,0}, Queue),
    Avg = case Cnt of
        0 -> 0;
        _ -> Total/Cnt
    end,
    {reply, {ok, Avg}, Queue};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({report, V}, Queue) ->
    {noreply, metrics_queue:push(V, ?TIMESTAMP, Queue)};
handle_cast(_, Queue) ->
    {noreply, Queue}.

handle_info(_Info, Queue) ->
    {noreply, Queue}.

terminate(_Reason, _Queue) ->
    ok.

code_change(_OldVsn, Queue, _Extra) ->
    {ok, Queue}.

%% =============================================================================
%%  Internal functions
%% =============================================================================

timestamp() -> erlang:monotonic_time(millisecond).
