-module(metric_admin).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    get_metric_srv/1,
    get_or_start_metric_srv/1
]).

-export([
    monitor/2 %% Used by metric_srv only
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

-define(METRICS_TABLE, '$metric_metric_table').

-record(state, {mon_t :: ets:tid()}).

%% =============================================================================
%%  API
%% =============================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    try ets:new(?METRICS_TABLE, [public, named_table])
    catch error:badarg -> ?METRICS_TABLE
    end,
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec monitor(Key :: term(), Pid :: pid()) -> ok.
monitor(Key, Pid) ->
    gen_server:cast(?MODULE, {monitor, Key, Pid}).

-spec get_metric_srv(Name :: binary()) -> {ok, pid()} | undefined.
get_metric_srv(Name) ->
    case ets:lookup(?METRICS_TABLE, Name) of
        [{Name, Pid}] -> {ok, Pid};
        _ -> undefined
    end.

-spec get_or_start_metric_srv(Name :: binary()) -> pid().
get_or_start_metric_srv(Name) ->
    case ets:lookup(?METRICS_TABLE, Name) of
        [{Name, Pid}] -> Pid;
        _ ->
            case metric_srv:start(Name, ?METRICS_TABLE) of
                {ok, Pid} -> Pid;
                {error, already_exists} -> get_or_start_metric_srv(Name)
            end
    end.

%% =============================================================================
%%  gen_server callbacks
%% =============================================================================

init([]) ->
    self() ! init,
    {ok, #state{mon_t = ets:new(?MODULE, [private])}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(init, #state{mon_t=MonT}=State) ->
    ets:foldl(fun({Name, Pid}, _) ->
        Ref = erlang:monitor(process, Pid),
        ets:insert(MonT, {Ref, Name, Pid})
    end, ok, ?METRICS_TABLE),
    {noreply, State};
handle_info({'DOWN', MonRef, process, Pid, _Info}, #state{mon_t=MonT}=State) ->
    case ets:lookup(MonT, MonRef) of
        [{MonRef, Pid, Name}] -> ets:delete_object(?METRICS_TABLE, {Name, Pid});
        _ -> ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%%  Internal functions
%% =============================================================================
