-module(metrics_app).

-behaviour(application).

%% Application callbacks
-export([
    start/2,
    stop/1
]).

%% API
-export([
    report/2,
    average/1
]).

%% =============================================================================
%% Application callbacks
%% =============================================================================

start(_StartType, _StartArgs) ->
    metrics_sup:start_link().

stop(_State) ->
    ok.

%% =============================================================================
%% API
%% =============================================================================

-spec report(MetricName :: binary(), MetricValue :: float()) -> ok.
report(Name, Value) ->
    Pid = metrics_admin:get_or_start_metrics_srv(Name),
    metrics_srv:report(Pid, Value).

-spec average(MetricName :: binary()) -> float().
average(Name) ->
    case metrics_admin:get_metrics_srv(Name) of
        {ok, Pid} ->
            {ok, Avg} = metrics_srv:get_average(Pid),
            Avg;
        undefined -> 0.0
    end.

%% =============================================================================
%% Internal functions
%% =============================================================================
