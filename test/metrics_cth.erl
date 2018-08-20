-module(metrics_cth).

-export([
    init/2,
    terminate/1,
    pre_init_per_suite/3
]).

init(_Id, State) ->
    application:ensure_all_started(metrics),
    State.

pre_init_per_suite(_SuiteName, Config, State) ->
    {Config, State}.

terminate(State) ->
    ok.
