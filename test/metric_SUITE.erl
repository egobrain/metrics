-module(metric_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([
    init_per_suite/1,
    all/0,
    end_per_suite/1
]).

-export([
    unknown_metric_test/1,
    average_test/1,
    window_test/1,
    load_test/1
]).

init_per_suite(Config)  ->
    Config.

end_per_suite(_Config) ->
    ok.


all() ->
    [
     unknown_metric_test,
     average_test,
     window_test,
     load_test
    ].

unknown_metric_test(_Config) ->
    ?assertEqual(0.0, metric_app:average(<<"unknown">>)).

average_test(_Config) ->
    metric_app:report(<<"a">>, 1.0),
    metric_app:report(<<"b">>, 6.0),
    metric_app:report(<<"a">>, 3.0),
    ?assertEqual(2.0, metric_app:average(<<"a">>)),
    ?assertEqual(6.0, metric_app:average(<<"b">>)).

window_test(_Config) ->
    M = <<"window_test">>,

    meck:expect(metric_srv, timestamp, fun() -> 0 end),
    metric_app:report(M, 1.0),
    timer:sleep(10),

    meck:expect(metric_srv, timestamp, fun() -> 30000 end),
    metric_app:report(M, 3.0),
    timer:sleep(10),

    meck:expect(metric_srv, timestamp, fun() -> 50000 end),
    metric_app:report(M, 5.0),
    timer:sleep(10),

    meck:expect(metric_srv, timestamp, fun() -> 60000 end),
    ?assertEqual(3.0, metric_app:average(M)),
    meck:expect(metric_srv, timestamp, fun() -> 70000 end),
    ?assertEqual(4.0, metric_app:average(M)),
    meck:delete(metric_srv, timestamp, 0).

load_test(_Config) ->
    Threads = 10000,
    Msgs = 10,
    SleepTimeout = 10,
    AckRef = make_ref(),
    Self = self(),
    lists:map(fun(I) ->
        spawn_link(fun() ->
            Name = thread_name(I),
            Diff = I*1.0,
            lists:foreach(fun(N) ->
                metric_app:report(Name, N+Diff),
                timer:sleep(SleepTimeout)
            end, lists:seq(1, Msgs)),
            Self ! {ack, AckRef}
        end)
    end, lists:seq(1, Threads)),

    Timeout = Msgs * SleepTimeout * 10,
    lists:foreach(fun(_I) ->
        receive {ack, AckRef} -> ok
        after Timeout -> throw(timeout)
        end
    end, lists:seq(1, Threads)),

    ?assertEqual(6.5, metric_app:average(thread_name(1))),
    ?assertEqual(7.5, metric_app:average(thread_name(2))).


thread_name(I) ->
    iolist_to_binary(["M-", integer_to_binary(I)]).
