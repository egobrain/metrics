-module(metrics_queue).

-export([
    new/1,
    push/3,
    foldl/4
]).

-compile(inline).
-compile(inline_list_funcs).

-type value() :: float().
-type timestamp() :: integer().

-record(window, {
    buff = [] :: [{timestamp(), value()}],
    rev_buff = [] :: [{timestamp(), value()}],
    start_ts = 0 :: timestamp(),
    time_span :: pos_integer()
}).

-opaque window() :: #window{}.

-export_type([
    window/0
]).

-spec new(TimeSpan :: pos_integer()) -> window().
new(TimeSpan) -> #window{time_span=TimeSpan}.

-spec push(value(), timestamp(), window()) -> window().
push(V, Ts, #window{buff=[]}=W) -> W#window{start_ts=Ts, buff=[{Ts,V}]};
push(V, Ts, #window{time_span=TimeSpan, start_ts = StartTs, buff = Buff, rev_buff=RevBuff}=W) ->
    MinTs = Ts - TimeSpan,
    case StartTs < MinTs of
        true -> W#window{start_ts=Ts, rev_buff=clear(lists:reverse(Buff), MinTs), buff=[{Ts, V}]};
        false -> W#window{buff=[{Ts, V}|Buff], rev_buff=clear(RevBuff, MinTs)}
    end.

clear([{Ts, _V}|Rest], MinTs) when Ts < MinTs -> clear(Rest, MinTs);
clear(Buff, _MinTs) -> Buff.

-spec foldl(timestamp(), Fun, Acc, window()) -> Acc when
      Fun :: fun((value(), Acc) -> Acc).
foldl(Ts, Fun, Acc, #window{time_span=TimeSpan, buff=Buff, rev_buff=RevBuff}) ->
    MinTs = Ts - TimeSpan,
    lists:foldr(
        fun ({T, V}, A) when T >= MinTs -> Fun(V, A);
            (_, A) -> A
        end, foldr_(Fun, Acc, MinTs, RevBuff), Buff).

foldr_(Fun, Acc, MinTs, [{Ts, _V}|Rest]) when Ts < MinTs ->
    foldr_(Fun, Acc, MinTs, Rest);
foldr_(Fun, Acc, MinTs, [{_Ts, V}|Rest]) ->
    foldr_(Fun, Fun(V, Acc), MinTs, Rest);
foldr_(_Fun, Acc, _MinTs, []) ->  Acc.

%% =============================================================================
%% Tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

empty_test() ->
    W = ?MODULE:new(10),
    ?assertEqual(0, ?MODULE:foldl(10, fun(A, B) -> A + B end, 0, W)).

window_test() ->
    W1 = ?MODULE:new(5),
    WR = lists:foldl(fun(I, W) -> ?MODULE:push(I*10, I, W) end, W1, lists:seq(1, 10)),
    ?assertEqual(
        [100,90,80,70],
         ?MODULE:foldl(12, fun(I, Acc) -> [I|Acc] end, [], WR)),
    ?assertEqual(
        [100,90,80,70,60,50],
        ?MODULE:foldl(9, fun(I, Acc) -> [I|Acc] end, [], WR)).

-endif.
