%% @doc

-module(heapsort).

-export([sort_v1/1]).

%%%*_ MACROS and SPECS =========================================================

%%%*_ API FUNCTIONS ============================================================

sort_v1(L) when length(L) =< 1 -> L;
sort_v1(List) ->
    sort_v1_heap(List, []).

%%%*_ PRIVATE FUNCTIONS ========================================================

sort_v1_heap([], Res) ->
    lists:reverse(Res);
sort_v1_heap(List, Res) ->
    [Min | Tail] = tuple_to_list(sink(List)),
    sort_v1_heap(Tail, [Min | Res]).

sink(List) ->
    sink(List, {}).

sink([], Res) ->
    Res;
sink([H | T], Res) ->
    sink(T, sink(erlang:insert_element(1, Res, H), 1, tuple_size(Res) + 1)).

sink(Tuple, Index, Len) when 2 * Index =< Len ->
    V1 = element(Index, Tuple),
    V2 = element(Index * 2, Tuple),
    case V1 > V2 of
        true ->
            sink(setelement(Index, setelement(Index * 2, Tuple, V1), V2),
                 Index * 2, Len);
        _ ->
            Tuple
    end;
sink(Tuple, _, _) ->
    Tuple.

%%%*_ TESTS ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sort_v1_test_() ->
    [ {"test for heapsort",
        fun() ->
            ?assertEqual([], sort_v1([])),
            [sort_v1_test_do(N) || N <- lists:seq(0, 100)]
        end}
    ].

sort_v1_test_do(N) ->
    List = [rand:uniform(100000) || _ <- lists:seq(1, N)],
    ?assertEqual(lists:sort(List), sort_v1(List)).

-endif.
