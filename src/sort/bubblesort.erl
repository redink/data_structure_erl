%% @doc

-module(bubblesort).

-export([sort_v1/1]).

%%%*_ MACROS and SPECS =========================================================

%%%*_ API FUNCTIONS ============================================================

sort_v1([]) -> [];
sort_v1(List) ->
    sort_v1_bubble(list_to_tuple(List), length(List)).

%%%*_ PRIVATE FUNCTIONS ========================================================

sort_v1_bubble(Tuple, 1) ->
    tuple_to_list(Tuple);
sort_v1_bubble(Tuple, Len) ->
    sort_v1_bubble(Tuple, 1, Len).

sort_v1_bubble(Tuple, Len, Len) ->
    sort_v1_bubble(Tuple, Len - 1);
sort_v1_bubble(Tuple, Index, Len) ->
    case element(Index, Tuple) > element(Index + 1, Tuple) of
        true ->
            NewTuple = exchange(Tuple, Index, Index + 1),
            sort_v1_bubble(NewTuple, Index + 1, Len);
        _ ->
            sort_v1_bubble(Tuple, Index + 1, Len)
    end.

exchange(Tuple, Index1, Index2) ->
    V1 = element(Index1, Tuple),
    V2 = element(Index2, Tuple),
    setelement(Index1, setelement(Index2, Tuple, V1), V2).

%%%*_ TESTS ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sort_v1_test_() ->
    [ {"test for bubblesort",
        fun() ->
            ?assertEqual([], sort_v1([])),
            [sort_v1_test_do(N) || N <- lists:seq(0, 100)]
        end}
    ].

sort_v1_test_do(N) ->
    List = [rand:uniform(100000) || _ <- lists:seq(1, N)],
    ?assertEqual(lists:sort(List), sort_v1(List)).

-endif.
