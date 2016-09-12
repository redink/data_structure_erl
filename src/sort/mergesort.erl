%% @doc

-module(mergesort).

-export([sort_v1/1]).

%%%*_ MACROS and SPECS =========================================================

%%%*_ API FUNCTIONS ============================================================

sort_v1(List) when length(List) =< 1 -> List;
sort_v1(List) ->
    {L1, L2} = lists:split(erlang:round(erlang:length(List)/2), List),
    merge_list(sort_v1(L1), sort_v1(L2)).

%%%*_ PRIVATE FUNCTIONS ========================================================

merge_list([], L2) ->
    L2;
merge_list(L1, []) ->
    L1;
merge_list([H1 | T1], [H2 | _] = L2) when H1 < H2 ->
    [H1 | merge_list(T1, L2)];
merge_list(L1, [H2 | T2]) ->
    [H2 | merge_list(L1, T2)].

%%%*_ TESTS ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sort_v1_test_() ->
    [ {"test for mergesort",
        fun() ->
            ?assertEqual([], sort_v1([])),
            [sort_v1_test_do(N) || N <- lists:seq(0, 100)]
        end}
    ].

sort_v1_test_do(N) ->
    List = [rand:uniform(100000) || _ <- lists:seq(1, N)],
    ?assertEqual(lists:sort(List), sort_v1(List)).

-endif.
