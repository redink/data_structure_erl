%% @doc

-module(quicksort).

-export([sort_v1/1]).

%%%*_ MACROS and SPECS =========================================================

%%%*_ API FUNCTIONS ============================================================

sort_v1(L) when length(L) =< 1 -> L;
sort_v1([P | List]) ->
    {Small, Big} = find_small_and_big(List, P),
    sort_v1(Small) ++ [P] ++ sort_v1(Big).

%%%*_ PRIVATE FUNCTIONS ========================================================

find_small_and_big(List, P) ->
    find_small_and_big(List, P, [], []).

find_small_and_big([], _, Res1, Res2) ->
    {lists:reverse(Res1), lists:reverse(Res2)};
find_small_and_big([H | Tail], P, Res1, Res2) ->
    case H > P of
        true ->
            find_small_and_big(Tail, P, Res1, [H | Res2]);
        _ ->
            find_small_and_big(Tail, P, [H | Res1], Res2)
    end.

%%%*_ TESTS ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sort_v1_test_() ->
    [ {"test for quicksort",
        fun() ->
            ?assertEqual([], sort_v1([])),
            [sort_v1_test_do(N) || N <- lists:seq(0, 100)]
        end}
    ].

sort_v1_test_do(N) ->
    List = [rand:uniform(100000) || _ <- lists:seq(1, N)],
    ?assertEqual(lists:sort(List), sort_v1(List)).

-endif.
