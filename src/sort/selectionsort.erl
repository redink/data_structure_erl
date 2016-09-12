%% @doc

-module(selectionsort).

-export([sort_v1/1]).

%%%*_ MACROS and SPECS =========================================================

%%%*_ API FUNCTIONS ============================================================

sort_v1([]) -> [];
sort_v1(List) ->
    sort_v1_select(List, []).

%%%*_ PRIVATE FUNCTIONS ========================================================

sort_v1_select([], Res) ->
    lists:reverse(Res);
sort_v1_select([H | Tail], Res) ->
    {Min, NewTail} = find_min(Tail, H, []),
    sort_v1_select(NewTail, [Min | Res]).

find_min([], Min, Res) ->
    {Min, lists:reverse(Res)};
find_min([H | Tail], Min, Res) when Min > H ->
    find_min(Tail, H, [Min | Res]);
find_min([H | Tail], Min, Res) ->
    find_min(Tail, Min, [H | Res]).

%%%*_ TESTS ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sort_v1_test_() ->
    [ {"test for selectionsort",
        fun() ->
            ?assertEqual([], sort_v1([])),
            [sort_v1_test_do(N) || N <- lists:seq(1, 100)]
        end}
    ].

sort_v1_test_do(N) ->
    List = [rand:uniform(100000) || _ <- lists:seq(1, N)],
    ?assertEqual(lists:sort(List), sort_v1(List)).

-endif.
