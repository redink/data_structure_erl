%% @doc

-module(insertionsort).

-export([sort_v1/1]).

%%%*_ MACROS and SPECS =========================================================

%%%*_ API FUNCTIONS ============================================================

sort_v1([]) -> [];
sort_v1(List) ->
    sort_v1_insert(List, {}).

%%%*_ PRIVATE FUNCTIONS ========================================================

sort_v1_insert([], Sorted) ->
    tuple_to_list(Sorted);
sort_v1_insert([H | T], Sorted) ->
    NewSorted = sort_v1_insert_do(Sorted, 1, tuple_size(Sorted), H),
    sort_v1_insert(T, NewSorted).

sort_v1_insert_do({}, _, _, Key) ->
    {Key};
sort_v1_insert_do({OldKey}, _, _, Key) when Key > OldKey ->
    {OldKey, Key};
sort_v1_insert_do({OldKey}, _, _, Key) ->
    {Key, OldKey};
sort_v1_insert_do(Sorted, Index, Len, Key) when Index < Len ->
    V1 = element(Index, Sorted),
    V2 = element(Index + 1, Sorted),
    if
        Key < V1 ->
            erlang:insert_element(Index, Sorted, Key);
        Key >= V1 andalso Key =< V2 ->
            erlang:insert_element(Index + 1, Sorted, Key);
        true ->
            sort_v1_insert_do(Sorted, Index + 1, Len, Key)
    end;
sort_v1_insert_do(Sorted, Len, Len, Key) ->
    erlang:insert_element(Len + 1, Sorted, Key).

%%%*_ TESTS ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sort_v1_test_() ->
    [ {"test for insertionsort",
        fun() ->
            ?assertEqual([], sort_v1([])),
            [sort_v1_test_do(N) || N <- lists:seq(0, 100)]
        end}
    ].

sort_v1_test_do(N) ->
    List = [rand:uniform(100000) || _ <- lists:seq(1, N)],
    ?assertEqual(lists:sort(List), sort_v1(List)).

-endif.
