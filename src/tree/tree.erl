%% @doc

-module(tree).

-export([ count/1
        , height/1
        , mid/1
        , prev/1
        , last/1
        , bfs/1
        , count_k/2
        , count_leaf/1
        , struct_cmp/2
        , reverse/1
        , get_nearest_common_parent/3
        , get_nearest_common_parent_v2/3
        , get_biggest_dis/1
        , from_last_mid/2
        , from_prev_mid/2
        , is_complete/1
        ]).

%%%*_ MACROS and SPECS =========================================================

-define(empty_tree,
        #node{root = undefined, left = undefined, right = undefined}).

-record(node, {root, left, right}).

%%%*_ API FUNCTIONS ============================================================

count(?empty_tree) -> 0;
count(undefined) -> 0;
count(#node{left = Left, right = Right}) ->
    count(Left) + count(Right) + 1.

height(undefined) -> 0;
height(?empty_tree) -> 0;
height(#node{left = Left, right = Right}) ->
    max(height(Left), height(Right)) + 1.

prev(?empty_tree) -> [];
prev(undefined) -> [];
prev(#node{root = Root, left = Left, right = Right}) ->
    [Root] ++ prev(Left) ++ prev(Right).

mid(?empty_tree) -> [];
mid(undefined) -> [];
mid(#node{root = Root, left = Left, right = Right}) ->
    mid(Left) ++ [Root] ++ mid(Right).

last(?empty_tree) -> [];
last(undefined) -> [];
last(#node{root = Root, left = Left, right = Right}) ->
    last(Left) ++ last(Right) ++ [Root].

bfs(?empty_tree) -> [];
bfs(Tree) -> bfs_do([Tree], []).

count_k(Tree, K) when Tree =/= undefined,
                      Tree =/= ?empty_tree,
                      K > 1 ->
    count_k_do(Tree, K);
count_k(Tree, 1) when Tree =/= undefined,
                      Tree =/= ?empty_tree ->
    1;
count_k(_, _) ->
    0.

count_leaf(?empty_tree) -> 0;
count_leaf(undefined) -> 0;
count_leaf(#node{left = undefined, right = undefined}) ->
    1;
count_leaf(#node{left = Left, right = Right}) ->
    count_leaf(Left) + count_leaf(Right).

struct_cmp(Tree1, Tree2) ->
    case {Tree1, Tree2} of
        {undefined, undefined} ->
            true;
        {?empty_tree, ?empty_tree} ->
            true;
        {undefined, _} ->
            false;
        {_, undefined} ->
            false;
        {?empty_tree, _} ->
            false;
        {_, ?empty_tree} ->
            false;
        {#node{left = L1, right = R1},
         #node{left = L2, right = R2}} ->
            struct_cmp(L1, L2) andalso struct_cmp(R1, R2)
    end.

reverse(Tree) when Tree == ?empty_tree; Tree == undefined ->
    Tree;
reverse(#node{left = L, right = R} = Tree) ->
    Tree#node{left = reverse(R), right = reverse(L)}.

get_nearest_common_parent(?empty_tree, _, _) -> not_found;
get_nearest_common_parent(undefined, _, _) -> not_found;
get_nearest_common_parent(#node{root = K1} = Tree, K1, K2) ->
    case in_tree(Tree, K2) of
        true ->
            K1;
        false ->
            not_found
    end;
get_nearest_common_parent(#node{root = K2} = Tree, K1, K2) ->
    case in_tree(Tree, K1) of
        true ->
            K2;
        false ->
            not_found
    end;
get_nearest_common_parent(#node{root = Root, left = Left, right = Right}, K1, K2) ->
    case {in_tree(Left, K1), in_tree(Right, K2),
          in_tree(Right, K1), in_tree(Left, K2)} of
        {true, true, false, false} ->
            Root;
        {false, false, true, true} ->
            Root;
        {true, false, false, true} ->
            get_nearest_common_parent(Left, K1, K2);
        {false, true, true, false} ->
            get_nearest_common_parent(Right, K1, K2);
        {false, _, false, _} ->
            not_found;
        {_, false, _, false} ->
            not_found
    end.

get_nearest_common_parent_v2(Tree, K1, K2) ->
    Path1 = get_node_path(Tree, K1),
    Path2 = get_node_path(Tree, K2),
    case length(Path1) < length(Path2) of
        true ->
            find_parent_via_path(lists:reverse(Path1), lists:reverse(Path2));
        _ ->
            find_parent_via_path(lists:reverse(Path2), lists:reverse(Path1))
    end.

get_biggest_dis(?empty_tree) -> 0;
get_biggest_dis(undefined) -> 0;
get_biggest_dis(#node{left = Left, right = Right}) ->
    lists:max([get_biggest_dis(Left), get_biggest_dis(Right),
               height(Left) + height(Right)]).

from_last_mid([], _) ->
    undefined;
from_last_mid(_, []) ->
    undefined;
from_last_mid(Last, Mid) ->
    {Root, LastTail} = get_root_from_last(Last),
    case split_left_right_from_mid_via_root(Mid, Root) of
        {Left, Right} ->
            #node{root = Root, left = from_last_mid(LastTail, Left),
                  right = from_last_mid(LastTail, Right)};
        _ ->
            from_last_mid(LastTail, Mid)
    end.

from_prev_mid([], _) ->
    undefined;
from_prev_mid(_, []) ->
    undefined;
from_prev_mid(Prev, Mid) ->
    [Root | PrevTail] = Prev,
    case split_left_right_from_mid_via_root(Mid, Root) of
        {Left, Right} ->
            #node{root = Root, left = from_prev_mid(PrevTail, Left),
                  right = from_prev_mid(PrevTail, Right)};
        _ ->
            from_prev_mid(PrevTail, Mid)
    end.

is_complete(?empty_tree) -> true;
is_complete(Tree) ->
    is_complete([Tree], 0).

%%%*_ PRIVATE FUNCTIONS ========================================================

bfs_do([], ResList) ->
    lists:reverse(ResList);
bfs_do([#node{root = Root, left = Left, right = Right} | Tail], ResList) ->
    NewTail = [X || X <- Tail ++ [Left, Right], X =/= undefined],
    bfs_do(NewTail, [Root | ResList]).

count_k_do(#node{left = Left, right = Right}, K) ->
    count_k(Left, K - 1) + count_k(Right, K -1).

in_tree(undefined, _Key) -> false;
in_tree(?empty_tree, _Key) -> false;
in_tree(#node{root = Key}, Key) -> true;
in_tree(#node{left = Left, right = Right}, Key) ->
    in_tree(Left, Key) orelse in_tree(Right, Key).

get_node_path(Tree, Key) ->
    lists:reverse(get_node_path(Tree, Key, [])).

get_node_path(?empty_tree, _, _) -> [];
get_node_path(undefined, _, _) -> [];
get_node_path(#node{root = Key}, Key, Res) -> [Key | Res];
get_node_path(#node{root = Root, left = Left, right = Right}, Key, Res) ->
    case get_node_path(Left, Key, [Root | Res]) of
        [] ->
            get_node_path(Right, Key, [Root | Res]);
        List ->
            List
    end.

find_parent_via_path([], _) -> not_found;
find_parent_via_path([H | T], Path2) ->
    case lists:member(H, Path2) of
        true ->
            H;
        _ ->
            find_parent_via_path(T, Path2)
    end.

get_root_from_last(Last) ->
    [Root | Tail] = lists:reverse(Last),
    {Root, lists:reverse(Tail)}.

split_left_right_from_mid_via_root(Mid, Root) ->
    case lists:member(Root, Mid) of
        true ->
            {Left, [Root | Right]} =
                lists:splitwith(fun(X) -> X =/= Root end, Mid),
            {Left, Right};
        _ ->
            not_found
    end.

is_complete([], _) ->
    true;
is_complete([undefined | Tail], _) ->
    is_complete(Tail, 1);
is_complete(_, 1) ->
    false;
is_complete([#node{left = Left, right = Right} | Tail], 0) ->
    is_complete(Tail ++ [Left, Right], 0).

%%%*_ TESTS ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

empty_tree() ->
    ?empty_tree.

single_node_tree() ->
    #node{root = 1}.

only_right_tree() ->
    binarytree([19, 30, 25, 22, 26, 39, 37, 40, 19]).

binarytree(List) ->
    lists:foldl(fun(Key, Acc) -> binarytree_insert(Acc, Key) end,
                empty_tree(), List).
    
binarytree_insert(?empty_tree, K) ->
    #node{root = K};
binarytree_insert(undefined, K) ->
    #node{root = K};
binarytree_insert(#node{root = Root, left = Left, right = Right} = OldTree, K) ->
    if
        K == Root ->
            OldTree;
        K > Root ->
            OldTree#node{right = binarytree_insert(Right, K)};
        K < Root ->
            OldTree#node{left = binarytree_insert(Left, K)}
    end.

count_test_() ->
    [ {"test for count",
        fun() ->
            ?assertEqual(0, count(empty_tree())),
            ?assertEqual(1, count(single_node_tree())),
            ?assertEqual(8, count(only_right_tree()))
        end}
    ].

height_test_() ->
    [ {"test for height",
        fun() ->
            ?assertEqual(0, height(empty_tree())),
            ?assertEqual(1, height(single_node_tree())),
            ?assertEqual(4, height(only_right_tree()))
        end}
    ].

prev_test_() ->
    [ {"test for prev",
        fun() ->
            ?assertEqual([], prev(empty_tree())),
            ?assertEqual([1], prev(single_node_tree())),
            ?assertEqual([19, 30, 25, 22, 26, 39, 37, 40],
                         prev(only_right_tree()))
        end}
    ].

mid_test_() ->
    [ {"test for mid",
        fun() ->
            ?assertEqual([], mid(empty_tree())),
            ?assertEqual([1], mid(single_node_tree())),
            ?assertEqual([19, 22, 25, 26, 30, 37, 39, 40],
                         mid(only_right_tree()))
        end}
    ].

last_test_() ->
    [ {"test for last",
        fun() ->
            ?assertEqual([], last(empty_tree())),
            ?assertEqual([1], last(single_node_tree())),
            ?assertEqual([22, 26, 25, 37, 40, 39, 30, 19],
                         last(only_right_tree()))
        end}
    ].

bfs_test_() ->
    [ {"test for bfs",
        fun() ->
            ?assertEqual([], bfs(empty_tree())),
            ?assertEqual([1], bfs(single_node_tree())),
            ?assertEqual([19, 30, 25, 39, 22, 26, 37, 40],
                         bfs(only_right_tree()))
        end}
    ].

count_k_test_() ->
    [ {"test for count K",
        fun() ->
            ?assertEqual(0, count_k(empty_tree(), 2)),
            ?assertEqual(0, count_k(empty_tree(), -1)),
            ?assertEqual(0, count_k(empty_tree(), 0)),
            ?assertEqual(0, count_k(single_node_tree(), 0)),
            ?assertEqual(1, count_k(single_node_tree(), 1)),
            ?assertEqual(1, count_k(only_right_tree(), 2)),
            ?assertEqual(2, count_k(only_right_tree(), 3)),
            ?assertEqual(4, count_k(only_right_tree(), 4))
        end}
    ].

count_leaf_test_() ->
    [ {"test for count leaf",
        fun() ->
            ?assertEqual(0, count_leaf(empty_tree())),
            ?assertEqual(1, count_leaf(single_node_tree())),
            ?assertEqual(4, count_leaf(only_right_tree()))
        end}
    ].

struct_cmp_test_() ->
    [ {"test for compare tree struct",
        fun() ->
            ?assertEqual(true, struct_cmp(empty_tree(), empty_tree())),
            ?assertEqual(false, struct_cmp(single_node_tree(), empty_tree())),
            ?assertEqual(false, struct_cmp(empty_tree(), single_node_tree())),
            ?assertEqual(true, struct_cmp(only_right_tree(), only_right_tree())),
            ?assertEqual(false, struct_cmp(only_right_tree(), binarytree([19, 18]))),
            ?assertEqual(false, struct_cmp(only_right_tree(), single_node_tree()))
        end}
    ].

reverse_test_() ->
    [ {"test for reverse test",
        fun() ->
            ?assertEqual(?empty_tree, reverse(empty_tree())),
            ?assertEqual(single_node_tree(), reverse(single_node_tree())),
            ?assertEqual([40, 39, 37, 30, 26, 25, 22, 19],
                         mid(reverse(only_right_tree())))
        end}
    ].

get_nearest_common_parent_test_() ->
    [ {"test for get nearest common parent",
        fun() ->
            ?assertEqual(not_found, get_nearest_common_parent(empty_tree(), 1, 1)),
            ?assertEqual(not_found, get_nearest_common_parent(single_node_tree(), 1, 2)),
            ?assertEqual(not_found, get_nearest_common_parent(single_node_tree(), 2, 1)),
            ?assertEqual(30, get_nearest_common_parent(only_right_tree(), 22, 39)),
            ?assertEqual(30, get_nearest_common_parent(only_right_tree(), 39, 22)),
            ?assertEqual(30, get_nearest_common_parent(only_right_tree(), 30, 40)),
            ?assertEqual(30, get_nearest_common_parent(only_right_tree(), 40, 30)),
            ?assertEqual(not_found, get_nearest_common_parent(only_right_tree(), 20, 28)),
            ?assertEqual(not_found, get_nearest_common_parent(only_right_tree(), 22, 28)),
            Tree = binarytree_insert(binarytree_insert(only_right_tree(), 10), 9),
            ?assertEqual(10, get_nearest_common_parent(Tree, 10, 9))
        end}
    ].

get_node_path_test_() ->
    [ {"test for get node path",
        fun() ->
            ?assertEqual([], get_node_path(empty_tree(), 1)),
            ?assertEqual([], get_node_path(single_node_tree(), 2)),
            ?assertEqual([1], get_node_path(single_node_tree(), 1)),
            ?assertEqual([19, 30, 25, 26], get_node_path(only_right_tree(), 26))
        end}
    ].

get_nearest_common_parent_v2_test_() ->
    [ {"test for get nearest common parent v2",
        fun() ->
            ?assertEqual(not_found, get_nearest_common_parent_v2(empty_tree(), 1, 1)),
            ?assertEqual(not_found, get_nearest_common_parent_v2(single_node_tree(), 1, 2)),
            ?assertEqual(not_found, get_nearest_common_parent_v2(single_node_tree(), 2, 1)),
            ?assertEqual(30, get_nearest_common_parent_v2(only_right_tree(), 22, 39)),
            ?assertEqual(30, get_nearest_common_parent_v2(only_right_tree(), 39, 22)),
            ?assertEqual(30, get_nearest_common_parent_v2(only_right_tree(), 30, 40)),
            ?assertEqual(30, get_nearest_common_parent_v2(only_right_tree(), 40, 30)),
            ?assertEqual(not_found, get_nearest_common_parent_v2(only_right_tree(), 20, 28)),
            ?assertEqual(not_found, get_nearest_common_parent_v2(only_right_tree(), 22, 28)),
            Tree = binarytree_insert(binarytree_insert(only_right_tree(), 10), 9),
            ?assertEqual(10, get_nearest_common_parent_v2(Tree, 10, 9))
        end}
    ].

get_biggest_dis_test_() ->
    [ {"test for get biggest dis",
        fun() ->
            ?assertEqual(0, get_biggest_dis(empty_tree())),
            ?assertEqual(0, get_biggest_dis(single_node_tree())),
            ?assertEqual(4, get_biggest_dis(only_right_tree())),
            Tree = binarytree_insert(binarytree_insert(only_right_tree(), 11), 9),
            ?assertEqual(5, get_biggest_dis(Tree))
        end}
    ].

from_last_mid_test_() ->
    [ {"test for generate tree from last and mid",
        fun() ->
            Tree1 = empty_tree(),
            Last1 = last(Tree1),
            Mid1  = mid(Tree1),
            ?assertEqual(undefined, from_last_mid(Last1, Mid1)),
            Tree2 = single_node_tree(),
            Last2 = last(Tree2),
            Mid2  = mid(Tree2),
            ?assertEqual(Tree2, from_last_mid(Last2, Mid2)),
            Tree3 = only_right_tree(),
            Last3 = last(Tree3),
            Mid3  = mid(Tree3),
            ?assertEqual(Tree3, from_last_mid(Last3, Mid3))
        end}
    ].

from_prev_mid_test_() ->
    [ {"test for generate tree from prev and mid",
        fun() ->
            Tree1 = empty_tree(),
            Prev1 = prev(Tree1),
            Mid1  = mid(Tree1),
            ?assertEqual(undefined, from_prev_mid(Prev1, Mid1)),
            Tree2 = single_node_tree(),
            Prev2 = prev(Tree2),
            Mid2  = mid(Tree2),
            ?assertEqual(Tree2, from_prev_mid(Prev2, Mid2)),
            Tree3 = only_right_tree(),
            Prev3 = prev(Tree3),
            Mid3  = mid(Tree3),
            ?assertEqual(Tree3, from_prev_mid(Prev3, Mid3))
        end}
    ].


is_complete_test_() ->
    [ {"test for check is complete",
        fun() ->
            ?assertEqual(true, is_complete(empty_tree())),
            ?assertEqual(true, is_complete(single_node_tree())),
            ?assertEqual(false, is_complete(only_right_tree())),
            Tree1 = binarytree([19, 11, 30, 9]),
            ?assertEqual(true, is_complete(Tree1)),
            Tree2 = binarytree([19, 11, 12, 30]),
            ?assertEqual(false, is_complete(Tree2))
        end}
    ].

-endif.
