-module(siamese_tests).
-include_lib("eunit/include/eunit.hrl").


new_test_() ->
    S1 = siamese:new(),
    ?_assertEqual(length(S1), 1).


open_scope_test_() ->
    S1 = siamese:new(),
    S2 = siamese:open_scope(S1),
    [?_assertEqual(length(S1), 1),
     ?_assertEqual(length(S2), 2)].


close_scope_test_() ->
    S1 = siamese:new(),
    S2 = siamese:open_scope(S1),
    S3 = siamese:close_scope(S2),
    [?_assertEqual(length(S1), 1),
     ?_assertEqual(length(S2), 2),
     ?_assertEqual(length(S3), 1)].


from_list_test_() ->
    S = siamese:from_list([{abc, 2}, {def, 3}, {ghi, 5}]),
    [?_assertEqual(siamese:get(abc, S), 2),
     ?_assertEqual(siamese:get(def, S), 3),
     ?_assertEqual(siamese:get(ghi, S), 5)].


to_list_test_() ->
    S = siamese:from_list([{abc, 2}, {def, 3}, {ghi, 5}]),
    L = siamese:to_list(S),
    [?_assertEqual(lists:keyfind(abc, 1, L), {abc, 2}),
     ?_assertEqual(lists:keyfind(def, 1, L), {def, 3}),
     ?_assertEqual(lists:keyfind(ghi, 1, L), {ghi, 5})].


add_test_() ->
    S1 = siamese:new(),
    S2 = siamese:add(abc, 2, S1),
    S3 = siamese:add(def, 3, S2),
    S4 = siamese:add(ghi, 5, S3),
    [?_assertEqual(siamese:size(S2), 1),
     ?_assertEqual(siamese:size(S3), 2),
     ?_assertEqual(siamese:size(S4), 3)].


add_duplicate_test_() ->
    S1 = siamese:from_list([{abc, 2}, {def, 3}, {ghi, 5}]),
    S2 = siamese:open_scope(S1),
    S3 = siamese:add(abc, 7, S2),
    [?_assertEqual(siamese:add(abc, 7, S1), key_already_exists),
     ?_assertEqual(siamese:size(S3), 4),
     ?_assertEqual(siamese:get(abc, S1), 2),
     ?_assertEqual(siamese:get(abc, S3), 7)].


remove_test_() ->
    S1 = siamese:from_list([{abc, 2}, {def, 3}, {ghi, 5}]),
    S2 = siamese:remove(abc, S1),
    S3 = siamese:remove(def, S2),
    S4 = siamese:remove(ghi, S3),
    [?_assertEqual(siamese:size(S2), 2),
     ?_assertEqual(siamese:size(S3), 1),
     ?_assertEqual(siamese:size(S4), 0)].


size_test_() ->
    S1 = siamese:from_list([{abc, 2}, {def, 3}, {ghi, 5}]),
    S2 = siamese:open_scope(S1),
    S3 = lists:foldl(fun({K,V}, S) -> siamese:add(K, V, S) end,
                     S2,
                     ([{abc, 7}, {def, 11}, {ghi, 13}])),
    [?_assertEqual(siamese:size(S1), 3),
     ?_assertEqual(siamese:size(S2), 3),
     ?_assertEqual(siamese:size(S3), 6)].
