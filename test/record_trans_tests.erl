
%% Copyright (c) jg_513@163.com, https://github.com/jg513

-module(record_trans_tests).

-compile(export_all).

-compile([{parse_transform, record_trans}]).

-record(dst, {
    f1 = 1,
    f2 = 2
}).

-record(src, {
    f2 = 11,
    f3 = 12,
    f4 = 13
}).

formater(F1, F2) ->
    F1 + F2.

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

copy_dst_test_() ->
    Dst = #dst{},
    [
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_copy(dst, src)),
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_copy({dst}, src)),
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_copy(#dst{}, src)),
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_copy({#dst{}}, src)),
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_copy({dst, #dst{}}, src)),
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_copy({dst, Dst}, src))
    ].

copy_src_test_() ->
    Src = #src{},
    [
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_copy(dst, src)),
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_copy(dst, {src})),
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_copy(dst, {#src{}})),
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_copy(dst, {src, #src{}})),
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_copy(dst, {src, Src}))
    ].

copy_fmt_test_() ->
    [
        ?_assertEqual(#dst{f1 = 1, f2 = "11"}, record_copy(dst, {src, [{f2, erlang:integer_to_list}]})),
        ?_assertEqual(#dst{f1 = 1, f2 = 25}, record_copy(dst, {src, [{f2, formater, [f3, f4]}]}))
    ].

assign_dst_test() ->
    Dst = #dst{},
    [
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_assign(dst, [11, 12])),
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_assign({dst}, [11, 12])),
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_assign(#dst{}, [11, 12])),
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_assign({#dst{}}, [11, 12])),
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_assign({dst, #dst{}}, [11, 12])),
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_assign({dst, Dst}, [11, 12]))
    ].

assign_list_src_test_() ->
    [
        ?_assertEqual(#dst{f1 = 11, f2 = 12}, record_assign(dst, [11, 12])),
        ?_assertEqual(#dst{f1 = 11, f2 = 12}, record_assign(dst, {[11, 12]})),
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_assign(dst, {src, [11, 12, 13]})),
        ?_assertEqual(#dst{f1 = 1, f2 = 12}, record_assign(dst, {[11, 12, 13], [other, f2, any]}))
    ].

assign_list_fmt_test_() ->
    [
        ?_assertEqual(#dst{f1 = 11, f2 = 23}, record_assign(dst, {[11, 12], [{f2, formater, [f1, f2]}]})),
        ?_assertEqual(#dst{f1 = 1, f2 = 25}, record_assign(dst, {src, [11, 12, 13], [{f2, formater, [f3, f4]}]})),
        ?_assertEqual(#dst{f1 = 1, f2 = 25}, record_assign(dst, {[11, 12, 13], [other, f2, any], [{f2, formater, [f2, any]}]}))
    ].

assign_variable_src_test_() ->
    List2 = [11, 12],
    List3 = [11, 12, 13],
    [
        ?_assertEqual(#dst{f1 = 11, f2 = 12}, record_assign(dst, List2)),
        ?_assertEqual(#dst{f1 = 11, f2 = 12}, record_assign(dst, {List2})),
        ?_assertEqual(#dst{f1 = 1, f2 = 11}, record_assign(dst, {src, List3})),
        ?_assertEqual(#dst{f1 = 1, f2 = 12}, record_assign(dst, {List3, [other, f2, any]}))
    ].

assign_variable_fmt_test_() ->
    List2 = [11, 12],
    List3 = [11, 12, 13],
    [
        ?_assertEqual(#dst{f1 = 11, f2 = 23}, record_assign(dst, {List2, [{f2, formater, [f1, f2]}]})),
        ?_assertEqual(#dst{f1 = 1, f2 = 25}, record_assign(dst, {src, List3, [{f2, formater, [f3, f4]}]})),
        ?_assertEqual(#dst{f1 = 1, f2 = 25}, record_assign(dst, {List3, [other, f2, any], [{f2, formater, [f2, any]}]}))
    ].

get_test_() ->
    Rec = #src{},
    Key = f3,
    [
        ?_assertEqual(12, record_get(src, Key)),
        ?_assertEqual(12, record_get({src, Rec}, Key)),
        ?_assertEqual([11, 12], record_get(src, [f2, f3])),
        ?_assertEqual(["11", 12], record_get({src, Rec}, {[f2, f3], [{f2, integer_to_list}]}))
    ].

-endif.
