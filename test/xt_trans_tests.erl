
%% Copyright (c) JinGan <jg_513@163.com>

-module(xt_trans_tests).

-compile(export_all).

-compile([{parse_transform, xt_trans}]).

-record(rec1, {
    f1 = 1,
    f2 = 2,
    f3 = 3
}).

-record(rec2, {
    f1 = 11,
    f3 = 12,
    f4 = 13,
    f5 = 14
}).

formater(Field) ->
    integer_to_list(Field).

%% copy fields
copy_3() ->
    Rec2 = #rec2{f1 = 101, f3 = 102, f4 = 103, f5 = 104},
    record_copy({rec1}, {rec2, Rec2}).

copy_4() ->
    Rec1 = #rec1{f1 = 4, f2 = 5, f3 = 6},
    Rec2 = #rec2{f1 = 101, f3 = 102, f4 = 103, f5 = 104},
    record_copy({rec1, Rec1}, {rec2, Rec2}).

%% copy fields with formaters
f_copy_4() ->
    Rec2 = #rec2{f1 = 101, f3 = 102, f4 = 103, f5 = 104},
    record_copy({rec1}, {rec2, Rec2, [{f1, formater}, {any, module:function}]}).

f_copy_5() ->
    Rec1 = #rec1{f1 = 4, f2 = 5, f3 = 6},
    Rec2 = #rec2{f1 = 101, f3 = 102, f4 = 103, f5 = 104},
    record_copy({rec1, Rec1}, {rec2, Rec2, [{f1, integer_to_list}]}).

%% assign fields
assign_2() ->
    List = [101, 102, 103],
    record_assign({rec1}, {List}).

assign_3() ->
    List = [101, 102, 103, 104],
    record_assign({rec1}, {rec2, List}).

assign_4() ->
    Rec1 = #rec1{f1 = 4, f2 = 5, f3 = 6},
    List = [101, 102, 103, 104],
    record_assign({rec1, Rec1}, {rec2, List}).

assign_list_2() ->
    record_assign({rec1}, {[101, 102, 103]}).

assign_list_3() ->
    record_assign({rec1}, {rec2, [101, 102, 103, 104]}).

assign_list_4() ->
    Rec1 = #rec1{f1 = 4, f2 = 5, f3 = 6},
    record_assign({rec1, Rec1}, {rec2, [101, 102, 103, 104]}).

assign_fields_3() ->
    List = [101, 102, 103, 104],
    record_assign({rec1}, {List, [f1, undefined, any, f3]}).

assign_fields_4() ->
    Rec1 = #rec1{f1 = 4, f2 = 5, f3 = 6},
    List = [101, 102, 103, 104],
    record_assign({rec1, Rec1}, {List, [f1, undefined, any, f3]}).

assign_fields_list_3() ->
    record_assign({rec1}, {[101, 102, 103, 104], [f1, undefined, any, f3]}).

assign_fields_list_4() ->
    Rec1 = #rec1{f1 = 4, f2 = 5, f3 = 6},
    record_assign({rec1, Rec1}, {[101, 102, 103, 104], [f1, undefined, any, f3]}).


%% assign fields with formaters
f_assign_3() ->
    List = [101, 102, 103],
    record_assign({rec1}, {List, [{f1, integer_to_list}]}).

f_assign_4() ->
    List = [101, 102, 103, 104],
    record_assign({rec1}, {rec2, List, [{f1, integer_to_list}]}).

f_assign_5() ->
    Rec1 = #rec1{f1 = 4, f2 = 5, f3 = 6},
    List = [101, 102, 103, 104],
    record_assign({rec1, Rec1}, {rec2, List, [{f1, integer_to_list}]}).

f_assign_list_3() ->
    record_assign({rec1}, {[101, 102, 103], [{f1, integer_to_list}]}).

f_assign_list_4() ->
    record_assign({rec1}, {rec2, [101, 102, 103, 104], [{f1, integer_to_list}]}).

f_assign_list_5() ->
    Rec1 = #rec1{f1 = 4, f2 = 5, f3 = 6},
    record_assign({rec1, Rec1}, {rec2, [101, 102, 103, 104], [{f1, integer_to_list}]}).

f_assign_fields_3() ->
    List = [101, 102, 103, 104],
    record_assign({rec1}, {List, [{f1, integer_to_list}, undefined, any, f3]}).

f_assign_fields_4() ->
    Rec1 = #rec1{f1 = 4, f2 = 5, f3 = 6},
    List = [101, 102, 103, 104],
    record_assign({rec1, Rec1}, {List, [{f1, integer_to_list}, undefined, any, f3]}).

f_assign_fields_list_3() ->
    record_assign({rec1}, {[101, 102, 103, 104], [{f1, integer_to_list}, undefined, any, f3]}).

f_assign_fields_list_4() ->
    Rec1 = #rec1{f1 = 4, f2 = 5, f3 = 6},
    record_assign({rec1, Rec1}, {[101, 102, 103, 104], [{f1, integer_to_list}, undefined, any, f3]}).


-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

copy_test_() ->
    [
        ?_assertEqual(#rec1{f1 = 101, f2 = 2, f3 = 102}, copy_3()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 5, f3 = 102}, copy_4())
    ].

f_copy_test_() ->
    [
        ?_assertEqual(#rec1{f1 = "101", f2 = 2, f3 = 102}, f_copy_4()),
        ?_assertEqual(#rec1{f1 = "101", f2 = 5, f3 = 102}, f_copy_5())
    ].

assign_test_() ->
    [
        ?_assertEqual(#rec1{f1 = 101, f2 = 102, f3 = 103}, assign_2()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 2, f3 = 102}, assign_3()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 5, f3 = 102}, assign_4())
    ].

f_assign_test_() ->
    [
        ?_assertEqual(#rec1{f1 = "101", f2 = 102, f3 = 103}, f_assign_3()),
        ?_assertEqual(#rec1{f1 = "101", f2 = 2, f3 = 102}, f_assign_4()),
        ?_assertEqual(#rec1{f1 = "101", f2 = 5, f3 = 102}, f_assign_5())
    ].

assign_list_test_() ->
    [
        ?_assertEqual(#rec1{f1 = 101, f2 = 102, f3 = 103}, assign_list_2()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 2, f3 = 102}, assign_list_3()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 5, f3 = 102}, assign_list_4())
    ].

f_assign_list_test_() ->
    [
        ?_assertEqual(#rec1{f1 = "101", f2 = 102, f3 = 103}, f_assign_list_3()),
        ?_assertEqual(#rec1{f1 = "101", f2 = 2, f3 = 102}, f_assign_list_4()),
        ?_assertEqual(#rec1{f1 = "101", f2 = 5, f3 = 102}, f_assign_list_5())
    ].

assign_fields_test_() ->
    [
        ?_assertEqual(#rec1{f1 = 101, f2 = 2, f3 = 104}, assign_fields_3()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 5, f3 = 104}, assign_fields_4())
    ].

f_assign_fields_test_() ->
    [
        ?_assertEqual(#rec1{f1 = "101", f2 = 2, f3 = 104}, f_assign_fields_3()),
        ?_assertEqual(#rec1{f1 = "101", f2 = 5, f3 = 104}, f_assign_fields_4())
    ].

assign_fields_list_test_() ->
    [
        ?_assertEqual(#rec1{f1 = 101, f2 = 2, f3 = 104}, assign_fields_list_3()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 5, f3 = 104}, assign_fields_list_4())
    ].

f_assign_fields_list_test_() ->
    [
        ?_assertEqual(#rec1{f1 = "101", f2 = 2, f3 = 104}, f_assign_fields_list_3()),
        ?_assertEqual(#rec1{f1 = "101", f2 = 5, f3 = 104}, f_assign_fields_list_4())
    ].

-endif.
