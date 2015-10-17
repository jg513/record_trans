
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

%% tests for destination record
copy_1() ->
    Rec2 = #rec2{f1 = 101, f3 = 102, f4 = 103, f5 = 104},
    record_copy({rec1}, {rec2, Rec2}).

copy_2() ->
    Rec2 = #rec2{f1 = 101, f3 = 102, f4 = 103, f5 = 104},
    record_copy({#rec1{f1 = 4, f2 = 5, f3 = 6}}, {rec2, Rec2}).

copy_3() ->
    Rec1 = #rec1{f1 = 4, f2 = 5, f3 = 6},
    Rec2 = #rec2{f1 = 101, f3 = 102, f4 = 103, f5 = 104},
    record_copy({rec1, Rec1}, {rec2, Rec2}).

copy_4() ->
    Rec2 = #rec2{f1 = 101, f3 = 102, f4 = 103, f5 = 104},
    record_copy({rec1, #rec1{f1 = 4, f2 = 5, f3 = 6}}, {rec2, Rec2}).

assign_1() ->
    record_assign({rec1}, {[101, 102, 103]}).

assign_2() ->
    record_assign({#rec1{f1 = 4, f2 = 5, f3 = 6}}, {[101, 102, 103]}).

assign_3() ->
    Rec1 = #rec1{f1 = 4, f2 = 5, f3 = 6},
    record_assign({rec1, Rec1}, {[101, 102, 103]}).

assign_4() ->
    record_assign({rec1, #rec1{f1 = 4, f2 = 5, f3 = 6}}, {[101, 102, 103]}).

%% record_copy with formaters
f_copy_1() ->
    Rec2 = #rec2{f1 = 101, f3 = 102, f4 = 103, f5 = 104},
    record_copy({rec1}, {rec2, Rec2, [{f1, ?MODULE:formater}, {any, module:function}]}).

f_copy_2() ->
    record_copy({rec1}, {rec2, #rec2{f1 = 101, f3 = 102, f4 = 103, f5 = 104}, [{f1, ?MODULE:formater}, {any, module:function}]}).

%% assign list fields
assign_list_1() ->
    record_assign({rec1}, {[101, 102, 103]}).

assign_list_2() ->
    record_assign({rec1}, {rec2, [101, 102, 103, 104]}).

assign_list_3() ->
    record_assign({rec1}, {[101, 102, 103, 104], [f1, undefined, any, f3]}).

%% assign list fields with formaters
f_assign_list_1() ->
    record_assign({rec1}, {[101, 102, 103], [{f1, integer_to_list}]}).

f_assign_list_2() ->
    record_assign({rec1}, {rec2, [101, 102, 103, 104], [{f1, integer_to_list}]}).

f_assign_list_3() ->
    record_assign({rec1}, {[101, 102, 103, 104], [{f1, integer_to_list}, undefined, any, f3]}).

%% assign variable fields
assign_variable_1() ->
    List = [101, 102, 103],
    record_assign({rec1}, {List}).

assign_variable_2() ->
    List = [101, 102, 103, 104],
    record_assign({rec1}, {rec2, List}).

assign_variable_3() ->
    List = [101, 102, 103, 104],
    record_assign({rec1}, {List, [f1, undefined, any, f3]}).

%% assign variable fields with formaters
f_assign_variable_1() ->
    List = [101, 102, 103],
    record_assign({rec1}, {List, [{f1, integer_to_list}]}).

f_assign_variable_2() ->
    List = [101, 102, 103, 104],
    record_assign({rec1}, {rec2, List, [{f1, integer_to_list}]}).

f_assign_variable_3() ->
    List = [101, 102, 103, 104],
    record_assign({rec1}, {List, [f1, undefined, any, f3], [{f1, integer_to_list}]}).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

destination_test_() ->
    [
        ?_assertEqual(#rec1{f1 = 101, f2 = 2, f3 = 102}, copy_1()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 5, f3 = 102}, copy_2()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 5, f3 = 102}, copy_3()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 5, f3 = 102}, copy_4()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 102, f3 = 103}, assign_1()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 102, f3 = 103}, assign_2()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 102, f3 = 103}, assign_3()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 102, f3 = 103}, assign_4())
    ].

f_copy_test_() ->
    [
        ?_assertEqual(#rec1{f1 = "101", f2 = 2, f3 = 102}, f_copy_1()),
        ?_assertEqual(#rec1{f1 = "101", f2 = 2, f3 = 102}, f_copy_2())
    ].

assign_list_test_() ->
    [
        ?_assertEqual(#rec1{f1 = 101, f2 = 102, f3 = 103}, assign_list_1()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 2, f3 = 102}, assign_list_2()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 2, f3 = 104}, assign_list_3())
    ].

f_assign_list_test_() ->
    [
        ?_assertEqual(#rec1{f1 = "101", f2 = 102, f3 = 103}, f_assign_list_1()),
        ?_assertEqual(#rec1{f1 = "101", f2 = 2, f3 = 102}, f_assign_list_2()),
        ?_assertEqual(#rec1{f1 = "101", f2 = 2, f3 = 104}, f_assign_list_3())
    ].


assign_variable_test_() ->
    [
        ?_assertEqual(#rec1{f1 = 101, f2 = 102, f3 = 103}, assign_variable_1()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 2, f3 = 102}, assign_variable_2()),
        ?_assertEqual(#rec1{f1 = 101, f2 = 2, f3 = 104}, assign_variable_3())
    ].

f_assign_variable_test_() ->
    [
        ?_assertEqual(#rec1{f1 = "101", f2 = 102, f3 = 103}, f_assign_variable_1()),
        ?_assertEqual(#rec1{f1 = "101", f2 = 2, f3 = 102}, f_assign_variable_2()),
        ?_assertEqual(#rec1{f1 = "101", f2 = 2, f3 = 104}, f_assign_variable_3())
    ].

-endif.
