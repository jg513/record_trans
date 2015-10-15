
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

copy_3() ->
    Rec2 = #rec2{f1 = 101, f3 = 102, f4 = 103, f5 = 104},
    record_copy(rec1, rec2, Rec2).

copy_4() ->
    Rec1 = #rec1{f1 = 4},
    Rec2 = #rec2{f1 = 101, f3 = 102, f4 = 103, f5 = 104},
    record_copy(rec1, Rec1, rec2, Rec2).

assign_3() ->
    List = [101, 102, 103, 104],
    record_assign(rec1, rec2, List).

assign_4() ->
    Rec1 = #rec1{f1 = 4},
    List = [101, 102, 103, 104],
    record_assign(rec1, Rec1, rec2, List).

assign_list_3() ->
    record_assign(rec1, rec2, [101, 102, 103, 104]).

assign_list_4() ->
    Rec1 = #rec1{f1 = 4},
    record_assign(rec1, Rec1, rec2, [101, 102, 103, 104]).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

all_test_() ->
    Result = #rec1{f1 = 101, f2 = 2, f3 = 102},
    [
        ?_assertEqual(copy_3(), Result),
        ?_assertEqual(copy_4(), Result),
        ?_assertEqual(assign_3(), Result),
        ?_assertEqual(assign_4(), Result),
        ?_assertEqual(assign_list_3(), Result),
        ?_assertEqual(assign_list_4(), Result)
    ].

-endif.
