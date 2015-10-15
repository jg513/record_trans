
%% Copyright (c) JinGan <jg_513@163.com>

-module(xt_trans_sample).

-compile(export_all).

-compile([{parse_transform, xt_trans}]).

-record(rec1, {
    field1 = 1,
    field2 = 2,
    field3 = 3
}).

-record(rec2, {
    field1 = 11,
    field2 = 12,
    field4 = 13
}).

record_copy() ->
    Rec1 = #rec1{},
    Rec2 = #rec2{},
    record_copy(rec1, Rec1, rec2, Rec2).
