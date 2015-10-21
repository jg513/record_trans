
%% Copyright (c) JinGan <jg_513@163.com>

-module(xt_trans).

-export([parse_transform/2]).

-record(state, {
    records = dict:new(),
    options = []
}).

%% External API
parse_transform(Forms0, Options0) ->
    Records = records(Forms0),
    State0 = #state{records = Records, options = Options0},
    {Forms, _State} = forms(Forms0, State0),
    Forms.

%% init state records
records(Forms) ->
    Definitions = [erl_syntax:attribute_arguments(Form) || Form <- Forms,
        erl_syntax:type(Form) =:= attribute,
        erl_syntax:atom_value(erl_syntax:attribute_name(Form)) =:= record],
    records(Definitions, dict:new()).

records([Record | Tail], Dict0) ->
    [RecName, RecFields] = Record,
    {Key, Value} = to_element(RecName, RecFields),
    Dict = dict:store(Key, Value, Dict0),
    records(Tail, Dict);
records([], Dict) ->
    Dict.

to_element(RecName0, RecFields) ->
    FieldNames = [erl_syntax:record_field_name(Field)
        || Field <- erl_syntax:tuple_elements(RecFields)],
    RecName = erl_syntax:atom_value(RecName0),
    {RecName, FieldNames}.

%% traverse forms
forms([Head | Tail], State0) when is_list(Head) ->
    {Forms0, State1} = forms(Head, State0),
    {Forms1, State} = forms(Tail, State1),
    {[Forms0 | Forms1], State};
forms([Form0 | Forms0], State0) ->
    {Form1, State1} = form(erl_syntax:type(Form0), Form0, State0),
    {Form, State3} =
        case erl_syntax:subtrees(Form1) of
            [] ->
                {Form1, State1};
            List ->
                {Forms1, State2} = forms(List, State1),
                Form2 = erl_syntax:update_tree(Form1, Forms1),
                {erl_syntax:revert(Form2), State2}
        end,
    {Forms, State} = forms(Forms0, State3),
    {[Form | Forms], State};
forms([], State) ->
    {[], State}.

form(application, Form0, #state{records = Records} = State) ->
    Oprt = erl_syntax:application_operator(Form0),
    Args = erl_syntax:application_arguments(Form0),
    Form = transform(erl_syntax:type(Oprt), Form0, Oprt, Args, Records),
    {Form, State};
form(_, Form, State) ->
    {Form, State}.

%% transform
transform(atom, Form, Oprt, Args0, Records) ->
    case transform_args_check(Args0) of
        {true, Args} ->
            try do_transform(erl_syntax:atom_value(Oprt), Args, Form, Records)
            catch
                _Err: _Reason ->
                    Form
            end;
        _ ->
            Form
    end;
transform(_, Form, _Oprt, _Args, _Records) ->
    Form.

transform_args_check(Args) ->
    case Args of
        [DArgs0, SArgs0] ->
            DArgs =
                case erl_syntax:type(DArgs0) of
                    atom ->
                        erl_syntax:tuple([DArgs0]);
                    record_expr ->
                        erl_syntax:tuple([DArgs0]);
                    tuple ->
                        DArgs0;
                    _ ->
                        false
                end,
            SArgs =
                case erl_syntax:type(SArgs0) of
                    atom ->
                        erl_syntax:tuple([SArgs0]);
                    list ->
                        erl_syntax:tuple([SArgs0]);
                    variable ->
                        erl_syntax:tuple([SArgs0]);
                    record_expr ->
                        erl_syntax:tuple([SArgs0]);
                    tuple ->
                        SArgs0;
                    _ ->
                        false
                end,
            case lists:member(false, [DArgs, SArgs]) of
                true ->
                    false;
                _ ->
                    {true, [DArgs, SArgs]}
            end;
        _ ->
            false
    end.

do_transform(record_copy, Args, Form, Records) ->
    [DArgs0, SArgs0] = Args,
    DArgs1 = erl_syntax:tuple_elements(DArgs0),
    SArgs1 = erl_syntax:tuple_elements(SArgs0),
    case copy_args_check(DArgs1, SArgs1) of
        {true, [DArgs, SArgs]} ->
            copy_transform(DArgs, SArgs, Form, Records);
        _ ->
            Form
    end;
do_transform(record_assign, Args, Form, Records) ->
    [DArgs0, SArgs0] = Args,
    DArgs = erl_syntax:tuple_elements(DArgs0),
    SArgs = erl_syntax:tuple_elements(SArgs0),
    case assign_args_check(DArgs, SArgs) of
        {true, assign_list} ->
            assign_list(DArgs, SArgs, Form, Records);
        {pre_transform, assign_list} ->
            pre_assign_list(DArgs, SArgs, Form, Records);
        {true, assign_variable} ->
            assign_variable(DArgs, SArgs, Form, Records);
        {pre_transform, assign_variable} ->
            pre_assign_variable(DArgs, SArgs, Form, Records);
        _ ->
            Form
    end;
do_transform(_Name, _Args, Form, _Records) ->
    Form.

%% copy and transform fields
copy_args_check(DArgs0, SArgs0) ->
    DArgs =
        case [erl_syntax:type(Arg) || Arg <- DArgs0] of
            [atom] ->
                DArgs0;
            [record_expr] ->
                [DArg] = DArgs0,
                [erl_syntax:record_expr_type(DArg), DArg];
            [atom, variable] ->
                DArgs0;
            [atom, record_expr] ->
                DArgs0;
            _ ->
                false
        end,
    SArgs =
        case [erl_syntax:type(Arg) || Arg <- SArgs0] of
            [atom] ->
                [SArg1] = SArgs0,
                SArg2 = erl_syntax:revert(erl_syntax:record_expr(SArg1, [])),
                [SArg1, SArg2];
            [record_expr] ->
                [SArg] = SArgs0,
                [erl_syntax:record_expr_type(SArg), SArg];
            [atom, variable] ->
                SArgs0;
            [atom, record_expr] ->
                SArgs0;
            [atom, list] ->
                [SArg1, SArg2] = SArgs0,
                SArg3 = erl_syntax:revert(erl_syntax:record_expr(SArg1, [])),
                [SArg1, SArg3, SArg2];
            [record_expr, list] ->
                [SArg1, SArg2] = SArgs0,
                [erl_syntax:record_expr_type(SArg1), SArg1, SArg2];
            [atom, variable, list] ->
                SArgs0;
            [atom, record_expr, list] ->
                SArgs0;
            _ ->
                false
        end,
    Args = [DArgs, SArgs],
    case lists:member(false, Args) of
        true ->
            false;
        _ ->
            {true, Args}
    end.

copy_transform([DRec, DVar], SArgs, Form0, Records) ->
    Form = copy_transform([DRec], SArgs, Form0, Records),
    record_expr_add_arg(DVar, Form);
copy_transform([DRec], [SRec, SVar], _Form, Records) ->
    copy_fields(DRec, SRec, SVar, Records, [], []);
copy_transform([DRec], [SRec, SV1, Fmtrs0], _Form, Records) ->
    case erl_syntax:type(SV1) of
        variable ->
            {formaters, Fmtrs, _Extras, Covers} = check_list_elments(Fmtrs0),
            copy_fields(DRec, SRec, SV1, Records, Fmtrs, Covers);
        record_expr ->
            {formaters, Fmtrs, _Extras, Covers} = check_list_elments(Fmtrs0),
            copy_fields(DRec, SRec, SV1, Records, Fmtrs, Covers)
    end.

copy_fields(DRec, SRec, SVar, Records, Fmtrs0, Covers) ->
    DFields = atom_fields(Records, DRec),
    SFields = atom_fields(Records, SRec),
    Fmtrs = formaters(Fmtrs0),
    Fields = [
        case lists:keyfind(Field, 1, Fmtrs) of
            false ->
                record_field(SVar, SRec, Field);
            Fmtr ->
                f_record_field(SVar, SRec, erl_syntax:atom(Field), Fmtr)
        end || Field <- DFields, lists:member(Field, SFields ++ Covers)],
    erl_syntax:record_expr(DRec, Fields).

%% assign and transform fields
assign_args_check(DArgs, SArgs) ->
    DCheck =
        case [erl_syntax:type(Arg) || Arg <- DArgs] of
            [atom] ->
                true;
            [record_expr] ->
                pre_transform;
            [atom, variable] ->
                true;
            [atom, record_expr] ->
                true;
            _ ->
                false
        end,
    SCheck =
        case [erl_syntax:type(Arg) || Arg <- SArgs] of
            [list] ->
                assign_list;
            [list, list] ->
                assign_list;
            [list, list, list] ->
                assign_list;
            [atom, list] ->
                assign_list;
            [atom, list, list] ->
                assign_list;

            [variable] ->
                assign_variable;
            [variable, list] ->
                assign_variable;
            [atom, variable] ->
                assign_variable;
            [atom, variable, list] ->
                assign_variable;
            [variable, list, list] ->
                assign_variable;
            _ ->
                false
        end,
    {DCheck, SCheck}.

pre_assign_list([DRecExpr], SArgs, Form, Records) ->
    DRec = erl_syntax:record_expr_type(DRecExpr),
    assign_list([DRec, DRecExpr], SArgs, Form, Records).

assign_list([DRec, DVar], SArgs, Form0, Records) ->
    Form = assign_list([DRec], SArgs, Form0, Records),
    record_expr_add_arg(DVar, Form);
assign_list([DRec], [SVar], Form, Records) ->
    assign_list([DRec], [DRec, SVar], Form, Records);
assign_list([DRec], [SV1, SV2], Form, Records) ->
    case erl_syntax:type(SV1) of
        atom ->
            SFields = atom_fields(Records, SV1),
            assign_list_fields(DRec, SFields, SV2, Records, [], []);
        list ->
            Length = erl_syntax:list_length(SV1),
            case check_list_elments(SV2) of
                {fields, Length, SFields} ->
                    assign_list_fields(DRec, SFields, SV1, Records, [], []);
                {formaters, Fmtrs, _Extras, Covers} ->
                    SFields = atom_fields(Records, DRec),
                    assign_list_fields(DRec, SFields, SV1, Records, Fmtrs, Covers);
                false ->
                    Form
            end
    end;
assign_list([DRec], [SV1, SVals, SFmtrs], Form, Records) ->
    case erl_syntax:type(SV1) of
        atom ->
            SFields = atom_fields(Records, SV1),
            Length = erlang:length(SFields),
            case erl_syntax:list_length(SVals) of
                Length ->
                    case check_list_elments(SFmtrs) of
                        {formaters, Fmtrs, _Extras, Covers} ->
                            assign_list_fields(DRec, SFields, SVals, Records, Fmtrs, Covers);
                        _ ->
                            Form
                    end;
                _ ->
                    Form
            end;
        list ->
            Length = erl_syntax:list_length(SV1),
            case {check_list_elments(SVals), check_list_elments(SFmtrs)} of
                {{fields, Length, SFields}, {formaters, Fmtrs, _Extras, Covers}} ->
                    assign_list_fields(DRec, SFields, SV1, Records, Fmtrs, Covers);
                _ ->
                    Form
            end
    end.

assign_list_fields(DRec, SFields, SVar, Records, Fmtrs0, Covers) when is_list(SFields) ->
    Fmtrs = formaters(Fmtrs0),
    DFields = atom_fields(Records, DRec),
    Dict = dict:from_list(lists:zip(SFields, erl_syntax:list_elements(SVar))),
    Fields = [
        case lists:keyfind(Field, 1, Fmtrs) of
            false ->
                record_field(Field, dict:fetch(Field, Dict));
            Fmtr ->
                f_record_field(erl_syntax:atom(Field), Dict, Fmtr)
        end || Field <- DFields, lists:member(Field, SFields ++ Covers)],
    erl_syntax:record_expr(DRec, Fields).

pre_assign_variable([DRecExpr], SArgs, Form, Records) ->
    DRec = erl_syntax:record_expr_type(DRecExpr),
    assign_variable([DRec, DRecExpr], SArgs, Form, Records).

assign_variable([DRec, DVar], SArgs, Form0, Records) ->
    Form = assign_variable([DRec], SArgs, Form0, Records),
    record_expr_add_arg(DVar, Form);
assign_variable([DRec], [SVar], Form, Records) ->
    assign_variable([DRec], [DRec, SVar], Form, Records);
assign_variable([DRec], [SV1, SV2], Form, Records) ->
    case erl_syntax:type(SV1) of
        atom ->
            SFields = atom_fields(Records, SV1),
            assign_variable_fields(DRec, SFields, SV2, Records, [], [], []);
        variable ->
            case check_list_elments(SV2) of
                {fields, _, SFields} ->
                    assign_variable_fields(DRec, SFields, SV1, Records, [], [], []);
                {formaters, Fmtrs, Extras, Covers} ->
                    SFields = atom_fields(Records, DRec),
                    assign_variable_fields(DRec, SFields, SV1, Records, Fmtrs, Extras, Covers);
                false ->
                    Form
            end
    end;
assign_variable([DRec], [SV1, SVals, SFmtrs], Form, Records) ->
    case erl_syntax:type(SV1) of
        atom ->
            case check_list_elments(SFmtrs) of
                {formaters, Fmtrs, Extras, Covers} ->
                    SFields = atom_fields(Records, SV1),
                    assign_variable_fields(DRec, SFields, SVals, Records, Fmtrs, Extras, Covers);
                false ->
                    Form
            end;
        variable ->
            case {check_list_elments(SVals), check_list_elments(SFmtrs)} of
                {{fields, _Length, SFields}, {formaters, Fmtrs, Extras, Covers}} ->
                    assign_variable_fields(DRec, SFields, SV1, Records, Fmtrs, Extras, Covers);
                _ ->
                    Form
            end
    end.

assign_variable_fields(DRec, SFields, SVals, Records, Fmtrs0, Extras, Covers) ->
    Fmtrs = formaters(Fmtrs0),
    DFields = atom_fields(Records, DRec),
    MatchFields = fields_umerge(Extras, DFields) -- Covers,
    MatchList = [
        begin
            case lists:member(Field, MatchFields) of
                true ->
                    {Field, temp_var(Field)};
                _ ->
                    {Field, erl_syntax:variable("_")}
            end
        end || Field <- SFields],
    Pattern = erl_syntax:revert(erl_syntax:list([F || {_, F} <- MatchList])),
    Dict = dict:from_list(MatchList),
    Match = erl_syntax:match_expr(Pattern, SVals),
    Fields = [
        case lists:keyfind(Field, 1, Fmtrs) of
            false ->
                record_field(Field, dict:fetch(Field, Dict));
            Fmtr ->
                f_record_field(erl_syntax:atom(Field), Fmtr)
        end || Field <- DFields, lists:member(Field, SFields ++ Covers)],
    Record = erl_syntax:record_expr(DRec, Fields),
    erl_syntax:block_expr([Match, Record]).

record_expr_add_arg(Arg, Form) ->
    case erl_syntax:type(Form) of
        record_expr ->
            Type = erl_syntax:record_expr_type(Form),
            Fields = erl_syntax:record_expr_fields(Form),
            erl_syntax:record_expr(Arg, Type, Fields);
        block_expr ->
            [Match, Record] = erl_syntax:block_expr_body(Form),
            Type = erl_syntax:record_expr_type(Record),
            Fields = erl_syntax:record_expr_fields(Record),
            erl_syntax:block_expr([Match, erl_syntax:record_expr(Arg, Type, Fields)])
    end.

%% Transform Internal API
field_names(Records, Type) ->
    Key = erl_syntax:atom_value(Type),
    case dict:find(Key, Records) of
        {ok, Value} ->
            Value;
        _ ->
            []
    end.

atom_fields(Records, Type) ->
    [erl_syntax:atom_value(Field) || Field <- field_names(Records, Type)].

formaters([]) ->
    [];
formaters(Fmtrs) when is_list(Fmtrs) ->
    Fmtrs;
formaters(Fmtrs) ->
    lists:foldl(fun(Fmtr, Acc) ->
        case erl_syntax:tuple_elements(Fmtr) of
            [Key, Fun] ->
                [{erl_syntax:atom_value(Key), Fun} | Acc];
            _ ->
                Acc
        end
    end, [], erl_syntax:list_elements(Fmtrs)).

record_field(Field0, Var) when is_atom(Field0) ->
    Field = erl_syntax:atom(Field0),
    erl_syntax:record_field(Field, Var).

record_field(Var0, Rec0, Field0) when is_atom(Field0) ->
    Field = erl_syntax:atom(Field0),
    Access = erl_syntax:record_access(Var0, Rec0, Field),
    erl_syntax:record_field(Field, Access).

f_record_field(Field, {_, Fun, Args0}) ->
    Args = [temp_var(Arg) || Arg <- Args0],
    Value = erl_syntax:application(Fun, Args),
    erl_syntax:revert(erl_syntax:record_field(Field, Value)).

f_record_field(Field, Dict, {_, Fun, Args0}) ->
    Atoms = [erl_syntax:atom_value(Arg) || Arg <- Args0],
    Args = [dict:fetch(Atom, Dict) || Atom <- Atoms],
    Value = erl_syntax:application(Fun, Args),
    erl_syntax:revert(erl_syntax:record_field(Field, Value)).

f_record_field(Var, Rec, Field, {_, Fun, Args}) ->
    Accesses = [erl_syntax:record_access(Var, Rec, Arg) || Arg <- Args],
    Value = erl_syntax:application(Fun, Accesses),
    erl_syntax:revert(erl_syntax:record_field(Field, Value)).

temp_var(Field) when is_atom(Field) ->
    erl_syntax:variable("XtTransVar____" ++ atom_to_list(Field));
temp_var(Field) ->
    erl_syntax:variable("XtTransVar____" ++ atom_to_list(erl_syntax:atom_value(Field))).

check_list_elments(ListExpr) ->
    Eles = erl_syntax:list_elements(ListExpr),
    Type = lists:umerge([[erl_syntax:type(Ele)] || Ele <- Eles]),
    case Type of
        [tuple] ->
            TuplesEles = [erl_syntax:tuple_elements(Ele) || Ele <- Eles],
            {Fmtrs, Extras, Covers} =
                lists:foldl(fun
                    ([Field, Fmtr0], {FmtrsAcc, ArgsAcc, CoversAcc}) ->
                        Fmtr = {erl_syntax:atom_value(Field), Fmtr0, [Field]},
                        {[Fmtr | FmtrsAcc], ArgsAcc, CoversAcc};
                    ([Field, Fmtr0, Args0], {FmtrsAcc, ArgsAcc, CoversAcc}) ->
                        {fields, _, Extras0} = check_list_elments(Args0),
                        FieldName = erl_syntax:atom_value(Field),
                        Fmtr = {FieldName, Fmtr0, erl_syntax:list_elements(Args0)},
                        Extras = fields_umerge(Extras0, ArgsAcc),
                        Covers = fields_umerge([FieldName], CoversAcc),
                        {[Fmtr | FmtrsAcc], Extras, Covers}
                end, {[], [], []}, TuplesEles),
            {formaters, Fmtrs, Extras, Covers -- Extras};
        [atom] ->
            Fields = [erl_syntax:atom_value(Ele) || Ele <- Eles],
            {fields, erlang:length(Fields), Fields};
        _ ->
            false
    end.

fields_umerge(Fields, Extra) ->
    lists:umerge(lists:sort(Fields), lists:sort(Extra)).
