
%% Copyright (c) JinGan <jg_513@163.com>

-module(xt_trans).

-compile(export_all).
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
%%     io:format(io_lib:format("~s", [erl_prettypr:format(erl_syntax:form_list(Forms))])),
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

form(application, Form0, #state{records = Records} = State0) ->
    Oprt = erl_syntax:application_operator(Form0),
    Args = erl_syntax:application_arguments(Form0),
    Form = transform(erl_syntax:type(Oprt), Form0, Oprt, Args, Records),
    {Form, State0};
form(_, Form0, State0) ->
    {Form0, State0}.

%% transform
transform(atom, Form, Oprt, Args, Records) ->
    case [erl_syntax:type(Arg) || Arg <- Args] of
        [tuple, tuple] ->
            Name = erl_syntax:atom_value(Oprt),
            try do_transform(Name, Args, Form, Records)
            catch
                _ ->
                    Form
            end;
        _ ->
            Form
    end;
transform(_, Form, _Oprt, _Args, _Records) ->
    Form.

do_transform(record_copy, Args, Form, Records) ->
    [DArgs0, SArgs0] = Args,
    DArgs = erl_syntax:tuple_elements(DArgs0),
    SArgs = erl_syntax:tuple_elements(SArgs0),
    case copy_args_check(DArgs, SArgs) of
        true ->
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
        {true, assign_variable} ->
            assign_variable(DArgs, SArgs, Form, Records);
        _ ->
            Form
    end;
do_transform(_Name, _Args, Form, _Records) ->
    Form.

%% copy and transform fields
copy_args_check(DArgs, SArgs) ->
    DCheck =
        case [erl_syntax:type(Arg) || Arg <- DArgs] of
            [atom] ->
                true;
            [atom, variable] ->
                true;
            [atom, tuple] ->
                true;
            _ ->
                false
        end,
    SCheck =
        case [erl_syntax:type(Arg) || Arg <- SArgs] of
            [atom, variable]->
                true;
            [atom, tuple] ->
                true;
            [atom, variable, list] ->
                true;
            [atom, tuple, list] ->
                true;
            _ ->
                false
        end,
    DCheck andalso SCheck.

copy_transform([DRec, DVar], SArgs, Form0, Records) ->
    Form = copy_transform([DRec], SArgs, Form0, Records),
    record_expr_add_arg(DVar, Form);
copy_transform([DRec], [SRec, SVar], _Form, Records) ->
    Fields = copy_fields(DRec, SRec, SVar, Records, []),
    erl_syntax:record_expr(DRec, Fields);
copy_transform([DRec], [SRec, SVar, Fmtrs], _Form, Records) ->
    Fields = copy_fields(DRec, SRec, SVar, Records, Fmtrs),
    erl_syntax:record_expr(DRec, Fields).

copy_fields(DRec, SRec, SVar, Records, Fmtrs0) ->
    DFields = atom_fields(Records, DRec),
    SFields = atom_fields(Records, SRec),
    Fmtrs = formaters(Fmtrs0),
    [case lists:keyfind(Field, 1, Fmtrs) of
         false ->
             record_field(SVar, SRec, Field);
         Fmtr ->
             f_record_field(SVar, SRec, Field, Fmtr)
     end || Field <- DFields, lists:member(Field, SFields)].

%% assign and transform fields
assign_args_check(DArgs, SArgs) ->
    DCheck =
        case [erl_syntax:type(Arg) || Arg <- DArgs] of
            [atom] ->
                true;
            [atom, variable] ->
                true;
            [atom, tuple] ->
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
            [atom, list] ->
                assign_list;
            [atom, list, list] ->
                assign_list;

            [variable] ->
                assign_variable;
            [variable, list] ->
                assign_variable;
            [atom, variable]->
                assign_variable;
            [atom, variable, list] ->
                assign_variable;
            _ ->
                false
        end,
    {DCheck, SCheck}.

assign_list([DRec, DVar], SArgs, Form0, Records) ->
    Form = assign_list([DRec], SArgs, Form0, Records),
    record_expr_add_arg(DVar, Form);
assign_list([DRec], [SVar], Form, Records) ->
    assign_list([DRec], [DRec, SVar], Form, Records);
assign_list([DRec], [SV1, SV2], Form, Records) ->
    case erl_syntax:type(SV1) of
        atom ->
            SFields = atom_fields(Records, SV1),
            assign_list_fields(DRec, SFields, SV2, Records, []);
        list ->
            Length = erl_syntax:list_length(SV1),
            case check_fields_list(SV2) of
                {true, Length, SFields, Fmtrs} ->
                    assign_list_fields(DRec, SFields, SV1, Records, Fmtrs);
                {true, _, _, Fmtrs} ->
                    SFields = atom_fields(Records, DRec),
                    assign_list_fields(DRec, SFields, SV1, Records, Fmtrs);
                false ->
                    Form
            end
    end;
assign_list([DRec], [SRec, SVals, SFmtrs], Form, Records) ->
    SFields = atom_fields(Records, SRec),
    Length = erlang:length(SFields),
    case erl_syntax:list_length(SVals) of
        Length ->
            case check_fields_list(SFmtrs) of
                {true, _Length, _Fields, Fmtrs} ->
                    assign_list_fields(DRec, SFields, SVals, Records, Fmtrs);
                _ ->
                    Form
            end;
        _ ->
            Form
    end.

assign_list_fields(DRec, SFields, SVar, Records, Fmtrs0) when is_list(SFields) ->
    Fmtrs = formaters(Fmtrs0),
    DFields = atom_fields(Records, DRec),
    Dict = dict:from_list(lists:zip(SFields, erl_syntax:list_elements(SVar))),
    Fields = [
        case lists:keyfind(Field, 1, Fmtrs) of
            false ->
                record_field(Field, dict:fetch(Field, Dict));
            Fmtr ->
                f_record_field(Field, dict:fetch(Field, Dict), Fmtr)
        end || Field <- DFields, lists:member(Field, SFields)],
    erl_syntax:record_expr(DRec, Fields).

assign_variable([DRec, DVar], SArgs, Form0, Records) ->
    Form = assign_variable([DRec], SArgs, Form0, Records),
    record_expr_add_arg(DVar, Form);
assign_variable([DRec], [SVar], Form, Records) ->
    assign_variable([DRec], [DRec, SVar], Form, Records);
assign_variable([DRec], [SV1, SV2], Form, Records) ->
    case erl_syntax:type(SV1) of
        atom ->
            SFields = atom_fields(Records, SV1),
            assign_variable_fields(DRec, SFields, SV2, Records, []);
        variable ->
            Length = erlang:length(atom_fields(Records, DRec)),
            case check_fields_list(SV2) of
                {true, Length, SFields, Fmtrs} ->
                    assign_variable_fields(DRec, SFields, SV1, Records, Fmtrs);
                {true, _Length, _SFields, Fmtrs} ->
                    SFields = atom_fields(Records, DRec),
                    assign_variable_fields(DRec, SFields, SV1, Records, Fmtrs);
                false ->
                    Form
            end
    end;
assign_variable([DRec], [SRec, SVals, SFmtrs], Form, Records) ->
    SFields = atom_fields(Records, SRec),
    case check_fields_list(SFmtrs) of
        {true, _Length, _Fields, Fmtrs} ->
            assign_variable_fields(DRec, SFields, SVals, Records, Fmtrs);
        false ->
            Form
    end.

assign_variable_fields(DRec, SFields, SVals, Records, Fmtrs0) ->
    Fmtrs = formaters(Fmtrs0),
    DFields = atom_fields(Records, DRec),
    MatchList = [
        begin
            case lists:member(Field, DFields) of
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
                f_record_field(Field, dict:fetch(Field, Dict), Fmtr)
        end || Field <- DFields, lists:member(Field, SFields)],
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

f_record_field(Field0, Var, {_, Fun}) when is_atom(Field0) ->
    Field = erl_syntax:atom(Field0),
    Value = erl_syntax:application(Fun, [Var]),
    erl_syntax:revert(erl_syntax:record_field(Field, Value)).

f_record_field(Var0, Rec0, Field0, {_, Fun}) when is_atom(Field0) ->
    Field = erl_syntax:atom(Field0),
    Access = erl_syntax:record_access(Var0, Rec0, Field),
    Value = erl_syntax:application(Fun, [Access]),
    erl_syntax:revert(erl_syntax:record_field(Field, Value)).

temp_var(Field) when is_atom(Field) ->
    erl_syntax:variable("XtTransTempVar____" ++ atom_to_list(Field)).

check_fields_list(Fields0) ->
    Result =
        try
            lists:foldr(fun(Field0, {FieldsAcc, FmtrsAcc}) ->
                case erl_syntax:type(Field0) of
                    atom ->
                        {[erl_syntax:atom_value(Field0) | FieldsAcc], FmtrsAcc};
                    tuple ->
                        case erl_syntax:tuple_elements(Field0) of
                            [Field, Fmtr] ->
                                FieldName = erl_syntax:atom_value(Field),
                                {[FieldName | FieldsAcc], [{FieldName, Fmtr} | FmtrsAcc]};
                            _ ->
                                throw(false)
                        end;
                    _ ->
                        throw(false)
                end
            end, {[], []}, erl_syntax:list_elements(Fields0))
        catch
            _ ->
                false
        end,
    case Result of
        false ->
            false;
        {Fields, Fmtrs} ->
            {true, erlang:length(Fields), Fields, Fmtrs}
    end.
