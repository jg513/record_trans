
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

form(application, Form0, #state{records = Records} = State0) ->
    Opr = erl_syntax:application_operator(Form0),
    Args = erl_syntax:application_arguments(Form0),
    Form = transform(erl_syntax:type(Opr), Form0, Opr, Args, Records),
    {Form, State0};
form(_, Form0, State0) ->
    {Form0, State0}.

field_names(Records, Type) ->
    Key = erl_syntax:atom_value(Type),
    dict:fetch(Key, Records).

%% transform
transform(atom, Form0, Opr, Args, Records) ->
    Name = erl_syntax:atom_value(Opr),
    ArgsType = [erl_syntax:type(Arg) || Arg <- Args],
    transform(Name, ArgsType, Form0, Opr, Args, Records);
transform(_, Form0, _Opr, _Args, _Records) ->
    Form0.

%% copy fields
transform(record_copy, [atom, atom, variable], _, _, Args, Records) ->
    [DRec0, SRec0, SVar0] = Args,
    Fields = record_copy_fields(DRec0, SRec0, SVar0, Records),
    erl_syntax:record_expr(DRec0, Fields);
transform(record_copy, [atom, variable, atom, variable], _, _, Args, Records) ->
    [DRec0, DVar0, SRec0, SVar0] = Args,
    Fields = record_copy_fields(DRec0, SRec0, SVar0, Records),
    erl_syntax:record_expr(DVar0, DRec0, Fields);

%% copy fields with formater
transform(record_copy, [atom, atom, variable, list], _, _, Args, Records) ->
    [DRec0, SRec0, SVar0, Fmtrs0] = Args,
    Fields = f_record_copy_fields(DRec0, SRec0, SVar0, Records, Fmtrs0),
    erl_syntax:record_expr(DRec0, Fields);
transform(record_copy, [atom, variable, atom, variable, list], _, _, Args, Records) ->
    [DRec0, DVar0, SRec0, SVar0, Fmtrs0] = Args,
    Fields = f_record_copy_fields(DRec0, SRec0, SVar0, Records, Fmtrs0),
    erl_syntax:record_expr(DVar0, DRec0, Fields);

%% assign fields
transform(record_assign, [atom, list], Opr, Forms0, Args, Records) ->
    [DRec, SVar] = Args,
    transform(record_assign, [atom, atom, list], Opr, Forms0, [DRec, DRec, SVar], Records);
transform(record_assign, [atom, atom, list], _, Forms0, Args, Records) ->
    [DRec0, SRec0, SVar0] = Args,
    Length = erlang:length(field_names(Records, SRec0)),
    case erl_syntax:list_length(SVar0) of
        Length ->
            SFields = atom_fields(Records, SRec0),
            Fields = record_assign_list_fields(DRec0, SFields, SVar0, Records),
            erl_syntax:record_expr(DRec0, Fields);
        _ ->
            Forms0
    end;
transform(record_assign, [atom, variable, atom, list], _, Forms0, Args, Records) ->
    [DRec0, DVar0, SRec0, SVar0] = Args,
    Length = erlang:length(field_names(Records, SRec0)),
    case erl_syntax:list_length(SVar0) of
        Length ->
            SFields = atom_fields(Records, SRec0),
            Fields = record_assign_list_fields(DRec0, SFields, SVar0, Records),
            erl_syntax:record_expr(DVar0, DRec0, Fields);
        _ ->
            Forms0
    end;
transform(record_assign, [atom, list, list], _, Forms0, Args, Records) ->
    [DRec0, ListNode, SVar0] = Args,
    Length = erl_syntax:list_length(SVar0),
    case check_fields_list(ListNode) of
        {true, Length, SFields} ->
            Fields = record_assign_list_fields(DRec0, SFields, SVar0, Records),
            erl_syntax:record_expr(DRec0, Fields);
        _ ->
            Forms0
    end;
transform(record_assign, [atom, variable, list, list], _, Forms0, Args, Records) ->
    [DRec0, DVar0, ListNode, SVar0] = Args,
    Length = erl_syntax:list_length(SVar0),
    case check_fields_list(ListNode) of
        {true, Length, SFields} ->
            Fields = record_assign_list_fields(DRec0, SFields, SVar0, Records),
            erl_syntax:record_expr(DVar0, DRec0, Fields);
        _ ->
            Forms0
    end;
transform(record_assign, [atom, list, variable], _, Forms0, Args, Records) ->
    [DRec0, ListNode, SVar0] = Args,
    case check_fields_list(ListNode) of
        {true, _Length, SFields} ->
            {Match, Fields} = record_assign_variable_fields(DRec0, SFields, SVar0, Records),
            Record = erl_syntax:record_expr(DRec0, Fields),
            erl_syntax:block_expr([Match, Record]);
        _ ->
            Forms0
    end;
transform(record_assign, [atom, variable, list, variable], _, Forms0, Args, Records) ->
    [DRec0, DVar0, ListNode, SVar0] = Args,
    case check_fields_list(ListNode) of
        {true, _Length, SFields} ->
            {Match, Fields} = record_assign_variable_fields(DRec0, SFields, SVar0, Records),
            Record = erl_syntax:record_expr(DVar0, DRec0, Fields),
            erl_syntax:block_expr([Match, Record]);
        _ ->
            Forms0
    end;
transform(record_assign, [atom, variable], Opr, Forms0, Args, Records) ->
    [DRec, SVar] = Args,
    transform(record_assign, [atom, atom, variable], Opr, Forms0, [DRec, DRec, SVar], Records);
transform(record_assign, [atom, atom, variable], _, _, Args, Records) ->
    [DRec0, SRec0, SVar0] = Args,
    SFields = atom_fields(Records, SRec0),
    {Match, Fields} = record_assign_variable_fields(DRec0, SFields, SVar0, Records),
    Record = erl_syntax:record_expr(DRec0, Fields),
    erl_syntax:block_expr([Match, Record]);
transform(record_assign, [atom, variable, atom, variable], _, _, Args, Records) ->
    [DRec0, DVar0, SRec0, SVar0] = Args,
    SFields = atom_fields(Records, SRec0),
    {Match, Fields} = record_assign_variable_fields(DRec0, SFields, SVar0, Records),
    Record = erl_syntax:record_expr(DVar0, DRec0, Fields),
    erl_syntax:block_expr([Match, Record]);
transform(_, _, Form0, _Opr, _Args, _Records) ->
    Form0.

%% Transform Internal API
record_copy_fields(DRec0, SRec0, SVar0, Records) ->
    DFields = atom_fields(Records, DRec0),
    SFields = atom_fields(Records, SRec0),
    [record_field(SVar0, SRec0, Field0) || Field0 <- DFields, lists:member(Field0, SFields)].

f_record_copy_fields(DRec, SRec, SVar, Records, Fmtrs0) ->
    DFields = atom_fields(Records, DRec),
    SFields = atom_fields(Records, SRec),
    Fmtrs = formaters(Fmtrs0),
    [case lists:keyfind(Field, 1, Fmtrs) of
         false ->
             record_field(SVar, SRec, Field);
         Fmtr ->
             record_field(SVar, SRec, Field, Fmtr)
     end || Field <- DFields, lists:member(Field, SFields)].

record_assign_list_fields(DRec0, SFields, SVar0, Records) when is_list(SFields) ->
    DFields = atom_fields(Records, DRec0),
    Dict = dict:from_list(lists:zip(SFields, erl_syntax:list_elements(SVar0))),
    [record_field(Field0, dict:fetch(Field0, Dict))
        || Field0 <- DFields, lists:member(Field0, SFields)].

record_assign_variable_fields(DRec0, SFields, SVar0, Records) ->
    DFields = atom_fields(Records, DRec0),
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
    Match = erl_syntax:match_expr(Pattern, SVar0),
    {Match, [record_field(Field0, dict:fetch(Field0, Dict))
        || Field0 <- DFields, lists:member(Field0, SFields)]}.

atom_fields(Records, Type) ->
    [erl_syntax:atom_value(Field) || Field <- field_names(Records, Type)].

formaters(Fmtrs) ->
    lists:foldl(fun(Fmtr, Acc) ->
        case erl_syntax:tuple_elements(Fmtr) of
            [Key, Fun] ->
                [{erl_syntax:concrete(Key), Fun} | Acc];
            [Key, Mod, Fun] ->
                [{erl_syntax:concrete(Key), Mod, Fun} | Acc];
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

record_field(Var0, Rec0, Field0, Fmtr) when is_atom(Field0) ->
    Field = erl_syntax:atom(Field0),
    Access = erl_syntax:record_access(Var0, Rec0, Field),
    Value =
        case Fmtr of
            {_, Mod, Fun} ->
                erl_syntax:application(Mod, Fun, [Access]);
            {_, Fun} ->
                erl_syntax:application(Fun, [Access])
        end,
    erl_syntax:revert(erl_syntax:record_field(Field, Value)).

temp_var(Field) when is_atom(Field) ->
    erl_syntax:variable("XtTransTempVar____" ++ atom_to_list(Field)).

check_fields_list(ListTree) ->
    Fields = erl_syntax:concrete(ListTree),
    Bool =
        lists:all(fun(Field) ->
            is_atom(Field)
        end, Fields),
    Length = erlang:length(Fields),
    {Bool, Length, Fields}.
