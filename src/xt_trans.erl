
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
    io:format("~s", [erl_prettypr:format(erl_syntax:form_list(Forms))]),
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
transform(atom, Form, Opr, Args, Records) ->
    Name = erl_syntax:atom_value(Opr),
    ArgsType = [erl_syntax:type(Arg) || Arg <- Args],
    io:format("==> ~p~n", [Form]),
    try transform(Name, ArgsType, Form, Opr, Args, Records)
    catch
        _ ->
            Form
    end;
transform(_, Form, _Opr, _Args, _Records) ->
    Form.

%% copy fields
transform(record_copy, [atom, atom, variable], _Form, _Opr, Args, Records) ->
    [DRec, SRec, SVar] = Args,
    Fields = copy_fields(DRec, SRec, SVar, Records, []),
    erl_syntax:record_expr(DRec, Fields);
transform(record_copy, [atom, variable, atom, variable], _Form, _Opr, Args, Records) ->
    [DRec, DVar0, SRec, SVar] = Args,
    Fields = copy_fields(DRec, SRec, SVar, Records, []),
    erl_syntax:record_expr(DVar0, DRec, Fields);

%% copy fields with formater
transform(record_copy, [atom, atom, variable, list], _Form, _Opr, Args, Records) ->
    [DRec, SRec, SVar, Fmtrs] = Args,
    Fields = copy_fields(DRec, SRec, SVar, Records, Fmtrs),
    erl_syntax:record_expr(DRec, Fields);
transform(record_copy, [atom, variable, atom, variable, list], _Form, _Opr, Args, Records) ->
    [DRec, DVar0, SRec, SVar, Fmtrs] = Args,
    Fields = copy_fields(DRec, SRec, SVar, Records, Fmtrs),
    erl_syntax:record_expr(DVar0, DRec, Fields);

%% assign fields
transform(record_assign, [atom, list], Form, Opr, Args, Records) ->
    [DRec, SVar] = Args,
    transform(record_assign, [atom, atom, list], Form, Opr, [DRec, DRec, SVar], Records);
transform(record_assign, [atom, atom, list], Form, _Opr, Args, Records) ->
    [DRec, SRec, SVar] = Args,
    Length = erlang:length(field_names(Records, SRec)),
    case erl_syntax:list_length(SVar) of
        Length ->
            SFields = atom_fields(Records, SRec),
            Fields = assign_list_fields(DRec, SFields, SVar, Records, []),
            erl_syntax:record_expr(DRec, Fields);
        _ ->
            Form
    end;
transform(record_assign, [atom, variable, atom, list], Form, _Opr, Args, Records) ->
    [DRec, DVar, SRec, SVar] = Args,
    Length = erlang:length(field_names(Records, SRec)),
    case erl_syntax:list_length(SVar) of
        Length ->
            SFields = atom_fields(Records, SRec),
            Fields = assign_list_fields(DRec, SFields, SVar, Records, []),
            erl_syntax:record_expr(DVar, DRec, Fields);
        _ ->
            Form
    end;
transform(record_assign, [atom, list, list], Form, _Opr, Args, Records) ->
    [DRec, SFields0, SVar] = Args,
    Length = erl_syntax:list_length(SVar),
    case check_fields_list(SFields0) of
        {true, Length, SFields, Fmtrs} ->
            Fields = assign_list_fields(DRec, SFields, SVar, Records, Fmtrs),
            erl_syntax:record_expr(DRec, Fields);
        _ ->
            io:format("====> ~p~n", [Form]),
            Form
    end;
transform(record_assign, [atom, variable, list, list], Form, _Opr, Args, Records) ->
    [DRec, DVar, SFields0, SVar] = Args,
    Length = erl_syntax:list_length(SVar),
    case check_fields_list(SFields0) of
        {true, Length, SFields, Fmtrs} ->
            Fields = assign_list_fields(DRec, SFields, SVar, Records, Fmtrs),
            erl_syntax:record_expr(DVar, DRec, Fields);
        _ ->
            Form
    end;
transform(record_assign, [atom, list, variable], Form, _Opr, Args, Records) ->
    [DRec, SFields0, SVar] = Args,
    case check_fields_list(SFields0) of
        {true, _Length, SFields, Fmtrs} ->
            {Match, Fields} = assign_variable_fields(DRec, SFields, SVar, Records, Fmtrs),
            Record = erl_syntax:record_expr(DRec, Fields),
            erl_syntax:block_expr([Match, Record]);
        _ ->
            Form
    end;
transform(record_assign, [atom, variable, list, variable], Form, _Opr, Args, Records) ->
    [DRec, DVar, SFields0, SVar] = Args,
    case check_fields_list(SFields0) of
        {true, _Length, SFields, Fmtrs} ->
            {Match, Fields} = assign_variable_fields(DRec, SFields, SVar, Records, Fmtrs),
            Record = erl_syntax:record_expr(DVar, DRec, Fields),
            erl_syntax:block_expr([Match, Record]);
        _ ->
            Form
    end;
transform(record_assign, [atom, variable], Form, Opr, Args, Records) ->
    [DRec, SVar] = Args,
    transform(record_assign, [atom, atom, variable], Form, Opr, [DRec, DRec, SVar], Records);
transform(record_assign, [atom, atom, variable], _Form, _Opr, Args, Records) ->
    [DRec, SRec, SVar] = Args,
    SFields = atom_fields(Records, SRec),
    {Match, Fields} = assign_variable_fields(DRec, SFields, SVar, Records, []),
    Record = erl_syntax:record_expr(DRec, Fields),
    erl_syntax:block_expr([Match, Record]);
transform(record_assign, [atom, variable, atom, variable], _Form, _Opr, Args, Records) ->
    [DRec, DVar, SRec, SVar] = Args,
    SFields = atom_fields(Records, SRec),
    {Match, Fields} = assign_variable_fields(DRec, SFields, SVar, Records, []),
    Record = erl_syntax:record_expr(DVar, DRec, Fields),
    erl_syntax:block_expr([Match, Record]);

%% assign fields with formaters

%% TODO
%% transform(record_assign, [atom, list, list], Opr, Forms0, Args0, Records) ->
%%     [DRec, SVar, Fmtrs] = Args0,
%%     Args = [DRec, DRec, SVar, Fmtrs],
%%     transform(record_assign, [atom, atom, list, list], Opr, Forms0, Args, Records);
transform(record_assign, [atom, atom, list, list], Form, _Opr, Args, Records) ->
    [DRec, SRec, SVar, Fmtrs] = Args,
    Length = erlang:length(field_names(Records, SRec)),
    case erl_syntax:list_length(SVar) of
        Length ->
            SFields = atom_fields(Records, SRec),
            Fields = assign_list_fields(DRec, SFields, SVar, Records, Fmtrs),
            erl_syntax:record_expr(DRec, Fields);
        _ ->
            Form
    end;
transform(record_assign, [atom, variable, atom, list, list], Form, _Opr, Args, Records) ->
    [DRec, DVar, SRec, SVar, Fmtrs] = Args,
    Length = erlang:length(field_names(Records, SRec)),
    case erl_syntax:list_length(SVar) of
        Length ->
            SFields = atom_fields(Records, SRec),
            Fields = assign_list_fields(DRec, SFields, SVar, Records, Fmtrs),
            erl_syntax:record_expr(DVar, DRec, Fields);
        _ ->
            Form
    end;
transform(record_assign, [atom, variable, list], Form, Opr, Args0, Records) ->
    [DRec, SVar, Fmtrs] = Args0,
    Args = [DRec, DRec, SVar, Fmtrs],
    transform(record_assign, [atom, atom, variable, list], Form, Opr, Args, Records);
transform(record_assign, [atom, atom, variable, list], _Form, _Opr, Args, Records) ->
    [DRec, SRec, SVar, Fmtrs] = Args,
    SFields = atom_fields(Records, SRec),
    {Match, Fields} = assign_variable_fields(DRec, SFields, SVar, Records, Fmtrs),
    Record = erl_syntax:record_expr(DRec, Fields),
    erl_syntax:block_expr([Match, Record]);
transform(record_assign, [atom, variable, atom, variable, list], _Form, _Opr, Args, Records) ->
    [DRec, DVar, SRec, SVar, Fmtrs] = Args,
    SFields = atom_fields(Records, SRec),
    {Match, Fields} = assign_variable_fields(DRec, SFields, SVar, Records, Fmtrs),
    Record = erl_syntax:record_expr(DVar, DRec, Fields),
    erl_syntax:block_expr([Match, Record]);

%% default
transform(_, _, Form, _Opr, _Args, _Records) ->
    Form.

%% Transform Internal API
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

assign_list_fields(DRec, SFields, SVar, Records, Fmtrs0) when is_list(SFields) ->
    Fmtrs = formaters(Fmtrs0),
    DFields = atom_fields(Records, DRec),
    Dict = dict:from_list(lists:zip(SFields, erl_syntax:list_elements(SVar))),
    [case lists:keyfind(Field, 1, Fmtrs) of
         false ->
             record_field(Field, dict:fetch(Field, Dict));
         Fmtr ->
             f_record_field(Field, dict:fetch(Field, Dict), Fmtr)
     end || Field <- DFields, lists:member(Field, SFields)].

assign_variable_fields(DRec, SFields, SVar, Records, Fmtrs0) ->
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
    Match = erl_syntax:match_expr(Pattern, SVar),
    {Match, [
        case lists:keyfind(Field, 1, Fmtrs) of
            false ->
                record_field(Field, dict:fetch(Field, Dict));
            Fmtr ->
                f_record_field(Field, dict:fetch(Field, Dict), Fmtr)
        end || Field <- DFields, lists:member(Field, SFields)]}.

atom_fields(Records, Type) ->
    [erl_syntax:atom_value(Field) || Field <- field_names(Records, Type)].

formaters([]) ->
    [];
formaters(Fmtrs) ->
    case erl_syntax:is_tree(Fmtrs) of
        true ->
            lists:foldl(fun(Fmtr, Acc) ->
                case erl_syntax:tuple_elements(Fmtr) of
                    [Key, Fun] ->
                        [{erl_syntax:atom_value(Key), Fun} | Acc];
                    _ ->
                        Acc
                end
            end, [], erl_syntax:list_elements(Fmtrs));
        _ ->
            Fmtrs
    end.

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
