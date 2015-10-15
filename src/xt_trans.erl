
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

%% Forms traverse
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
    Form = transform(Form0, Opr, Args, Records),
    {Form, State0};
form(_, Form0, State0) ->
    {Form0, State0}.

field_names(Records, Type) ->
    Key = erl_syntax:atom_value(Type),
    dict:fetch(Key, Records).

%% transform
transform(Form0, Opr, Args, Records) ->
    Name = erl_syntax:atom_value(Opr),
    ArgsType = [erl_syntax:type(Arg) || Arg <- Args],
    transform(Name, ArgsType, Form0, Opr, Args, Records).

transform(record_copy, [atom, variable, atom, variable], _, _, Args, Records) ->
    [DRec0, DVar0, SRec0, SVar0] = Args,
    DFields = [erl_syntax:atom_value(Field) || Field <- field_names(Records, DRec0)],
    SFields = [erl_syntax:atom_value(Field) || Field <- field_names(Records, SRec0)],
    Fields = [
        begin
            Field = erl_syntax:atom(Field0),
            Access = erl_syntax:record_access(SVar0, SRec0, Field),
            erl_syntax:record_field(Field, Access)
        end || Field0 <- DFields, lists:member(Field0, SFields)],
    io:format("~p~n", [Fields]),
    erl_syntax:record_expr(DVar0, DRec0, Fields);
transform(_, _, Form0, _Operator, _Arguments, _Records) ->
    Form0.
