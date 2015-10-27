
%% Copyright (c) JinGan <jg_513@163.com>

-module(xt_trans).

-ifdef(MERL_DEP).
-include_lib("merl/include/merl.hrl").
-else.
-include_lib("syntax_tools/include/merl.hrl").
-endif.

-export([parse_transform/2]).

-record(state, {
    file = "",
    records = dict:new(),
    options = []
}).

%% External API
parse_transform(Forms0, Options0) ->
    State0 = init_state(Forms0, Options0),
    {Forms, _State} = forms(Forms0, State0),
    Forms.

init_state(Forms, Options) ->
    lists:foldl(fun(Form, Acc) ->
        case Form of
            ?Q("-file(\"'@File\", 9090).") ->
                Acc#state{file = filename:absname(erl_syntax:string_value(File))};
            ?Q("-record('@Type0', {'@_@Fields0' = []}).") ->
                Type = erl_syntax:atom_value(Type0),
                Fields = [erl_syntax:atom_value(erl_syntax:record_field_name(Field))
                    || Field <- Fields0],
                Acc#state{records = dict:store(Type, Fields, Acc#state.records)};
            _ ->
                Acc
        end
    end, #state{options = Options}, Forms).

%% traverse forms
forms([Head | Tail], State0) when is_list(Head) ->
    {Forms0, State1} = forms(Head, State0),
    {Forms1, State} = forms(Tail, State1),
    {[Forms0 | Forms1], State};
forms([Form0 | Forms0], State0) ->
    {Form1, State1} = form(Form0, State0),
    {Form, State3} =
        case erl_syntax:subtrees(Form1) of
            [] ->
                {Form1, State1};
            List ->
                {Forms1, State2} = forms(List, State1),
                Tree = erl_syntax:make_tree(erl_syntax:type(Form1), Forms1),
                {erl_syntax:revert(Tree), State2}
        end,
    {Forms, State} = forms(Forms0, State3),
    {[Form | Forms], State};
forms([], State) ->
    {[], State}.

form(Form, #state{records = Records, file = File} = State) ->
    try
        transform(Records, Form, State)
    catch
        Err: Reason ->
            io:format("~s:~w ~1000p: ~1000p~n", [File, erl_syntax:get_pos(Form), Err, Reason]),
            exit(parse_error)
    end.

transform(Records, Form0, State) ->
    case Form0 of
        ?Q("record_copy(_@DArgs, _@SArgs)") ->
            Form = copy_transform(DArgs, SArgs, Form0, Records),
            {Form, State};
        ?Q("record_assign(_@DArgs, _@SArgs)") ->
            Form = assign_transform(DArgs, SArgs, Form0, Records),
            {Form, State};
        ?Q("record_get(_@DArgs, _@SArgs)") ->
            Form = get_transform(DArgs, SArgs, Form0, Records),
            {Form, State};
        _ ->
            {Form0, State}
    end.

copy_dst_args(DArgs) ->
    case DArgs of
        ?Q("{'@DRec', #'@DRec'{}}") ->
            ?Q("{'@DRec'}");
        ?Q("{'@DRec', _@DVar}") ->
            ?Q("{'@DRec', _@DVar}");
        ?Q("{#'@DRec'{}}") ->
            ?Q("{'@DRec'}");
        ?Q("{'@DRec'}") ->
            ?Q("{'@DRec'}");
        ?Q("#'@DRec'{}") ->
            ?Q("{'@DRec'}");
        ?Q("'@DRec'") ->
            ?Q("{'@DRec'}")
    end.

copy_src_args(SArgs) ->
    case SArgs of
        ?Q("{'@SRec', #'@SRec'{}}") ->
            ?Q("{'@SRec', #'@SRec'{}, []}");
        ?Q("{#'@SRec'{}}") ->
            ?Q("{'@SRec', #'@SRec'{}, []}");
        ?Q("{'@SRec'}") ->
            ?Q("{'@SRec', #'@SRec'{}, []}");
        ?Q("#'@SRec'{}") ->
            ?Q("{'@SRec', #'@SRec'{}, []}");
        ?Q("{'@SRec', [_@@SVar]}") ->
            ?Q("{'@SRec', #'@SRec'{}, [_@@SVar]}");
        ?Q("{'@SRec', _@SVar}") ->
            ?Q("{'@SRec', _@SVar, []}");
        ?Q("{'@SRec', _@SVar, _@Fmtrs}") ->
            ?Q("{'@SRec', _@SVar, _@Fmtrs}");
        ?Q("'@SRec'") ->
            ?Q("{'@SRec', #'@SRec'{}, []}")
    end.

copy_transform(DArgs0, SArgs0, _Form, Records) ->
    {DRec, DVar} =
        case copy_dst_args(DArgs0) of
            ?Q("{'@DRec0', _@DVar0}") ->
                {DRec0, DVar0};
            ?Q("{'@DRec0'}") ->
                {DRec0, undefined}
        end,
    DFields = atom_fields(Records, DRec),
    ?Q("{'@SRec', _@SVar, _@Fmtrs0}") = copy_src_args(SArgs0),
    SFields = atom_fields(Records, SRec),
    {Fmtrs, _Extras, Covers} = check_formaters(Fmtrs0),
    Fields = [copy_record_field(SVar, SRec, Field, lists:keyfind(Field, 1, Fmtrs))
        || Field <- DFields, lists:member(Field, SFields ++ Covers)],
    case DVar of
        undefined ->
            ?Q("#'@DRec'{'@_@Fields' = []}");
        _ ->
            ?Q("_@DVar#'@DRec'{'@_@Fields' = []}")
    end.

copy_record_field(Var, Rec, Field0, {_, Fun, Args}) when is_atom(Field0) ->
    Accesses = [erl_syntax:record_access(Var, Rec, Arg) || Arg <- Args],
    Value = erl_syntax:application(Fun, Accesses),
    Field = erl_syntax:atom(Field0),
    erl_syntax:record_field(Field, Value);
copy_record_field(Var, Rec, Field0, _Fmtr) when is_atom(Field0) ->
    Field = erl_syntax:atom(Field0),
    Access = erl_syntax:record_access(Var, Rec, Field),
    erl_syntax:record_field(Field, Access).

assign_dst_args(DArgs) ->
    copy_dst_args(DArgs).

assign_src_args(SArgs, Records, DFields) ->
    case SArgs of
        ?Q("{[_@@Values], [_@@Fields], [_@@Fmtrs]}") ->
            ?Q("{[_@@Values], [_@@Fields], [_@@Fmtrs]}");
        ?Q("{_@SVar, [_@@List], [_@@Fmtrs]}") ->
            case erl_syntax:type(SVar) of
                variable ->
                    ?Q("{_@SVar, [_@@List], [_@@Fmtrs]}");
                atom ->
                    SFields = atom_fields(Records, SVar),
                    ?Q("{[_@@List], _@@SFields@, [_@@Fmtrs]}")
            end;
        ?Q("{_@SRec, _@SVar, [_@@Fmtrs]}") ->
            SFields = atom_fields(Records, SRec),
            ?Q("{_@SVar, _@@SFields@, [_@@Fmtrs]}");
        ?Q("{[_@@Values], [_@@List]}") ->
            case check_list_type(List) of
                formaters ->
                    ?Q("{[_@@Values], _@@DFields@, [_@@List]}");
                fields ->
                    ?Q("{[_@@Values], [_@@List], []}")
            end;
        ?Q("{_@SVar, [_@@List]}") ->
            case erl_syntax:type(SVar) of
                variable ->
                    case check_list_type(List) of
                        formaters ->
                            ?Q("{_@SVar, _@@DFields@, [_@@List]}");
                        fields ->
                            ?Q("{_@SVar, [_@@List], []}")
                    end;
                atom ->
                    SFields = atom_fields(Records, SVar),
                    ?Q("{[_@@List], _@@SFields@, []}")
            end;
        ?Q("{_@SRec, _@SVar}") ->
            SFields = atom_fields(Records, SRec),
            ?Q("{_@SVar, _@@SFields@, []}");
        ?Q("[_@@Values]") ->
            ?Q("{[_@@Values], _@@DFields@, []}");
        ?Q("{[_@@Values]}") ->
            ?Q("{[_@@Values], _@@DFields@, []}");
        ?Q("{_@SVar}") ->
            ?Q("{_@SVar, _@@DFields@, []}");
        ?Q("_@SVar") ->
            ?Q("{_@SVar, _@@DFields@, []}")
    end.

assign_transform(DArgs0, SArgs0, _Form, Records) ->
    {DRec, DVar} =
        case assign_dst_args(DArgs0) of
            ?Q("{'@DRec0', _@DVar0}") ->
                {DRec0, DVar0};
            ?Q("{'@DRec0'}") ->
                {DRec0, undefined}
        end,
    DFields = atom_fields(Records, DRec),
    ?Q("{_@SVar, _@SFields0, _@Fmtrs0}") = assign_src_args(SArgs0, Records, DFields),
    SFields = erl_syntax:concrete(SFields0),
    {Fmtrs, Extras, Covers} = check_formaters(Fmtrs0),
    case erl_syntax:type(SVar) of
        variable ->
            assign_variable_fields(DRec, DVar, SFields, SVar, Records, Fmtrs, Extras, Covers);
        list ->
            assign_list_fields(DRec, DVar, SFields, SVar, Records, Fmtrs, Covers)
    end.

assign_variable_fields(DRec, DVar, SFields, SVals, Records, Fmtrs, Extras, Covers) ->
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
    Fields = [assign_record_field(Field, dict:fetch(Field, Dict), lists:keyfind(Field, 1, Fmtrs))
        || Field <- DFields, lists:member(Field, SFields ++ Covers)],
    Record =
        case DVar of
            undefined ->
                erl_syntax:record_expr(DRec, Fields);
            _ ->
                erl_syntax:record_expr(DVar, DRec, Fields)
        end,
    erl_syntax:block_expr([Match, Record]).

assign_record_field(Field0, _Var, {_, Fun, Args0}) ->
    Args = [temp_var(Arg) || Arg <- Args0],
    Value = erl_syntax:application(Fun, Args),
    Field = erl_syntax:atom(Field0),
    erl_syntax:record_field(Field, Value);
assign_record_field(Field0, Var, _Fmtr) when is_atom(Field0) ->
    Field = erl_syntax:atom(Field0),
    erl_syntax:record_field(Field, Var).

temp_var(Field) when is_atom(Field) ->
    erl_syntax:variable("XtTransVar____" ++ atom_to_list(Field));
temp_var(Field) ->
    erl_syntax:variable("XtTransVar____" ++ atom_to_list(erl_syntax:atom_value(Field))).

assign_list_fields(DRec, DVar, SFields, SVar, Records, Fmtrs, Covers) ->
    DFields = atom_fields(Records, DRec),
    Dict = dict:from_list(lists:zip(SFields, erl_syntax:list_elements(SVar))),
    Fields = [assign_list_field(Field, Dict, lists:keyfind(Field, 1, Fmtrs))
        || Field <- DFields, lists:member(Field, SFields ++ Covers)],
    case DVar of
        undefined ->
            erl_syntax:record_expr(DRec, Fields);
        _ ->
            erl_syntax:record_expr(DVar, DRec, Fields)
    end.

assign_list_field(Field0, Dict, {_, Fun, Args0}) ->
    Field = erl_syntax:atom(Field0),
    Atoms = [erl_syntax:atom_value(Arg) || Arg <- Args0],
    Args = [dict:fetch(Atom, Dict) || Atom <- Atoms],
    Value = erl_syntax:application(Fun, Args),
    erl_syntax:record_field(Field, Value);
assign_list_field(Field0, Dict, _Fmtr) when is_atom(Field0) ->
    Var = dict:fetch(Field0, Dict),
    Field = erl_syntax:atom(Field0),
    erl_syntax:record_field(Field, Var).

get_dst_args(DArgs) ->
    copy_dst_args(DArgs).

get_src_args(SArgs, _Records, _DFields) ->
    case SArgs of
        ?Q("{_@SVar, [_@@Fmtrs]}") ->
            ?Q("{_@SVar, [_@@Fmtrs]}");
        ?Q("{[_@@Fields]}") ->
            ?Q("{[_@@Fields], []}");
        ?Q("{_@SVar}") ->
            ?Q("{_@SVar, []}");
        ?Q("[_@@Fields]") ->
            ?Q("{[_@@Fields], []}");
        ?Q("_@SVar") ->
            ?Q("{_@SVar, []}")
    end.

get_transform(DArgs0, SArgs0, _Form, Records) ->
    {DRec, DVar} =
        case get_dst_args(DArgs0) of
            ?Q("{'@DRec0', _@DVar0}") ->
                {DRec0, DVar0};
            ?Q("{'@DRec0'}") ->
                ?Q("{'@DRec', _@DVar}") = ?Q("{'@DRec0', #'@DRec0'{}}"),
                {DRec, DVar}
        end,
    DFields = atom_fields(Records, DRec),
    ?Q("{_@SVar, _@Fmtrs0}") = get_src_args(SArgs0, Records, DFields),
    {Fmtrs, _Extras, Covers} = check_formaters(Fmtrs0),
    case erl_syntax:type(SVar) of
        variable ->
            get_field(SVar, DRec, DVar, Records, Fmtrs, Covers);
        list ->
            get_fields(DRec, DVar, SVar, Fmtrs, Covers)
    end.

get_field(DVar, SRec, SVar, Records, Fmtrs, _Covers) ->
    SFields = atom_fields(Records, SRec),
    Clauses = [
        begin
            Patterns = [erl_syntax:abstract(Field)],
            Body = record_access(SVar, SRec, Field, lists:keyfind(Field, 1, Fmtrs)),
            erl_syntax:clause(Patterns, none, [Body])
        end || Field <- SFields],
    Default = ?Q("_ -> undefined"),
    erl_syntax:case_expr(DVar, lists:reverse([Default | Clauses])).

get_fields(SRec, SVar, DVar, Fmtrs, _Covers) ->
    SFields = erl_syntax:concrete(DVar),
    List = [record_access(SVar, SRec, Field, lists:keyfind(Field, 1, Fmtrs)) || Field <- SFields],
    erl_syntax:list(List).

%% Transform Internal API
atom_fields(Records, Type) when is_atom(Type) ->
    dict:fetch(Type, Records);
atom_fields(Records, Type) ->
    dict:fetch(erl_syntax:atom_value(Type), Records).

record_access(Var, Rec, _Field, {_, Fun, Args}) ->
    Accesses = [erl_syntax:record_access(Var, Rec, Arg) || Arg <- Args],
    erl_syntax:application(Fun, Accesses);
record_access(Var, Rec, Field0, _Fmtr) when is_atom(Field0) ->
    Field = erl_syntax:atom(Field0),
    erl_syntax:record_access(Var, Rec, Field).

check_formaters(Fmtrs0) ->
    {Fmtrs, Extras, Covers} =
        lists:foldl(fun(Element, {FmtrsAcc, ArgsAcc, CoversAcc}) ->
            case Element of
                ?Q("{'@Field', _@Fmtr0}") ->
                    Fmtr = {erl_syntax:atom_value(Field), Fmtr0, [Field]},
                    {[Fmtr | FmtrsAcc], ArgsAcc, CoversAcc};
                ?Q("{'@Field', _@Fmtr0, _@Args}") ->
                    Fmtr = {erl_syntax:atom_value(Field), Fmtr0, erl_syntax:list_elements(Args)},
                    Extras = fields_umerge(erl_syntax:concrete(Args), ArgsAcc),
                    Covers = fields_umerge([erl_syntax:atom_value(Field)], CoversAcc),
                    {[Fmtr | FmtrsAcc], Extras, Covers}
            end
        end, {[], [], []}, erl_syntax:list_elements(Fmtrs0)),
    {Fmtrs, Extras, Covers -- Extras}.

check_list_type(List) ->
    Type = lists:umerge([[erl_syntax:type(E)] || E <- List]),
    case Type of
        [tuple] ->
            formaters;
        [atom] ->
            fields;
        _ ->
            false
    end.

fields_umerge(Fields, Extra) ->
    lists:umerge(lists:sort(Fields), lists:sort(Extra)).
