%%%-------------------------------------------------------------------
%%% @author Nero.li
%%% @copyright (C) 2021, <Lilith>
%%% @doc
%%%     生成sheet_xxx.erl文件，yacc解析的结果
%%% @end
%%% Created : 02. 三月 2021 15:05
%%%-------------------------------------------------------------------
-module(sheets_generate).
-author("lifan").

%%% API
-export([generate/1, generate/2, get_out_files/2]).

-include("sheets_common_def.hrl").

-define(GENERATOR, ?MODULE).
-define(VERSION, "v0.2").

%%% Record
-record(sheet, {
  metas = [],
  record
}).

-record(record, {
  name,
  fields = []
}).

-record(field, {
  name,
  default,
  metas = []
}).

-record(meta, {
  comment,
  method
}).

-record(meta_method, {
  main,
  modifiers = []
}).

-record(method_main, {
  name,
  params = [],
  logic
}).

-record(method_modifier, {
  name,
  params = [],
  logic
}).

-record(method_logic, {
  content
}).


%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc generate sheet files
%% @spec generate(Sheets) -> [SheetName]
%%       generate(Sheets, Options) -> ok
%%  When Sheets = [#sheet{}], Options = [Option]
%%    Option = {include, IncludeFiles} | {custom, CustomCodes} | {generator, atom()} | {version, string()}
%%    CustomCodes = [{Tag, Code}], Code = string()
%%    SheetName = atom()
%%--------------------------------------------------------------------
generate(Sheets) ->
  generate(Sheets, []).

generate(Sheets, Options) ->
  [generate_one(Sheet, Options) || Sheet <- Sheets].

%%--------------------------------------------------------------------
%% @doc get_out_files sheet files
%% @spec get_out_files(Sheets) -> [FileName]
%%       get_out_files(Sheets, Options) -> ok
%%  When Sheets = [#sheet{}], Options = [Option]
%%    Option = {include, IncludeFiles} | {custom, CustomCodes} | {generator, atom()} | {version, string()}
%%    CustomCodes = [{Tag, Code}], Code = string()
%%    SheetName = atom()
%%--------------------------------------------------------------------
get_out_files(Sheets, Options) ->
  [get_out_filename(Sheet, Options) || #sheet{} = Sheet <- Sheets].

get_out_filename(#sheet{record = #record{name = Name}}, Options) ->
  OutDir = option(Options, out_dir, "."),
  NameStr = atom_to_list(Name),
  Prefix = option(Options, module_name_prefix, ""),
  Suffix = option(Options, module_name_suffix, ""),
  filename:join(OutDir, lists:concat([Prefix, NameStr, Suffix, ".erl"])).

%%%===================================================================
%%% Implements
%%%===================================================================
generate_one(Sheet, Options) ->
  try
    generate_one2(Sheet, Options)
  catch
    ?CATCH_RETURN(Error) -> {error, Error}
  end.

%% generate one sheet file
generate_one2(#sheet{metas = Metas, record = Record} = Sheet, Options0) ->
  Generator = option(Options0, generator, ?GENERATOR),
  Version = option(Options0, version, ?VERSION),
  SheetMd5 = md5str(term_to_binary({Generator, Version, Options0, Sheet})),
  #record{name = Name} = Record,
  NameStr = atom_to_list(Name),
%%  io:format("generate_one2 ~p~n", [NameStr]),
  OutputFileName = get_out_filename(Sheet, Options0),
  filelib:ensure_dir(OutputFileName),
  IsClean = option(Options0, clean, false),
  Options = ?VAL_IF(IsClean, Options0, update_options_from_file(Options0, OutputFileName)),
  SheetMd5Old = option(Options, "SheetMd5", ?UNDEF),
  IsForce = option(Options, force, false),
  ?RETURN_IF(not IsForce andalso SheetMd5 =:= SheetMd5Old, {sheet_md5_same, Name}),

  {ok, Fd} = file:open(OutputFileName, [write, binary]),
  % Head
  write_head_start(Fd),
  write_head(Fd, "SheetName", [get_uf8_binary(get_meta_method_param(sheet, Metas))]),
  write_head(Fd, "AutoGenerated", [Generator]),
  write_head(Fd, "Version", [Version]),
  write_head(Fd, "SheetMd5", [SheetMd5]),
  write_head(Fd, "UpdateTime", [timestamp_to_string(timenow())]),
  write_head_end(Fd),

  % Module
  write_module_start(Fd, "Module"),
  write_code(Fd, "-module(~s).", [Name]),
  write_code(Fd, "-author(\"~s\").", [Generator]),
  write_module_end(Fd),

  % Mate
  write_section_start(Fd, "Mate"),
  write_metas(Fd, Metas),
  write_section_end(Fd),
  
  % API
  write_section_start(Fd, "API"),
  write_code(Fd, "-export([init/0, reload/0, file/0, headers/0, record/0, add_record/2]).", []),
  write_code(Fd, "-export([has/1, get/1, find/1, rfind/1, ets/0, list/0])."),
  case option(Options, export, []) of
    [] -> pass;
    Exports ->
      write_code(Fd, "-export([~s]).", [string:join(Exports, ", ")])
  end,
  write_section_end(Fd),

  % Include
  write_section_start(Fd, "Include"),
  write_multi_code(Fd, "-include(\"~s\").", option(Options, include, [])),
  write_section_end(Fd),

  % Define
  write_section_start(Fd, "Define"),
  write_code(Fd, "-define(SHEET_FILE, \"~s\").", [get_meta_method_param(file, Metas)]),
  write_code(Fd, "-define(SHEET_HEADERS, [~s]).", [get_field_headers(Sheet)]),
  write_code(Fd, "-define(SHEET_RECORD, #~s{}).", [Name]),
  write_code(Fd, "-define(SHEET_RECORD_KEY, #~s.~s).", [Name, get_sheet_key_name(Sheet)]),
  write_section_end(Fd),


  % API sheet edit
  write_section_start(Fd, "Edit"),

  % init()
  write_code_api_start(Fd, "init ets", "init() -> ok | error()"),
  write_code(Fd, "init() ->"),
  write_code(Fd, "  ?new_set_ets(?MODULE, ?SHEET_RECORD_KEY),"),
  write_code(Fd, "  reload()."),
  write_code_api_end(Fd),

  % reload()
  write_code_api_start(Fd, "reload ets", "reload() -> ok | error()"),
  write_code(Fd, "reload() -> ?reload_sheet(?MODULE)."),
  write_code_api_end(Fd),

  % file()
  write_code_api_start(Fd, "file of sheet", "file() -> string()"),
  write_code(Fd, "file() -> ?SHEET_FILE."),
  write_code_api_end(Fd),

  % headers()
  write_code_api_start(Fd, "headers of sheet", "headers() -> [string()]"),
  write_code(Fd, "headers() -> ?SHEET_HEADERS."),
  write_code_api_end(Fd),

  % record()
  write_code_api_start(Fd, "record of sheet", "record() -> #"++NameStr++"{}"),
  write_code(Fd, "record() -> ?SHEET_RECORD."),
  write_code_api_end(Fd),

  % add_record(Ets, Fields)
  write_code_api_start(Fd, "add record to ets", ["add_record(Ets, Fields) -> ok | error()", "  Ets = atom(),", " Fields = [term()]"]),
  write_code(Fd, "add_record(Ets, Fields) ->"),
  write_code(Fd, "  Record = fill_record(record(), Fields),"),
  write_code(Fd, "  ets:insert(Ets, Record)."),
  write_code_api_end(Fd),

  write_section_end(Fd),

  % API sheet query
  write_section_start(Fd, "Query"),

  % has(Key)
  write_code_api_start(Fd, "has key in ets", "has(Key) -> true | false"),
  write_code(Fd, "has(Key) -> ?has_record(?MODULE, Key)."),
  write_code_api_end(Fd),

  % get(Key)
  write_code_api_start(Fd, "get record in ets", "get(Key) -> #"++NameStr++"{} | error()"),
  write_code(Fd, "get(Key) -> ?get_record(?MODULE, Key)."),
  write_code_api_end(Fd),

  % find(Fun)
  write_code_api_start(Fd, "find record in ets, start from left", ["find(Fun) -> {true, Ret} | false", "Fun = fun(#"++NameStr++"{} = Record) -> Ret end", "Ret = term()"]),
  write_code(Fd, "find(Fun) -> ?find_record(?MODULE, Fun)."),
  write_code_api_end(Fd),

  % find_r(Fun)
  write_code_api_start(Fd, "find record in ets, start from right", ["find(Fun) -> {true, Ret} | false", "Fun = fun(#"++NameStr++"{} = Record) -> Ret end", "Ret = term()"]),
  write_code(Fd, "rfind(Fun) -> ?rfind_record(?MODULE, Fun)."),
  write_code_api_end(Fd),

  % ets()
  write_code_api_start(Fd, "return ets name", "ets() -> atom()"),
  write_code(Fd, "ets() -> ?sheet_ets(?MODULE)."),
  write_code_api_end(Fd),

  % list()
  write_code_api_start(Fd, "return ets name", "list() -> [Record]"),
  write_code(Fd, "list() -> ets:tab2list(ets())."),
  write_code_api_end(Fd),

  write_section_end(Fd),

  % Header implements
  write_section_start(Fd, "Header"),
  % h(Name)
  HeaderCodes = get_header_implements(Sheet),
  case HeaderCodes of
    [] -> pass;
    _ ->
      write_code_api_start(Fd, "get header define", ["h(Name) -> Header", "Name = atom()", "Header = {function(), Arg} | {value, Value}", "Arg = Value = term()"]),
      write_header_codes(Fd, HeaderCodes),
      write_code_api_end(Fd),
      write_header_implements(Fd, HeaderCodes, Sheet)
  end,
  write_section_end(Fd),

  % Field implements
  write_section_start(Fd, "Field"),
  % fill_record(Record, Columns)
  FieldCodes = get_field_codes(Sheet),
  case has_override_api("fill_record/2", Options) of
    true -> pass;
    false ->
      write_code_api_start(Fd, "setup fields of record", ["fill_record(Record, Columns) -> NewRecord", "Record = NewRecord = #"++NameStr++"{}", "Columns = [term()]"]),
      write_code(Fd, "fill_record(Record, [~s]) ->", [string:join(get_field_columns(FieldCodes), ", ")]),
      KeyField = get_key_field(Sheet),
      case KeyField of
        ?UNDEF -> write_code(Fd, "  Record#~s{", [NameStr]);
        _ -> write_code(Fd, "  Record1 = Record#~s{", [NameStr])
      end,
      write_fill_record_fields(Fd, FieldCodes, Sheet),
      case KeyField of
        ?UNDEF -> write_code(Fd, "  }.");
        _ ->
          write_code(Fd, "  },"),
          write_code(Fd, "  Record2 = Record1#~s{", [NameStr]),
          write_fill_record_key_field(Fd, KeyField, format("Record1#~s", [NameStr])),
          write_code(Fd, "  },"),
          write_code(Fd, "  Record2.")
      end
  end,
  write_code_api_end(Fd),
  FieldCodes2 = [_FieldCode || {_Name, _Code} = _FieldCode <- FieldCodes, format("F_~w", [_Name]) =/= _Code],
  % f(Name, ParsedValue)
  case FieldCodes2 of
    [] -> pass;
    _ ->
      write_code_api_start(Fd, "get field value", ["f(Name, ParsedValue) -> Value", "Name = atom()", "ParsedValue = Value = term()"]),
      write_field_codes(Fd, FieldCodes2),
      write_code_api_end(Fd)
  end,
  write_section_end(Fd),

  {CustomOptions, CustomCodes} = option(Options, "Custom", {[], []}),
  write_section_start(Fd, "Custom", CustomOptions),
  write_section_end(Fd, CustomCodes),

  file:close(Fd),

  {ok, OutputFileName}.


%% local
update_options_from_file(Options, FileName) ->
  case filelib:is_file(FileName) of
    false -> Options;
    true -> sheets_module_parser:file(FileName, Options)
  end.

%option(Options, Name) -> option(Options, Name, ?UNDEF).
option(Options, Name, Default) ->
  proplists:get_value(Name, Options, Default).

has_override_api(ApiName, Options) ->
  lists:member(ApiName, option(Options, override, [])).

timenow() ->
  {A, B, _} = os:timestamp(),
  A * 1000 * 1000 + B.

timestamp_to_string(TimeStamp) ->
  {{Y, M, D}, {H, MM, S}} = timestamp_to_datetime(TimeStamp),
  lists:concat([Y, '-', M, '-', D, ' ', H, ':', MM, ':', S]).

timestamp_to_datetime(TimeStamp) ->
  MegaS = TimeStamp div (1000*1000),
  S = TimeStamp rem (1000*1000),
  calendar:now_to_local_time({MegaS, S, 0}).



md5str(S) ->
  Md5_bin = erlang:md5(S),
  Md5_list = binary_to_list(Md5_bin),
  lists:flatten(list_to_hex(Md5_list)).

list_to_hex(L) ->
  lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 -> $0 + N;
hex(N) when N >= 10, N < 16 -> $a + (N-10).

%% write functions
-define(STR_HEAD_LINE, "%%%-------------------------------------------------------------------").
-define(STR_SECTION_LINE, "%%%===================================================================").
-define(STR_API_LINE, "%%--------------------------------------------------------------------").

write_line(Fd) ->
  io:format(Fd, "~n", []).

write_line(Fd, Line) ->
  io:format(Fd, "~s~n", [Line]).

write_head_start(Fd) ->
  write_line(Fd, ?STR_HEAD_LINE),
  ok.

write_head(Fd, Key, Values) ->
  io:format(Fd, "%%% @~s ~s", [Key | Values]),
  write_line(Fd).

write_head_end(Fd) ->
  write_line(Fd, ?STR_HEAD_LINE).


write_module_start(_Fd, _Name) -> ok.

write_module_end(Fd) ->
  write_line(Fd).

write_section_start(Fd, TagName) ->
  write_section_start(Fd, TagName, []).

write_section_start(Fd, TagName, SectionOptions) ->
  write_line(Fd, ?STR_SECTION_LINE),
  write_code(Fd, "%%% ~s", [TagName]),
  lists:foreach(fun(Line) -> write_code(Fd, "~s", [Line], 0) end, SectionOptions),
  write_line(Fd, ?STR_SECTION_LINE).


write_section_end(Fd) ->
  write_section_end(Fd, []).

write_section_end(Fd, Codes) ->
  lists:foreach(fun(Line) -> write_code(Fd, "~s", [Line], 0) end, Codes),
  write_line(Fd).


write_metas(Fd, _Mates) ->
  write_line(Fd).

write_code(Fd, Format) ->
  write_code(Fd, Format, []).

write_code(Fd, Format, Params) ->
  write_code(Fd, Format, Params, 1).

write_code(Fd, Format, Params, Lines) ->
  io:format(Fd, Format, Params),
  lists:foreach(fun(_) -> write_line(Fd) end, lists:seq(1, Lines)).

write_multi_code(Fd, Format, ParamsList) ->
  lists:foreach(
    fun(Params) -> write_code(Fd, Format, [Params]) end,
    ParamsList
  ).

write_code_api_start(Fd, Doc, Specs) ->
  write_line(Fd, ?STR_API_LINE),
  write_code(Fd, "%% @doc ~s", [Doc]),
  case Specs of
    [] -> pass;
    [Spec | Rest] when is_list(Spec) ->
      write_code(Fd, "%% @spec ~s", [Spec]),
      lists:foreach(
        fun(Line) ->
          write_code(Fd, "%%   ~s", [Line])
        end,
        Rest
      );
    [_|_] -> write_code(Fd, "%% @spec ~s", [Specs])
  end,
  write_line(Fd, ?STR_API_LINE).

write_code_api_end(Fd) ->
  write_line(Fd).

write_header_codes(Fd, [{Name, Code}]) ->
  write_code(Fd, "h(~s) ->", [Name]),
  write_code(Fd, "  ~s.", [Code]);

write_header_codes(Fd, [{Name, Code}|Rest]) ->
  write_code(Fd, "h(~s) ->", [Name]),
  write_code(Fd, "  ~s;", [Code]),
  write_header_codes(Fd, Rest).


%% 属性
-define(HEADER_FUNC_NAMES, [map, list, prop, group]).

write_header_implements(Fd, HeaderCodes, Sheet) ->
  write_header_implements2(Fd, ?HEADER_FUNC_NAMES, HeaderCodes, Sheet),
  ok.

write_header_implements2(_Fd, [], _, _) -> ok;
write_header_implements2(Fd, [FuncName | NameRest], HeaderCodes, Sheet) ->
  case has_header_implement(FuncName, HeaderCodes) of
    false ->
      write_header_implements2(Fd, NameRest, HeaderCodes, Sheet);
    true ->
      write_header_impl_func(Fd, FuncName, Sheet),
      write_header_implements2(Fd, NameRest, HeaderCodes, Sheet)
  end.

has_header_implement(FuncName, HeaderCodes) ->
  Name = lists:concat(["h_", FuncName]),
  lists:any(fun({_, Str}) -> string:str(Str, Name) > 0 end, HeaderCodes).

write_header_impl_func(Fd, FuncName, Sheet) when FuncName =:= map ->
  write_code_api_start(Fd, format("h_~s field", [FuncName]),
    [
      format("h_~s(Name, Cols, Values) -> [{Key, Value}]", [FuncName]),
      "Name = atom(), Key = string(), Cols = [string()]",
      "Values = [Value]", "Value = term()"
    ]),
  Fields = get_fields_by_func(FuncName, Sheet),
  write_header_impl_func_fields(Fd, FuncName, Fields, Sheet),
  write_code_api_end(Fd);

write_header_impl_func(Fd, FuncName, Sheet) when FuncName =:= prop ->
  write_code_api_start(Fd, format("h_~s field", [FuncName]),
    [
      format("h_~s(Name, Cols, Values) -> [tuple()]", [FuncName]),
      "Name = atom(), Key = term(), Cols = [string()]",
      "Values = [Value]", "Value = term()"
    ]),
  Fields = get_fields_by_func(FuncName, Sheet),
  write_header_impl_func_fields(Fd, FuncName, Fields, Sheet),
  write_code_api_end(Fd);

write_header_impl_func(Fd, FuncName, Sheet) when FuncName =:= list  ->
  write_code_api_start(Fd, format("h_~s field", [FuncName]),
    [
      format("h_~s(Name, Cols, Values) -> [Value]", [FuncName]),
      "Name = atom(), Cols = [string()]",
      "Values = [Value]", "Value = term()"
    ]),
  Fields = get_fields_by_func(FuncName, Sheet),
  write_header_impl_func_fields(Fd, FuncName, Fields, Sheet),
  write_code_api_end(Fd);

write_header_impl_func(Fd, FuncName, Sheet) when FuncName =:= group  ->
  write_code_api_start(Fd, format("h_~s field", [FuncName]),
    [
      format("h_~s(Name, Cols, Values) -> Value", [FuncName]),
      "Name = atom(), Cols = [string()]",
      "Value = tuple()"
    ]),
  Fields = get_fields_by_func(FuncName, Sheet),
  write_header_impl_func_fields(Fd, FuncName, Fields, Sheet),
  write_code_api_end(Fd);

write_header_impl_func(Fd, FuncName, Sheet) when FuncName =:= check; FuncName =:= enum ->
  write_code_api_start(Fd, format("h_~s field", [FuncName]),
    [
      format("h_~s(Name, Cols, Values) -> Value", [FuncName]),
      "Name = atom(), Cols = [string()]",
      "Values = [Value]", "Value = term()"
    ]),
  Fields = get_fields_by_func(FuncName, Sheet),
  write_header_impl_func_fields(Fd, FuncName, Fields, Sheet),
  write_code_api_end(Fd).


get_fields_by_func(FuncName, #sheet{record = #record{fields = Fields}}) ->
  [Field || #field{metas = Mates} = Field <- Fields, has_meta_method(FuncName, Mates)].

write_header_impl_func_fields(Fd, FuncName, [Field], Sheet) ->
  write_header_impl_func_field(Fd, FuncName, Field, Sheet, ".");

write_header_impl_func_fields(Fd, FuncName, [Field | Rest], Sheet) ->
  write_header_impl_func_field(Fd, FuncName, Field, Sheet, ";"),
  write_header_impl_func_fields(Fd, FuncName, Rest, Sheet).


write_header_impl_func_field(Fd, FuncName, #field{name = Name, metas = Mates}, _Sheet, End) when FuncName =:= prop; FuncName =:= group ->
  #meta_method{main = #method_main{params = Params}} = get_meta_method(FuncName, Mates),
  ListStr = format("[~s]", [string:join(Params, ", ")]),
  write_code(Fd, "h_~s(~w, Cols, Values) -> ?h_~s(~w, ~s, Cols, Values)~s", [FuncName, Name, FuncName, Name, ListStr, End]);

write_header_impl_func_field(Fd, FuncName, #field{name = Name, metas = Mates}, _Sheet, End) when FuncName =:= map; FuncName =:= list ->
  #meta_method{main = #method_main{params = [Param]}} = get_meta_method(FuncName, Mates),
  write_code(Fd, "h_~s(~w, Cols, Values) -> ?h_~s(~w, ~s, Cols, Values)~s", [FuncName, Name, FuncName, Name, Param, End]);

write_header_impl_func_field(Fd, FuncName, #field{name = Name, metas = Mates}, Sheet, End) when FuncName =:= check ->
  #meta_method{main = #method_main{params = Params, logic = Logic}} = get_meta_method(FuncName, Mates),
  Func = get_method_func_code(Name, Params, Logic, Sheet),
  write_code(Fd, "h_~s(~w, Cols, Values) ->", [FuncName, Name]),
  write_code(Fd, "  Fun = ~s,", [Func]),
  write_code(Fd, "  ?h_~s(~w, Fun, Cols, Values)~s", [FuncName, Name, End]);

write_header_impl_func_field(Fd, FuncName, #field{name = Name, metas = Mates}, Sheet, End) when FuncName =:= enum ->
  #meta_method{main = #method_main{params = Params, logic = Logic}} = get_meta_method(FuncName, Mates),
  Enum = get_method_enum_list(Name, Params, Logic, Sheet),
  write_code(Fd, "h_~s(~w, Cols, Values) ->", [FuncName, Name]),
  write_code(Fd, "  ?h_~s(~w, ~s, Cols, Values)~s", [FuncName, Name, Enum, End]).


get_method_func_code(Name, [Def], ?UNDEF, Sheet) -> get_define_value(Def, Sheet, Name);
get_method_func_code(_Name, Params, #method_logic{content = Content}, _Sheet) ->
  format("fun(~s) -> ~s end", [string:join(Params, ", "), Content]).

get_method_enum_list(Name, [Def], ?UNDEF, Sheet) -> get_define_value(Def, Sheet, Name);
get_method_enum_list(_Name, Params, ?UNDEF, _Sheet) -> format("[~s]", [string:join(Params, ", ")]);
get_method_enum_list(_Name, _Params, #method_logic{content = Content}, _Sheet) -> Content.

get_define_value("?"++_ = Key, _, _Name) -> Key;
get_define_value(DefKey, #sheet{metas = Mates}, Name) ->
  L = [{Key, Value} || #meta{method = #meta_method{main = #method_main{name = def, params = [Key, Value]}}} <- Mates],
  Value = proplists:get_value(DefKey, L),
  ?RETURN_IF(Value =:= ?UNDEF, {"get_define_value_failed", {def, DefKey}, {field, Name}}),
  Value.


get_field_columns(FieldCodes) ->
  Columns = [get_field_column(FieldCode) || FieldCode <- FieldCodes],
  [Column || Column <- Columns, Column =/= ?UNDEF].

get_field_column({Name, _}) -> format("F_~w", [Name]).


write_fill_record_fields(_Fd, [], _Sheet) -> pass;
write_fill_record_fields(Fd, [{Name, Code}], Sheet) ->
  write_fill_record_field(Fd, Name, Code, Sheet, "");
write_fill_record_fields(Fd, [{Name, Code}|Rest], Sheet) ->
  write_fill_record_field(Fd, Name, Code, Sheet, ","),
  write_fill_record_fields(Fd, Rest, Sheet).


write_fill_record_field(Fd, Name, Code, _Sheet, End) ->
  case format("F_~w", [Name]) of
    Code -> write_code(Fd, "    ~w = ~s~s", [Name, Code, End]);
    FName -> write_code(Fd, "    ~w = f(~w, ~s)~s", [Name, Name, FName, End])
  end.

write_field_codes(Fd, [{Name, Code}]) ->
  write_code(Fd, "f(~s, F_~w) ->", [Name, Name]),
  write_code(Fd, "  ~s.", [Code]);

write_field_codes(Fd, [{Name, Code}|Rest]) ->
  write_code(Fd, "f(~s, F_~w) ->", [Name, Name]),
  write_code(Fd, "  ~s;", [Code]),
  write_field_codes(Fd, Rest).


write_fill_record_key_field(Fd, #field{name = Name} = Field, RecordPre) ->
  ValueStr = get_key_field_value_str(Field, RecordPre),
  write_code(Fd, "    ~s = ~s", [Name, ValueStr]).

get_key_field_value_str(#field{default = Default}, RecordPre) when is_tuple(Default) ->
  get_key_field_value_str2(tuple_to_list(Default), "{~s}", RecordPre);

get_key_field_value_str(#field{default = Default}, RecordPre) when is_list(Default) ->
  get_key_field_value_str2(Default, "[~s]", RecordPre).

get_key_field_value_str2(List, RetFormat, RecordPre) ->
  List2 = [format("~s.~w", [RecordPre, Name]) || Name <- List],
  format(RetFormat, [string:join(List2, ", ")]).

get_field_codes(#sheet{record = #record{fields = Fields}} = Sheet) ->
  Codes = [{Name, get_field_code(Field, Sheet)} || #field{name = Name} = Field <- Fields],
  [{Name, Code} || {Name, Code} <- Codes, Code =/= ?UNDEF].


get_field_code(#field{metas = Mates} = Field, Sheet) ->
  try
    ?RETURN_IF(has_meta_method(key, Mates), ?UNDEF),
    ?RETURN_IF(has_meta_method(ignore, Mates), ?UNDEF),
    Code = get_field_impl_code(Field, Sheet),
    Code
  catch
    ?CATCH_RETURN(Ret) -> Ret
  end.

get_field_impl_code(#field{name = Name, default = Default, metas = Mates}, Sheet) ->
  get_field_impl_code2(Name, Default, Mates, Sheet, format("F_~w", [Name])).

get_field_impl_code2(_Name, Default, [], _Sheet, Code) ->
  case Default of
    ?UNDEF -> Code;
    _ -> format("?default(~s, ~s)", [Code, term_to_string(Default)])
  end;

get_field_impl_code2(Name, Default, [#meta{method = ?UNDEF}| Rest], Sheet, Code) ->
  get_field_impl_code2(Name, Default, Rest, Sheet, Code);

get_field_impl_code2(Name, Default, [#meta{method = #meta_method{main = #method_main{name = FuncName}} = Method}| Rest], Sheet, Code) ->
  NewCode = update_field_impl_code(Name, FuncName, Method, Sheet, Code),
  get_field_impl_code2(Name, Default, Rest, Sheet, NewCode).

update_field_impl_code(Name, FuncName, Method, Sheet, Code) when FuncName =:= check ->
  #meta_method{main = #method_main{params = Params, logic = Logic}} = Method,
  Func = get_method_func_code(Name, Params, Logic, Sheet),
  format("?f_~s(~w, ~s, ~s)", [FuncName, Name, Func, Code]);

update_field_impl_code(Name, FuncName, Method, Sheet, Code) when FuncName =:= enum ->
  #meta_method{main = #method_main{params = Params, logic = Logic}, modifiers = Modifiers} = Method,
  Enum = get_method_enum_list(Name, Params, Logic, Sheet),
  EnumStr = format("?f_~s(~w, ~s, ~s)", [FuncName, Name, Enum, Code]),
  get_modifiers_code(Name, Sheet, Modifiers, [cast], EnumStr, EnumStr);


update_field_impl_code(Name, FuncName, Method, Sheet, Code) when FuncName =:= list ->
  #meta_method{modifiers = Modifiers} = Method,
  case Modifiers of
    [] -> Code;
    [_|_] ->
      ValueStr = get_modifiers_code(Name, Sheet, Modifiers, [value_enum, value_cast], "_Value", "_Value"),
      ValueFilter = get_modifiers_code(Name, Sheet, Modifiers, [filter, value_filter], "_Value", ""),
      Filters = [Filter || Filter <- [ValueFilter], Filter =/= ""],
      FilterStr = string:join(Filters, ","),
      Code2 = format("[~s || _Value <- ~s~s]",
        [
          ValueStr,
          Code,
          ?VAL_IF(FilterStr =/= "", ", " ++ FilterStr, FilterStr)
        ]),
      get_modifiers_code(Name, Sheet, Modifiers, [cast], Code2, Code2)
  end;

update_field_impl_code(Name, FuncName, Method, Sheet, Code) when FuncName =:= map ->
  #meta_method{modifiers = Modifiers} = Method,
  case Modifiers of
    [] -> Code;
    [_|_] ->
      KeyStr = get_modifiers_code(Name, Sheet, Modifiers, [key_enum, key_cast], "_Key", "_Key"),
      ValueStr = get_modifiers_code(Name, Sheet, Modifiers, [value_enum, value_cast], "_Value", "_Value"),
      PairStr =
        case KeyStr =:= "_Key" andalso ValueStr =:= "_Value" of
          true -> format("{~s, ~s}", [KeyStr, ValueStr]);
          false -> format("{~n    ~s,~n    ~s~n    }", [KeyStr, ValueStr])
        end,
      RetStr = get_modifiers_code(Name, Sheet, Modifiers, [pair_cast], PairStr, PairStr),
      KeyFilter = get_modifiers_code(Name, Sheet, Modifiers, [key_filter], "_Key", ""),
      ValueFilter = get_modifiers_code(Name, Sheet, Modifiers, [value_filter], "_Value", ""),
      PairFilter = get_modifiers_code(Name, Sheet, Modifiers, [filter], "_Pair", ""),
      Filters = [Filter || Filter <- [KeyFilter, ValueFilter, PairFilter], Filter =/= ""],
      FilterStr = string:join(Filters, ","),
      Code2 = format("[~s || {_Key, _Value} = _Pair <- ~s~s]",
        [
          RetStr,
          Code,
          ?VAL_IF(FilterStr =/= "", ", " ++ FilterStr, FilterStr)
        ]),
      get_modifiers_code(Name, Sheet, Modifiers, [cast], Code2, Code2)
  end;

update_field_impl_code(Name, FuncName, Method, Sheet, Code) when FuncName =:= prop ->
  #meta_method{modifiers = Modifiers} = Method,
  case Modifiers of
    [] -> Code;
    [_|_] ->
      KeyStr = get_modifiers_code(Name, Sheet, Modifiers, [key_enum, key_cast], "_Key", ""),
      RetStr =
        case KeyStr of
          "" -> get_modifiers_code(Name, Sheet, Modifiers, [value_cast], "_Pair", "_Pair");
          _ ->
            CastStr = get_modifiers_code(Name, Sheet, Modifiers, [value_cast], "_Pair2", "_Pair2"),
            format("begin ~n    _Key = element(1, _Pair),~n    _Key2 = ~s,~n    _Pair2 = erlang:setelement(1, _Pair, _Key2),~n    ~s~n    end",
              [KeyStr, CastStr])
        end,
      KeyFilter = get_modifiers_code(Name, Sheet, Modifiers, [key_filter], "element(1, _Pair)", ""),
      PairFilter = get_modifiers_code(Name, Sheet, Modifiers, [filter], "_Pair", ""),
      Filters = [Filter || Filter <- [KeyFilter, PairFilter], Filter =/= ""],
      FilterStr = string:join(Filters, ","),
      Code2 = format("[~s || _Pair <- ~s~s]",
        [
          RetStr,
          Code,
          ?VAL_IF(FilterStr =/= "", ", " ++ FilterStr, FilterStr)
        ]),
      get_modifiers_code(Name, Sheet, Modifiers, [cast], Code2, Code2)
  end;

update_field_impl_code(Name, _FuncName, Method, Sheet, Code) ->
  #meta_method{modifiers = Modifiers} = Method,
  case Modifiers of
    [] -> Code;
    [_|_] ->
      CastStr = get_modifiers_code(Name, Sheet, Modifiers, [cast], Code, Code),
      CastStr
  end.


get_modifiers_code(Name, Sheet, Modifiers, Names, Code, Default) ->
  Modifiers2 = [Modifier || #method_modifier{name = ModifierName} = Modifier <- Modifiers, lists:member(ModifierName, Names)],
  case Modifiers2 of
    [] -> Default;
    _ -> get_modifiers_code2(Modifiers2, Name, Sheet, Code)
  end.

get_modifiers_code2([], _Name, _Sheet, Code) -> Code;
get_modifiers_code2([Modifier| Rest], Name, Sheet, Code) ->
  NewCode = update_modifier_code(Modifier, Name, Sheet, Code),
  get_modifiers_code2(Rest, Name, Sheet, NewCode).

update_modifier_code(#method_modifier{name = M, params = Params, logic = Logic}, Name, Sheet, Code) when M =:= key_enum; M =:= value_enum ->
  Enum = get_method_enum_list(Name, Params, Logic, Sheet),
  format("?f_enum(~s, ~s, ~s)", [Name, Enum, Code]);

update_modifier_code(#method_modifier{name = M, params = Params, logic = Logic}, Name, Sheet, Code) when M =:= key_cast; M =:= value_cast; M =:= cast ->
  Func = get_method_func_code(Name, Params, Logic, Sheet),
  format("?f_cast(~s, ~s, ~s)", [Name, Func, Code]);

update_modifier_code(#method_modifier{name = M, params = Params, logic = Logic}, Name, Sheet, Code) when M =:= filter ->
  Func = get_method_func_code(Name, Params, Logic, Sheet),
  format("?f_filter(~s, ~s, ~s)", [Name, Func, Code]).


%% parse data
get_meta_method_param(Method, Metas) ->
  case get_meta_method_params(Method, Metas) of
    [] -> "";
    List -> hd(List)
  end.

get_meta_method(MethodName, Metas) ->
  Ret = lists:dropwhile(
    fun
      (#meta{method = #meta_method{main = #method_main{name = Name}}}) -> MethodName =/= Name;
      (#meta{}) -> true
    end,
    Metas),
  case Ret of
    [] -> ?UNDEF;
    [#meta{method = Method}|_] -> Method
  end.

has_meta_method(MethodName, Mates) ->
  lists:any(
    fun(#meta{method = Method}) ->
      case Method of
        #meta_method{main = #method_main{name = MethodName}} -> true;
        _ -> false
      end
    end,
    Mates).

%%has_any_meta_method(Mates) ->
%%  lists:any(
%%    fun(#meta{method = Method}) -> Method =/= ?UNDEF end,
%%    Mates).


get_meta_method_params(_Name, []) -> [];
get_meta_method_params(Name, [#meta{method = #meta_method{main = #method_main{name = Name, params = Params}}}|_]) ->
  Params;
get_meta_method_params(Name, [#meta{}|Rest]) ->
  get_meta_method_params(Name, Rest).

get_field_headers(#sheet{record = #record{fields = Fields}, metas = Mates}) ->
  Headers = [get_field_header(Field, Mates) || Field <- Fields],
  Headers2 = [Header || Header <- Headers, Header =/= ?UNDEF],
  string:join(Headers2, ", ").

get_field_header(#field{name = Name, metas = Mates}, _HeadMates) ->
  try
    ?RETURN_IF(has_meta_method(key, Mates), ?UNDEF),
    ?RETURN_IF(has_meta_method(ignore, Mates), ?UNDEF),
    Params = get_meta_method_params(ref, Mates),
    case Params of
      ["\"" ++ _ = RefName] -> ?RETURN(RefName);
      [RefName] -> ?RETURN(lists:concat(["\"", RefName, "\""]));
      _ -> pass
    end,
    lists:concat(["h(", Name, ")"])
  catch
    ?CATCH_RETURN(Ret) -> Ret
  end.


get_sheet_key_name(#sheet{record = #record{fields = [#field{name = Name}|_] = Fields}}) ->
  case get_sheet_key_name2(Fields) of
    ?UNDEF -> Name;
    Key -> Key
  end.

get_sheet_key_name2([]) -> ?UNDEF;
get_sheet_key_name2([#field{name = Name, metas = Metas}| Rest]) ->
  case has_meta_method(key, Metas) of
    true -> Name;
    false -> get_sheet_key_name2(Rest)
  end.

get_key_field(#sheet{record = #record{fields = Fields}}) ->
  case get_key_field2(Fields) of
    ?UNDEF -> ?UNDEF;
    Field -> Field
  end.

get_key_field2([]) -> ?UNDEF;
get_key_field2([#field{metas = Metas} = Field| Rest]) ->
  case has_meta_method(key, Metas) of
    true -> Field;
    false -> get_key_field2(Rest)
  end.

get_header_implements(#sheet{record = #record{fields = Fields}, metas = Mates}) ->
  Impls = [{Name, get_header_codes(Field, Mates)} || #field{name = Name} = Field <- Fields],
  lists:filter(fun({_, Code}) -> Code =/= ?UNDEF end, Impls).

get_header_codes(#field{name = Name, metas = Mates}, _HeadMates) ->
  try
    ?RETURN_IF(has_meta_method(key, Mates), ?UNDEF),
    ?RETURN_IF(has_meta_method(ignore, Mates), ?UNDEF),
    ?RETURN_IF(get_meta_method_params(ref, Mates) =/= [], ?UNDEF),
    ?RETURN_IF(has_meta_method(list, Mates), lists:concat(["{fun h_list/3, ", Name, "}"])),
    ?RETURN_IF(has_meta_method(map, Mates), lists:concat(["{fun h_map/3, ", Name, "}"])),
    ?RETURN_IF(has_meta_method(prop, Mates), lists:concat(["{fun h_prop/3, ", Name, "}"])),
    ?RETURN_IF(has_meta_method(group, Mates), lists:concat(["{fun h_group/3, ", Name, "}"])),
    lists:concat(["?h_auto_ref(", Name, ")"])
  catch
    ?CATCH_RETURN(Ret) -> Ret
  end.

get_uf8_binary(String) ->
  unicode:characters_to_binary(String).

term_to_string(Term) ->
  R= io_lib:format("~p",[Term]),
  lists:flatten(R).

format(Fmt, Args) ->
  lists:flatten(io_lib:format(Fmt, Args)).
