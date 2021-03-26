%%%-------------------------------------------------------------------
%%% @author Nero.li
%%% @copyright (C) 2021, <Lilith>
%%% @doc
%%%   测试模块
%%% @end
%%% Created : 26. 二月 2021 16:26
%%%-------------------------------------------------------------------
-module(sheets_compile).
-author("lifan").

%% API
-export([compile/2, out_files/2, generate/0, yacc_parse/0, load_def_file/0]).

-define(DEF_FILE, "include/sheets_def.hrl").
-define(RECORD_LEX_FILE, "src/sheets_record_lex.xrl").
-define(RECORD_YACC_FILE, "src/sheets_record_yacc.yrl").
-define(MODULE_LEX_FILE, "src/sheets_module_lex.xrl").
-define(MODULE_YACC_FILE, "src/sheets_module_yacc.yrl").

compile(DefFile, Options) ->
  Sheets = prepare_lex_yacc(DefFile, Options),
  DefInc = filename:basename(DefFile),
  Options2 = merge_option_values(Options, {include, ["sheets_common.hrl", DefInc]}),
  %io:format("~s:~w generate ~p options ~p~n", [?MODULE, ?LINE, Sheets, Options2]),
  sheets_generate:generate(Sheets, Options2).

merge_option_values(Options, {Key, Values}) ->
  Found = proplists:get_value(Key, Options, []),
  NewValues = lists:umerge(Found, Values),
  lists:keystore(Key, 1, Options, {Key, NewValues}).

out_files(DefFile, Options) ->
  Sheets = prepare_lex_yacc(DefFile, Options),
  sheets_generate:get_out_files(Sheets, Options).

generate() ->
  {ok, Tokens, _} = lex_parse(),
  {ok, Sheets} = yacc_parse(Tokens),
  Options = [{our_dir, "src/sheet/"}, {include, ["sheets_common.hrl"]}, {force, true}, {clean, true}],
  sheets_generate:generate(Sheets, Options).

prepare_lex_yacc(DefFile, _GenOptions) ->
  {ok, Tokens, _} = lex_parse(DefFile, sheets_record_lex),
  %io:format("~s:~w lex_parse ~p tokens ~p~n", [?MODULE, ?LINE, DefFile, Tokens]),
  {ok, Sheets} = yacc_parse(Tokens, sheets_record_yacc),
  Sheets.


lex_compile(LexFile, Force) ->
  ModuleName = module_name(LexFile),
  FileName = filename:join([filename:dirname(LexFile), lists:concat([ModuleName, ".erl"])]),
  case Force orelse filelib:last_modified(LexFile) > filelib:last_modified(FileName) of
    false -> pass;
    true -> {ok, FileName} = leex:file(LexFile, [dfa_graph])
  end,
  compile:file(FileName),
  code:soft_purge(ModuleName),
  code:load_file(ModuleName).

lex_compile() -> lex_compile(?RECORD_LEX_FILE, true).

lex_parse(DefFile, ModuleName) when is_list(ModuleName) ->
  Module = module_name(ModuleName),
  lex_parse(DefFile, Module);

lex_parse(DefFile, Module) when is_atom(Module) ->
  Binary = load_def_file(DefFile),
  List = unicode:characters_to_list(Binary),
  Module:string(List).

lex_parse() -> lex_parse(?DEF_FILE, ?RECORD_LEX_FILE).

yacc_compile(YaccFile, Force) ->
  ModuleName = module_name(YaccFile),
  FileName = filename:join([filename:dirname(YaccFile), lists:concat([ModuleName, ".erl"])]),
  case Force orelse filelib:last_modified(YaccFile) > filelib:last_modified(FileName) of
    false -> pass;
    true -> {ok, FileName} = yecc:file(YaccFile)
  end,
  compile:file(FileName),
  code:soft_purge(ModuleName),
  code:load_file(ModuleName).

yacc_compile() -> yacc_compile(?RECORD_YACC_FILE, true).


yacc_parse(Tokens, ModuleName) when is_list(ModuleName) ->
  Module = module_name(ModuleName),
  yacc_parse(Tokens, Module);

yacc_parse(Tokens, Module) when is_atom(Module) ->
  Module:parse(Tokens).

yacc_parse(Tokens) -> yacc_parse(Tokens, ?RECORD_YACC_FILE).

yacc_parse() ->
  lex_compile(),
  {ok, Tokens, _} = lex_parse(),
  yacc_compile(),
  yacc_parse(Tokens).


load_def_file() -> load_def_file(?DEF_FILE).
load_def_file(FileName) ->
  {ok, Binary} = file:read_file(FileName),
  Binary.

module_name(FileName) ->
  list_to_atom(filename:rootname(filename:basename(FileName))).
