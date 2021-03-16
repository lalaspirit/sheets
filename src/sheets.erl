%%%-------------------------------------------------------------------
%%% @author Nero.li
%%% @copyright (C) 2020, <ProjectCat>
%%% @doc
%%%   表格对外接口
%%% @end
%%% Created : 17. 五月 2020 15:11
%%%-------------------------------------------------------------------
-module(sheets).
-author("lifan").

%% API
-export([init/2, get_ets/1, reload_sheet/1, reload_sheet/5, fill_sheet/2, fill_sheet/3]).
-export([test/0]).

-include("sheets_common_def.hrl").
-define(CSV_DIR_KEY, '_csv_dir_').
-define(SHEETS_ETS, sheets_ets).
%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc 初始化所有表
%% @spec init(CsvDir) ->
%%--------------------------------------------------------------------
init(SheetModules, CsvDir) ->
  ets:new(?SHEETS_ETS, [set | ?COMMON_ETS_OPTS]),
  ets:insert(?SHEETS_ETS, {?CSV_DIR_KEY, CsvDir}),
  lists:foreach(
    fun(Module) ->
      Ets = get_ets_name(Module),
      ets:insert(?SHEETS_ETS, {Module, Ets}),
      Module:init()
    end,
    SheetModules
  ).

get_ets_name(Module) ->
  list_to_atom(lists:concat([Module, "_ets"])).

%%--------------------------------------------------------------------
%% @doc 获取某个sheets模块的ets表名
%% @spec get_ets(SheetModule) ->
%%--------------------------------------------------------------------
get_ets(Module) -> ets:lookup_element(?SHEETS_ETS, Module, 2).



%%--------------------------------------------------------------------
%% @doc 重新载入csv表到ets中，ets不存在会自动创建，只能是named_table
%% @spec reload_csv(Ets, CsvPath, Columns, InsertFun, HookFun) -> ok
%%  when Ets = atom(), CsvPath = Col
%%--------------------------------------------------------------------
reload_sheet(Ets, CsvPath, Columns, InsertFun, HookFun) ->
  {EtsName, Opts} = case is_tuple(Ets) of
    true -> Ets;
    false -> {Ets, get_ets_options(Ets)}
  end,
  safe_create(EtsName, Opts),
  safe_reload(EtsName, Opts, CsvPath, Columns, InsertFun, HookFun).


%%--------------------------------------------------------------------
%% @doc 重新载入csv表到ets中，ets不存在会自动创建，只能是named_table
%% @spec reload_csv(Module) -> ok
%%  when Module = atom()
%%--------------------------------------------------------------------
reload_sheet(Module) when is_atom(Module) ->
  reload_sheet(get_ets(Module), Module:file(), Module:headers(), fun Module:add_record/2, ?UNDEF);

%%--------------------------------------------------------------------
%% @doc 重新载入多个csv表到ets中，循环调用reload_csv/5
%% @spec reload_csv([{Ets, CsvPath, Columns, InsertFun, HookFun}|...]) -> ok
%%--------------------------------------------------------------------
reload_sheet([]) -> ok;
reload_sheet([{Ets, FilePath, Columns, InsertFun, HookFun} | Rest]) ->
  case reload_sheet(Ets, FilePath, Columns, InsertFun, HookFun) of
    ok -> reload_sheet(Rest)
  end.

%%--------------------------------------------------------------------
%% @doc 填充一个表，每列对应
%% @spec fill_sheet(SheetRecord, Columns) -> NewSheetRecord
%%  SheetRecord = NewSheetRecord = record()
%%--------------------------------------------------------------------
fill_sheet(Record, Columns) ->
  fill_sheet(Record, Columns, 2).

fill_sheet(Record, [], _) -> Record;
fill_sheet(Record, [Col|Rest], Index) ->
  fill_sheet(erlang:setelement(Index, Record, Col), Rest, Index + 1).


%% 文件名
file_name(File) -> csv_dir() ++ "/" ++ File.

csv_dir() ->
  ets:lookup_element(?SHEETS_ETS, ?CSV_DIR_KEY, 2).

% 测试函数
test() ->
  File = file_name("Unit.csv"),
  Ret = csv_parser:parse(File),
  Ret.

%%%===================================================================
%%% 内部函数实现
%%%===================================================================
get_ets_options(Ets) when is_atom(Ets) ->
  case ets:info(Ets) of
    undefined ->
      [set, {keypos, 2} | ?COMMON_ETS_OPTS];
    InfoList ->
      infolist_to_options(InfoList)
  end.

infolist_to_options(InfoList) ->
  true = proplists:get_value(named_table, InfoList),
  Type = proplists:get_value(type, InfoList),
  public = proplists:get_value(protection, InfoList),
  KeyPos = proplists:get_value(keypos, InfoList),
  [Type, named_table, public, {keypos, KeyPos}, {read_concurrency, true}].

safe_create(Ets, Opts) ->
  case ets:info(Ets) of
    undefined -> ets:new(Ets, Opts);
    _Other -> ok
  end.

safe_reload(EtsName, Opts, Csv, ColumnSequence, InsertFun, HookFun) ->
  Owner = ets:info(EtsName, owner),
  TmpTable = create_temp_table(EtsName, Opts),
  Table = csv_parser:parse_and_fetch(file_name(Csv), ColumnSequence),
  [call_fun(InsertFun, [TmpTable, Line]) || Line <- Table],
  HookFun =/= undefined andalso call_fun(HookFun, [TmpTable]),
  ets:delete(EtsName),
  ets:rename(TmpTable, EtsName),
  self() =/= Owner andalso ets:give_away(EtsName, Owner, undefined),
  ok.

create_temp_table(EtsName, Opts) ->
  TmpName = list_to_atom(lists:concat(["tmp_",EtsName])),
  TmpName = safe_create(TmpName, Opts),
  ets:delete_all_objects(TmpName),
  TmpName.

call_fun({M,F}, Args) ->
  erlang:apply(M, F, Args);
call_fun(F, Args) when is_function(F) ->
  erlang:apply(F, Args).

