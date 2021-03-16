%%%-------------------------------------------------------------------
%%% @author Nero.li
%%% @copyright (C) 2020, <ProjectCat>
%%% @doc
%%%   公用的表宏
%%% @end
%%% Created : 05. 六月 2020 23:29
%%%-------------------------------------------------------------------
-author("nero.li").

-ifndef(__COMMON_SHEET_MACRO_H__).
-define(__COMMON_SHEET_MACRO_H__, true).
-include("sheets_common_def.hrl").

%% 获取表所在ets
-define(sheet_ets(Module), sheets:get_ets(Module)).

-define(sheet_ets_new(Module), sheets:get_ets(Module)).

%% 初始化一个set类型的ets
-define(new_set_ets(Module, KeyPos), ets:new(?sheet_ets_new(Module), [ordered_set, {keypos, KeyPos} | ?COMMON_ETS_OPTS])).

%% 重新载入到表格中
-define(reload_sheet(Module), sheets:reload_sheet(Module)).


%% 测试表中是否存在记录
-define(has_record(Module, Key), ets:member(?sheet_ets(Module), Key)).


%% 获取表中记录
-define(get_record(Module, Key),
  case ets:lookup(?sheet_ets(Module), Key) of
    [] -> ?RETURN({"sheet_record_not_found", Module, Key});
    [Record] -> Record
  end
).

%% 查找表中记录
-define(find_record(Module, Fun),
  sheets_func:ets_take_l(
    fun(Key) -> try Value = Fun(Key), {true, Value} catch _:_ -> false end end,
    ?sheet_ets(Module)
  )
).

%% 查找表中记录
-define(rfind_record(Module, Fun),
  sheets_func:ets_take_r(
    fun(Key) -> try Value = Fun(Key), {true, Value} catch _:_ -> false end end,
    ?sheet_ets(Module)
  )
).

%% 默认值
-define(default(Value, Default), ?VAL_IF(Value =:= ?UNDEF, Default, Value)).

%% 表头处理函数
-define(h_auto_ref(Name), {fun sheets_func:auto_ref/3, Name}).
-define(h_map(Name, ReStr, Cols, Values), sheets_func:map(Name, ReStr, Cols, Values)).
-define(h_prop(Name, ReStrList, Cols, Values), sheets_func:prop(Name, ReStrList, Cols, Values)).
-define(h_list(Name, ReStr, Cols, Values), sheets_func:list(Name, ReStr, Cols, Values)).
-define(f_check(Name, Fun, Value), sheets_func:check(Name, Fun, Value)).
-define(f_filter(Name, Fun, Value), sheets_func:filter(Name, Fun, Value)).
-define(f_cast(Name, Fun, Value), sheets_func:cast(Name, Fun, Value)).
-define(f_enum(Name, Enum, Value), sheets_func:enum(Name, Enum, Value)).

%% 转换定义
-define(to_atom, fun(Key) -> list_to_atom(string:lowercase(Key)) end).

-endif.
