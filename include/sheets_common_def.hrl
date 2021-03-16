%%%-------------------------------------------------------------------
%%% @author Nero.Li
%%% @copyright (C) 2019, <ProjectCat>
%%% @doc
%%%   公用宏定义
%%% @end
%%% Created : 23. 四月 2019 22:22
%%%-------------------------------------------------------------------
-author("Nero.Li").

-ifndef(__SHEETS_COMMON_DEF_H__).
-define(__SHEETS_COMMON_DEF_H__, true).

-include_lib("stdlib/include/ms_transform.hrl").

%% 空值
-define(UNDEF, undefined).
%% 表的末尾
-define(EOT, '$end_of_table').

%% 布尔值:true和false
-define(TRUE, true).
-define(FALSE, false).
-define(true, true).
-define(false, false).

%% 其他常用atom定义
-define(OK, ok).
-define(NOT_FOUND, not_found).

%% 桩子
-define(NIY, erlang:nif_error(not_implement_yet)).

%% 常用配置ETS选项
-define(COMMON_ETS_OPTS, [named_table, public, {read_concurrency, true}]).
-define(ETS_OPT_WRITE, {write_concurrency, true}).


-define(RETURN(Error), throw({return, Error})).
-define(RETURN_IF(IsTrue, Error), (IsTrue) andalso ?RETURN(Error)).
-define(OK_IF(IsTrue, Error), case IsTrue of true -> ok; _ -> Error end).
-define(OK_IF_NOT(IsTrue, Error), case IsTrue of false -> ok; _ -> Error end).
-define(VAL_IF(COND, TRUE_VAL, FALSE_VAL), case COND of true -> TRUE_VAL; false -> FALSE_VAL end).
-define(CATCH_RETURN(Error), throw:{return, Error}).


-endif.
