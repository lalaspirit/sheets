%%%-------------------------------------------------------------------
%%% @author Nero.li
%%% @copyright (C) 2021, <Lilith>
%%% @doc
%%%   函数支持
%%% @end
%%% Created : 10. 三月 2021 17:32
%%%-------------------------------------------------------------------
-module(sheets_func).
-author("lifan").

%%%===================================================================
%%% API
%%%===================================================================
-export([auto_ref/3, map/4, prop/4, list/4, group/4, check/3, filter/3, enum/3, cast/3]).
-export([filter_empty/1, wrap_re_str/1]).
-export([ets_take_l/2, ets_take_r/2, ets_keys/1, ets_foldtake_l/3, ets_foldtake_r/3]).
-include("sheets_common_def.hrl").

%%%===================================================================
%%% Functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc auto reference field
%% @spec h_auto_ref(Name, Cols, Values) -> Value
%%   Name = atom(), Cols = [string()]
%%   Values = [Value]
%%   Value = term()
%%--------------------------------------------------------------------
auto_ref(Name, Cols, Values) when is_atom(Name) ->
  Name1 = atom_to_list(Name),
  Name2 = string_replace(Name1, "_", ""),
  Name3 = string:to_lower(Name2),
  auto_ref(Name3, Cols, Values);

auto_ref(Name, [ColName|ColRest], [Value|ValRest]) when is_list(Name) ->
  case string:to_lower(string_replace(ColName, " ", "")) of
    Name -> Value;
    _Other -> auto_ref(Name, ColRest, ValRest)
  end;

auto_ref(Name, [], []) -> ?RETURN({field_not_found, Name}).


%%--------------------------------------------------------------------
%% @doc map field
%% @spec map(Name, ReStr, Cols, Values) -> [{Key, Value}]
%%   Name = atom(), ReStr = string(), Cols = [string()]
%%   Values = [Value]
%%   Value = term()
%%--------------------------------------------------------------------
map(Name, ReStr, Cols, Values) ->
  {ok, Re} = re:compile(wrap_re_str(ReStr)),
  [
    try
      C = lists:nth(N, Cols),
      {match, [[_|_] = Key]} = re:run(C, Re, [{capture, [1], list}]),
      {Key, lists:nth(N, Values)}
    catch
       _E : _R ->
         Trace = erlang:get_stacktrace(),
         ?RETURN({map_error, Name, ReStr, N, Cols, Values, {_E, _R, Trace}})
    end || N <- lists:seq(1, length(Cols)), re:run(lists:nth(N, Cols), Re) =/= nomatch
  ].


%%--------------------------------------------------------------------
%% @doc prop field
%% @spec prop(Name, ReStrList, Cols, Values) -> [tuple()]
%%   Name = atom(), ReStrList = [string()], Cols = [string()]
%%   Values = [Value]
%%   Value = term()
%%--------------------------------------------------------------------
prop(Name, ReStrList, Cols, Values) ->
  Props = [lists:keysort(1, map(Name, ReStr, Cols, Values)) || ReStr <- ReStrList],
  build_props(Props, Name, []).

build_props([], _Name, Rslt) -> lists:reverse(Rslt);
build_props([[], []], _Name, Rslt) -> lists:reverse(Rslt);
build_props([[], [], []], _Name, Rslt) -> lists:reverse(Rslt);
build_props([[], [], [], []], _Name, Rslt) -> lists:reverse(Rslt);

build_props([[{Key, Value1}|Rest1], [{Key, Value2}|Rest2]], Name, Rslt) ->
  build_props([Rest1, Rest2], Name, [{Value1, Value2}|Rslt]);

build_props([[{Key, Value1}|Rest1], [{Key, Value2}|Rest2], [{Key, Value3}|Rest3]], Name, Rslt) ->
  build_props([Rest1, Rest2, Rest3], Name, [{Value1, Value2, Value3}|Rslt]);

build_props([[{Key, Value1}|Rest1], [{Key, Value2}|Rest2], [{Key, Value3}|Rest3], [{Key, Value4}|Rest4]], Name, Rslt) ->
  build_props([Rest1, Rest2, Rest3, Rest4], Name, [{Value1, Value2, Value3, Value4}|Rslt]);

build_props(Props, Name, Rslt) -> ?RETURN({map_key_error, Name, Props, Rslt}).


%%--------------------------------------------------------------------
%% @doc list field
%% @spec list(Name, ReStr, Cols, Values) -> [{Key, Value}]
%%   Name = atom(), ReStr = string(), Cols = [string()]
%%   Values = [Value]
%%   Value = term()
%%--------------------------------------------------------------------
list(Name, ReStr, Cols, Values) ->
  {ok, Re} = re:compile(wrap_re_str(ReStr)),
  List =
    [
      lists:nth(N, Values) || N <- lists:seq(1, length(Cols)), re:run(lists:nth(N, Cols), Re, [{capture, none}]) =:= match
    ],
  ?RETURN_IF(List =:= [], {list_length_error, Name, ReStr}),
  List.


%%--------------------------------------------------------------------
%% @doc group field
%% @spec group(Name, ReStrList, Cols, Values) -> tuple()
%%   Name = atom(), ReStrList = [string()], Cols = [string()]
%%--------------------------------------------------------------------
group(Name, ReStrList, Cols, Values) ->
  list_to_tuple([
    case list(Name, ReStr, Cols, Values) of
      [] -> ?RETURN({group_not_matched, Name, ReStr});
      [Value] -> Value;
      List -> List
    end || ReStr <- ReStrList]).


%%--------------------------------------------------------------------
%% @doc check field
%% @spec check(Name, Func, Value) -> Value
%%   Name = atom(), Func = function(), Value = term()
%%--------------------------------------------------------------------
check(Name, Func, Value) ->
  ?RETURN_IF(not Func(Value), {field_check_error, Name, Value, term_to_string(Func)}),
  Value.


%%--------------------------------------------------------------------
%% @doc enum field
%% @spec enum(Name, EnumList, Value) -> Value
%%   Name = atom(), EnumList = [EnumValue]
%%   EnumValue = term()
%%   Value = term()
%%--------------------------------------------------------------------
enum(Name, EnumList, Value) ->
  ?RETURN_IF(not lists:member(Value, EnumList), {field_enum_error, Name, Value, EnumList}),
  Value.


%%--------------------------------------------------------------------
%% @doc cast field
%% @spec cast(Name, Func, Value) -> Value
%%   Name = atom(), Func = function(Value) -> Value, Value = term()
%%--------------------------------------------------------------------
cast(Name, Func, Value) ->
  try
    Func(Value)
  catch
    _E:_R ->
      Trace = erlang:get_stacktrace(),
      ?RETURN({field_cast_error, Name, Value, {_E, _R, Trace}})
  end.


%%--------------------------------------------------------------------
%% @doc filter field
%% @spec filter(Name, Func, Value) -> true | false
%%   Name = atom(), Func = function(), Value = term()
%%--------------------------------------------------------------------
filter(Name, Func, Value) ->
  case Func(Value) of
    true -> true;
    false -> false;
    Other -> ?RETURN({field_filter_error, Name, Value, Other})
  end.

%%--------------------------------------------------------------------
%% @doc filter empty value
%% @spec filter_empty(List) -> List2
%%--------------------------------------------------------------------
filter_empty(List) ->
  lists:filtermap(
    fun({Key, Value}) when is_atom(Key), is_integer(Value) -> Value > 0;
      ({Key, ID, Value}) when is_atom(Key), is_integer(ID), is_integer(Value) -> ID > 0 andalso Value > 0;
      ({ID, Value}) when is_integer(ID), is_integer(Value) -> ID > 0 andalso Value > 0;
      (L) when is_list(L) ->
        case filter_empty(L) of
          [] -> false;
          L2 -> {true, L2}
        end
    end,
    List
  ).


%%%===================================================================
%%% Ets functions
%%%===================================================================

%% 遍历并取出一个值, 从小开始
%% ets_take_l(ets(), Fun) -> Ret | undefined
%%  when
%%    Fun=fun(Value) -> {IsFinish, Ret} | false
%%    IsFinish = true | false, Ret = term()
ets_take_l(Ets, Fun) ->
  ets:safe_fixtable(Ets, true),
  Ret = ets_take_l(Ets, ets:first(Ets), Fun),
  ets:safe_fixtable(Ets, false),
  Ret.

ets_take_l(_Ets, ?EOT, _Fun) -> false;
ets_take_l(Ets, Key, Fun) ->
  [Value] = ets:lookup(Ets, Key),
  case Fun(Value) of
    {true, Ret} -> {true, Ret};
    false -> ets_take_l(Ets, ets:next(Ets, Key), Fun)
  end.

%% 遍历并取出一个值, 从大开始
%% ets_take_r(ets(), Fun) -> Ret | undefined
%%  when
%%    Fun=fun(Value) -> {IsFinish, Ret} | false
%%    IsFinish = true | false, Ret = term()
ets_take_r(Ets, Fun) ->
  ets:safe_fixtable(Ets, true),
  Ret = ets_take_r(Ets, ets:last(Ets), Fun),
  ets:safe_fixtable(Ets, false),
  Ret.

ets_take_r(_Ets, ?EOT, _Fun) -> false;
ets_take_r(Ets, Key, Fun) ->
  [Value] = ets:lookup(Ets, Key),
  case Fun(Value) of
    {true, Ret} -> {true, Ret};
    false -> ets_take_r(Ets, ets:prev(Ets, Key), Fun)
  end.

%% 遍历并取出一个值，Fun=(Tab, Key, Acc) -> {IsFinish, NewAcc} | false
%% IsFinish = true 表示停止寻找，返回NewAcc
%% IsFinish = false 表示继续寻找，使用NewAcc
%% false 表示使用上一个Acc寻找
ets_foldtake_l(Ets, Fun, Acc) ->
  ets_foldtake_l(Ets, Fun, Acc, ?UNDEF).

ets_foldtake_l(Ets, Fun, Acc, FisrtKey) ->
  ets:safe_fixtable(Ets, true),
  First =
    case FisrtKey of
      ?UNDEF -> ets:first(Ets);
      _ -> FisrtKey
    end,
  Ret = ets_foldtake_l_0(Ets, First, Fun, Acc),
  ets:safe_fixtable(Ets, false),
  Ret.

ets_foldtake_l_0(_Tab, ?EOT, _Fun, Acc) -> Acc;
ets_foldtake_l_0(Tab, Key, Fun, Acc) ->
  Ret =
    case Fun of
      {M, F} -> M:F(Tab, Key, Acc);
      Fun -> Fun(Tab, Key, Acc)
    end,
  case Ret of
    {true, NewAcc} -> NewAcc;
    {false, NewAcc} -> ets_foldtake_l_0(Tab, ets:next(Tab, Key), Fun, NewAcc);
    false -> ets_foldtake_l_0(Tab, ets:next(Tab, Key), Fun, Acc)
  end.

%% IsFinish = false 表示继续寻找，使用NewAcc
%% false 表示使用上一个Acc寻找
ets_foldtake_r(Ets, Fun, Acc) ->
  ets_foldtake_r(Ets, Fun, Acc, ?UNDEF).

ets_foldtake_r(Ets, Fun, Acc, LastKey) ->
  ets:safe_fixtable(Ets, true),
  Last =
    case LastKey of
      ?UNDEF -> ets:last(Ets);
      _ -> LastKey
    end,
  Ret = ets_foldtake_r_0(Ets, Last, Fun, Acc),
  ets:safe_fixtable(Ets, false),
  Ret.

ets_foldtake_r_0(_Tab, ?EOT, _Fun, Acc) -> Acc;
ets_foldtake_r_0(Tab, Key, Fun, Acc) ->
  Ret =
    case Fun of
      {M, F} -> M:F(Tab, Key, Acc);
      Fun -> Fun(Tab, Key, Acc)
    end,
  case Ret of
    {true, NewAcc} -> NewAcc;
    {false, NewAcc} -> ets_foldtake_r_0(Tab, ets:prev(Tab, Key), Fun, NewAcc);
    false -> ets_foldtake_r_0(Tab, ets:prev(Tab, Key), Fun, Acc)
  end.

ets_keys(Ets) ->
  ets_foldtake_r(Ets, fun(_, Key, Keys) -> {false, [Key| Keys]} end, []).


term_to_string(Term) ->
  R = io_lib:format("~p",[Term]),
  lists:flatten(R).

string_replace(String, Re, To) ->
  re:replace(String, Re, To, [global, {return,list}]).

wrap_re_str("^" ++ _ = Str) ->
  wrap_re_str2(Str);
wrap_re_str(Str) ->
  wrap_re_str2("^" ++ Str).

wrap_re_str2(Str) ->
  case lists:reverse(Str) of
    "$"++_ -> Str;
    Rever -> lists:reverse("$" ++ Rever)
  end.
