%%%-------------------------------------------------------------------
%%% @author Nero.li
%%% @copyright (C) 2020, <ProjectCat>
%%% @doc
%%%   csv解析器
%%% @end
%%% Created : 17. 五月 2020 15:11
%%%-------------------------------------------------------------------
-module(sheets_csv_parser).
-author("lifan").

%% API
-export([parse/1, parse_and_fetch/2]).
-include("sheets_common_def.hrl").

%% 解析的状态
-record(pstate, {
  line_num = 1,
  types = [],
  names = [],
  rows = []
}).

%% 行的状态
-record(row, {
  key,
  row_num = 1,
  fields = []
}).

%% 数据字段状态
-record(fprops, {
  name,
  is_key = false,     % key字段
  is_number = false,  % 数据字段
  is_boolean = false, % 布尔字段
  is_string = false,  % 字符串字段
  is_array = false,   % 数组字段
  is_auto = false,    % 自动处理字段
  is_omit = false     % 是否省略
}).

-define(ARRAY_DELIM, ";").
-define(COMPLEX_DELIM, ",").
%%====================================================================
%% API
%%====================================================================
-ifdef(OTP_RELEASE).
%% 解析csv文件
parse(CsvFile) ->
  {{ok, IoDevice}, CsvFile} = {file:open(CsvFile, [read, compressed]), CsvFile},
  try
    PState = ecsv:process_csv_file_with(IoDevice, fun parse_line/2, #pstate{}),
    file:close(IoDevice),
    PState
  catch
    E:R:Trace ->
      erlang:raise(E, R, Trace)
  end.
-else.
%% 解析csv文件
parse(CsvFile) ->
  {{ok, IoDevice}, CsvFile} = {file:open(CsvFile, [read, compressed]), CsvFile},
  try
    PState = ecsv:process_csv_file_with(IoDevice, fun parse_line/2, #pstate{}),
    file:close(IoDevice),
    PState
  catch
    E:R ->
      Trace = erlang:get_stacktrace(),
      erlang:raise(E, R, Trace)
  end.
-endif.

%% 解析并读取列
parse_and_fetch(CsvFile, Columns) ->
  {ok, #pstate{names = Names, rows = Rows}} = parse(CsvFile),
  [fetch_row(Row, Columns, Names) || #row{} = Row <- Rows].

%%%===================================================================
%%% Local
%%%===================================================================
%% 解析每一行
parse_line({eof}, #pstate{rows = Rows} = State) ->
  State#pstate{rows = lists:reverse(Rows)};

% 第1行类型行
parse_line({newline, Line}, #pstate{line_num = LineN} = State) when LineN =:= 1 ->
  State#pstate{line_num = LineN + 1, types = Line};

% 第2行列名行
parse_line({newline, Line}, #pstate{line_num = LineN} = State) when LineN =:= 2 ->
  State#pstate{line_num = LineN + 1, names = Line};

% 其余数据
parse_line({newline, Line}, #pstate{line_num = LineN, types = Types, names = Names, rows = Rows} = State) ->
  #row{key = Key} = Row = parse_row(Line, Types, Names, LineN - 2),
  ?RETURN_IF(lists:keymember(Key, #row.key, Rows), {"key repeated", Key, LineN}),
  State#pstate{line_num = LineN + 1, rows = [Row | Rows]}.

%% 解析一行数据
parse_row(Cols, Types, Names, RowNum) ->
  ColNum = length(Cols),
  ?RETURN_IF(length(Types) =/= ColNum, {"column and type length not compared", ColNum, length(Types)}),
  ?RETURN_IF(length(Names) =/= ColNum, {"column and name length not compared", ColNum, length(Names)}),
  parse_row2(Cols, Types, Names, #row{row_num = RowNum}).

parse_row2([], [], [], #row{fields = Fields} = Row) ->
  Row#row{fields = lists:reverse(Fields)};

parse_row2([Col | Cols], [Type | Types], [Name | Names], #row{key = Key, fields = Fields} = Row) ->
  Props = init_fprops(Type, #fprops{name = Name}),
  case Props#fprops.is_omit of
    true -> parse_row2(Cols, Types, Names, Row#row{fields = [omit | Fields]});
    false ->
      Field = parse_field(Col, Props),
      NewKey = append_key(Key, Props, Field),
      parse_row2(Cols, Types, Names, Row#row{key = NewKey, fields = [Field | Fields]})
  end.

%% 解析单项数据
init_fprops([], Props) -> Props;
init_fprops("K" ++ Types, Props) ->
  init_fprops(Types, Props#fprops{is_key = true});

init_fprops("A" ++ Types, Props) ->
  init_fprops(Types, Props#fprops{is_array = true});

init_fprops("N" ++ Types, Props) ->
  init_fprops(Types, Props#fprops{is_number = true});

init_fprops("B" ++ Types, Props) ->
  init_fprops(Types, Props#fprops{is_boolean = true});

init_fprops("S" ++ Types, Props) ->
  init_fprops(Types, Props#fprops{is_string = true});

init_fprops("E" ++ Types, Props) ->
  init_fprops(Types, Props#fprops{is_auto = true});

init_fprops("O" ++ Types, Props) ->
  init_fprops(Types, Props#fprops{is_omit = true}).

%% 添加Key
append_key(Key, #fprops{is_key = false}, _Field) -> Key;
append_key(?UNDEF, _FS, Field) -> Field;
append_key(Key, FS, Field) when not is_tuple(Key) ->
  append_key({Key}, FS, Field);
append_key(Key, _FS, Field) ->
  erlang:append_element(Key, Field).

parse_field("", #fprops{is_array = true}) -> [];
parse_field(Val, #fprops{is_array = true} = Props) ->
  Array = string:split(Val, ?ARRAY_DELIM, all),
  Props2 = Props#fprops{is_array = false},
  [parse_field(string:trim(One), Props2) || One <- Array];

parse_field(Val, #fprops{is_string = true}) ->
  Val;

parse_field("", #fprops{is_number = true}) -> 0;
parse_field(Val, #fprops{is_number = true} = Props) ->
  case string:to_float(Val) of
    {error, no_float} ->
      case string:to_integer(Val) of
        {error, Err} ->
          ?RETURN({"field not float or integer", Props#fprops.name, Val, Err});
        {Num, [S|Rest]} when S =:= $E; S =:= $e ->
          {N2, _} = string:to_float(string:concat("1.0E", Rest)),
          N2 * Num;
        {Num, _} -> Num
      end;
    {Num, _} -> Num
  end;

parse_field(Val, #fprops{name = Name, is_boolean = true}) ->
  if
    Val =:= "False"; Val =:= "FALSE"; val =:= "false"; Val =:= "" -> false;
    Val =:= "True"; Val =:= "TRUE"; val =:= "true" -> true;
    true -> ?RETURN({"parse boolean error", Val, Name})
  end;

parse_field("", #fprops{is_auto = true}) -> ?UNDEF;
parse_field(Val, #fprops{is_auto = true} = Props) ->
  % 尝试数组
  case string:find(Val, ?ARRAY_DELIM) =/= nomatch of
    true -> parse_field(Val, Props#fprops{is_array = true});
    false ->
      % 尝试boolean
      case catch parse_field(Val, Props#fprops{is_boolean = true}) of
        Ret when is_boolean(Ret) -> Ret;
        _ ->
          % 尝试integer
          case catch parse_field(Val, Props#fprops{is_number = true}) of
            Number when is_number(Number) -> Number;
            _ -> parse_complex_field(Val, Props) % 混合数据
          end
      end
  end.

parse_complex_field(Val, Props) ->
  case re:run(Val, "^\s*\\[(.+)\\]\s*$", [{capture,[1],list}]) of
    nomatch -> Val;
    {match, [Str]} ->
      Array = string:split(Str, ?COMPLEX_DELIM, all),
      List = [parse_field(string:trim(One), Props) || One <- Array],
      list_to_tuple(List)
  end.

%% 提取行数据
fetch_row(#row{} = Row, ColNames, Names) ->
  [fetch_col(Row, ColName, Names) || ColName <- ColNames].

fetch_col(#row{key = Key}, key, _) -> Key;
fetch_col(#row{row_num = Num}, row, _) -> Num;
fetch_col(#row{}, {value, Value}, _) -> Value;
fetch_col(#row{} = Row, {re, RenameRe}, Names) ->
  fetch_col(Row, {re, RenameRe, []}, Names);
fetch_col(#row{fields = Fields}, {re, RenameRe, Options}, Names) ->
  Cols = [
    case re:run(lists:nth(N, Names), RenameRe, [{capture, all, list}]) of
      {match, [_]} ->  lists:nth(N, Fields);
      {match, [_ | Values]} -> {lists:nth(N, Fields), Values}
    end || N <- lists:seq(1, length(Names)), re:run(lists:nth(N, Names), RenameRe) =/= nomatch
  ],
  [ColVal || ColVal <- Cols, test_col_options(ColVal, Options)];

fetch_col(#row{fields = Fields}, {Func, Arg}, Names) when is_function(Func) ->
  Func(Arg, Names, Fields);

fetch_col(#row{fields = Fields}, Name, Names) when is_list(Name) ->
  N = string:str(Names, [Name]),
  ?RETURN_IF(N =:= 0, {"column not found", Name, Names}),
  lists:nth(N, Fields).

test_col_options(_Val, []) -> true;
test_col_options(Val, [Option| Rest]) ->
  case test_col_option(Val, Option) of
    false -> false;
    true -> test_col_options(Val, Rest)
  end.

test_col_option(Val, {skip, Val2}) -> Val =/= Val2;
test_col_option(Val, {test, Fun}) -> Fun(Val).