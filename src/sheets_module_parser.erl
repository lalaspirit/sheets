%%%-------------------------------------------------------------------
%%% @author Nero.li
%%% @copyright (C) 2021, <Lilith>
%%% @doc
%%%   解析模块文件，生成sheets_generate的选项
%%% @end
%%% Created : 04. 三月 2021 15:39
%%%-------------------------------------------------------------------
-module(sheets_module_parser).
-author("lifan").

%% API
-export([file/2, parse/1]).

-record(chunk, {
  type,
  name,
  options,
  codes
}).


%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc 解析文件，更新options
%% @spec parse(FileName, Options) -> Options
%%  When FileName = string()
%%    Options = [Option]
%%--------------------------------------------------------------------
file(FileName, Options) ->
  {ok, Chunks} = parse(FileName),
  update_options(Options, Chunks).


%%--------------------------------------------------------------------
%% @doc 解析文件
%% @spec parse(FileName, Options) -> Options
%%  When FileName = string()
%%    Options = [Option]
%%--------------------------------------------------------------------
parse(FileName) ->
  {ok, Binary} = file:read_file(FileName),
  List = unicode:characters_to_list(Binary),
  {ok, Tokens, _} = sheets_module_lex:string(List),
  % io:format("parse ~p, tokens: ~p~n", [FileName, Tokens]),
  sheets_module_yacc:parse(Tokens).


update_options(Options, []) -> Options;
update_options(Options, [Chunk| Rest]) ->
  NewOptions = parse_chunk(Options, Chunk),
  update_options(NewOptions, Rest).


parse_chunk(Options, #chunk{type = head, options = HeadOptions}) ->
  lists:ukeymerge(1, HeadOptions, Options);

parse_chunk(Options, #chunk{type = section, name = "Custom", options = SectionOptions} = Chunk) ->
  Options2 = parse_chunk_section_options(Options, SectionOptions),
  parse_section(Options2, Chunk);

parse_chunk(Options, _) -> Options.

parse_chunk_section_options(Options, []) -> Options;
parse_chunk_section_options(Options, [HeadOption| Rest]) ->
  Options2 = parse_chunk_section_option(Options, HeadOption),
  parse_chunk_section_options(Options2, Rest).

parse_chunk_section_option(Options, {"export", List, _}) ->
  Exists = proplists:get_value(export, Options, []),
  Exists2 = lists:umerge([Exists, List]),
  lists:keystore(export, 1, Options, {export, Exists2});

parse_chunk_section_option(Options, {"override", List, _}) ->
  Exists = proplists:get_value(override, Options, []),
  Exists2 = lists:umerge([Exists, List]),
  lists:keystore(override, 1, Options, {override, Exists2});

parse_chunk_section_option(Options, {"include", List, _}) ->
  Exists = proplists:get_value(include, Options, []),
  Exists2 = lists:umerge([Exists, List]),
  lists:keystore(include, 1, Options, {include, Exists2}).


parse_section(Options, #chunk{name = Name, options = SectionOptions, codes = Codes}) ->
  SOptionList = [S || {_, _, S} <- SectionOptions],
  SCodeList = [Code || {code_line, _N, Code} <- Codes],
  lists:keystore(Name, 1, Options, {Name, {SOptionList, SCodeList}}).






