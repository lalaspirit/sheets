Definitions.

Digits = [0-9]+
Chars = [a-zA-Z]+
Atoms = [a-z][0-9a-zA-Z_]*
Variables = [A-Z_][0-9a-zA-Z_]*
Floats = (\+|-)?[0-9]+\.[0-9]+((E|e)(\+|-)?[0-9]+)?
String = "((\\")|.)+"
Tuple = {[^\r\n\{\}]*}
List = \[[^\r\n\[\]]*\]
NotS = [^\s\r\n]+
Words = [\x{4e00}-\x{9fa5}]+
Fun = \s*(fun)\s*[(].+(end)
Macro = \s*[?][a-zA-Z][0-9a-zA-Z_]*

Term = ({Atoms}|{Digits}|{Floats}|{String}|{Tuple}|{List})
FileName = ({Atoms}|{Digits}|{Variables})\.({Atoms}|{Digits}|{Variables})
Param = ({Variables}|{Atoms}|{Digits}|{Floats}|{String}|{Tuple}|{List}|{Words}|{FileName}|{Fun}|{Macro})
Spaces = [\t\s]*
Equal = {Spaces}[=]{Spaces}
EndField = {Spaces}[,]
Comment = %%?{Spaces}
SectionComment = %%%.+
EndLine = {Spaces}[\r\n]+
NoneLine = [\r\n]+

Rules.

{SectionComment} : skip_token.

{Comment}.*@{Atoms}.* : {token, {sheet_def_comment, TokenLine, parse_comment(TokenChars)}, push_back_method(TokenChars)}.

{Comment}.* : {token, {sheet_def_comment, TokenLine, parse_comment(TokenChars)}}.

[-]record[(]{Spaces}{Atoms}[,]{Spaces}[{]{EndLine} : {token, {sheet_def_record, TokenLine, parse_sheet(TokenChars)}}.

[-]{Atoms}.+ : skip_token.

{Spaces}{Atoms}({EndField})? : {token, {sheet_def_field, TokenLine, parse_field(TokenChars)}}.

{Spaces}{Atoms}{Equal}{Term}({EndField})? : {token, {sheet_def_field, TokenLine, parse_field(TokenChars)}}.

[@]{Atoms} : {token, {sheet_def_method, TokenLine, parse_method(TokenChars)}}.

[(] : {token, {sheet_def_param_start, TokenLine}}.

{String} : {token, {sheet_def_param, TokenLine, parse_param_from_str(TokenChars)}, push_back_param_str(TokenChars)}.

{Param} : {token, {sheet_def_param, TokenLine, parse_param(TokenChars)}}.

[,] : {token, {sheet_def_param_delimiter, TokenLine}}.

[)] : {token, {sheet_def_param_end, TokenLine}}.

[-][>]{Spaces}[^|\r\n]+[|] : {token, {sheet_def_logic, TokenLine, parse_logic(TokenChars)}, "|"}.

[-][>]{Spaces}[^|\r\n]+ : {token, {sheet_def_logic, TokenLine, parse_logic(TokenChars)}}.

[|]{Spaces}{Atoms} : {token, {sheet_def_modifier, TokenLine, parse_modifier(TokenChars)}}.

[}][)][.] : {token, {sheet_def_end, TokenLine}}.

{NoneLine} : skip_token.

\s+ : skip_token.



Erlang code.

parse_sheet(TokenChars) ->
    {match, [Name]} = re:run(get_uf8_binary(TokenChars), "\\(\\s*([a-z][0-9a-zA-Z_]*)\\s*,", [{capture,[1],list}]),
    list_to_atom(Name).

parse_field(TokenChars) ->
    TokenChars2 = trim(TokenChars, trailing, ","),
    [Name0|Rest] = string:tokens(TokenChars2, "="),
    Name = trim(Name0),
    Default = case trim(Rest) of
        [] -> undefined;
        Val -> eval(Val ++ ".")
    end,
    {list_to_atom(Name), Default}.

parse_comment(TokenChars) ->
    TokenChars2 = trim(TokenChars),
    TokenChars3 = trim(TokenChars2, leading, "%"),
    trim(TokenChars3, leading).

parse_method(TokenChars) ->
    [$@|TokenChars2] = trim(TokenChars),
    list_to_atom(TokenChars2).

parse_param_from_str(TokenChars) ->
    S = fetch_param_string(TokenChars),
    %io:format("parse_param_from_str ~p, ~p~n", [S, TokenChars]),
    S.

push_back_param_str(TokenChars) ->
    S = fetch_param_string(TokenChars),
    TokenChars2 = trim(TokenChars, leading, S),
    %io:format("push_back_param_str ~p, ~p~n", [S, TokenChars2]),
    TokenChars2.

fetch_param_string(TokenChars) ->
    {match, [SubStr]} = re:run(TokenChars, "(\"((\\\")|.)+\")", [{capture, first, list}, ungreedy]),
    SubStr.

parse_param(TokenChars) ->
    trim(TokenChars).

parse_modifier(TokenChars) ->
    TokenChars2 = trim(TokenChars),
    [$||TokenChars3] = trim(TokenChars2),
    list_to_atom(trim(TokenChars3)).

parse_logic(TokenChars) ->
    TokenChars2 = trim(TokenChars),
    TokenChars3 = trim(TokenChars2, leading, "->"),
    TokenChars4 = trim(TokenChars3, trailing, "|"),
    trim(TokenChars4).

push_back_method(TokenChars) ->
    {match, [Binary]} = re:run(get_uf8_binary(TokenChars), "(@[a-z][0-9a-zA-Z_]*.*)", [{capture,[1],binary}]),
    get_unicode(Binary).

get_uf8_binary(String) ->
    unicode:characters_to_binary(String).

get_unicode(Binary) when is_binary(Binary) ->
    unicode:characters_to_list(Binary);

get_unicode(Binary) -> get_unicode(iolist_to_binary(Binary)).

eval(S) ->
  {ok, Scanned, _} = erl_scan:string(S),
  {ok, Parsed} = erl_parse:parse_exprs(Scanned),
  {value, Value, _} = erl_eval:exprs(Parsed,[]),
  Value.
  

otp_ver() ->
    {match, [VerStr]} = re:run(erlang:system_info(otp_release), "^R?([0-9]+)", [{capture,[1],list}]),
    list_to_integer(VerStr).

has_string_trim() ->
    otp_ver() >= 20.

trim(S) -> trim(S, both).
trim(S, D) ->
    case has_string_trim() of
        true -> string:trim(S, D);
        false -> my_trim(S, D, " ")
    end.

trim(S, D, Chars) ->
    case has_string_trim() of
        true -> string:trim(S, D, Chars);
        false -> my_trim(S, D, Chars)
    end.

my_trim([S], D, List) when is_list(S) ->
    my_trim(S, D, List);

my_trim(S, D, [Sep]) ->
    D2 = case D of
        leading -> left;
        trailing -> right;
        both -> both
    end,
    string:strip(S, D2, Sep);
my_trim(S, leading, List) ->
    L = length(List),
    case string:substr(S, 1, L) of
        List -> string:substr(S, L + 1);
        _ -> S
    end.
