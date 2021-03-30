Definitions.

StartTag = %%%
Spaces = [\t\s]*
EndLine = [\r\n]?
Rules.

{StartTag}---+{EndLine} : {token, {head_line, TokenLine}}.

{StartTag}{Spaces}@.+{EndLine} : {token, {head_option, TokenLine, parse_head_option(TokenChars)}}.

{StartTag}===+{EndLine} : {token, {section_line, TokenLine}}.

{StartTag}{Spaces}-.+{EndLine} : {token, {section_option, TokenLine, parse_section_option(TokenChars)}}.

{StartTag}{Spaces}.+{EndLine} : {token, {name, TokenLine, parse_name(TokenChars)}}.

.*{EndLine} : {token, {code_line, TokenLine, TokenChars}}.


Erlang code.

parse_head_option(TokenChars) ->
    S = get_uf8_binary(TokenChars),
    {Key, Value} = case re:run(S, "@([a-zA-Z][0-9a-zA-Z_]+)\s+(.+)", [{capture,[1,2],binary}]) of
        {match, [_Key, _Value]} -> {_Key, _Value};
        _ ->
            {match, [_Key]} = re:run(S, "@([a-zA-Z][0-9a-zA-Z_]+).+", [{capture,[1],binary}]),
            {_Key, ""}
    end,
    {get_unicode(Key), get_unicode(Value)}.

parse_section_option(TokenChars) ->
    {match, [Key, Value]} = re:run(get_uf8_binary(TokenChars), "-([a-z][0-9a-zA-Z_]+)\\((.+)\\)\\.?", [{capture,[1,2],binary}]),
    Value2 = trim(get_unicode(Value), leading, "["),
    Value3 = trim(Value2, trailing, "]"),
    ValueList = string:tokens(Value3, ","),
    ValueList2 = [trim(V) || V <- ValueList],
    {get_unicode(Key), ValueList2, TokenChars}.


parse_name(TokenChars) ->
    {match, [Name]} = re:run(get_uf8_binary(TokenChars), "%%%\s*(.+)", [{capture,[1],binary}]),
    trim(get_unicode(Name)).


get_uf8_binary(String) ->
    unicode:characters_to_binary(String).

get_unicode(Binary) when is_binary(Binary) ->
    unicode:characters_to_list(Binary);

get_unicode(Binary) -> get_unicode(iolist_to_binary(Binary)).


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
