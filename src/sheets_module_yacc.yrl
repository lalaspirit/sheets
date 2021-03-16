Header "%% Copyright (C)"
"%% @private"
"%% @Author Nero.li".

Nonterminals
chunks chunk head_options section_options code_lines.

Terminals
head_line name head_option section_line section_option code_line.

Rootsymbol chunks.

chunks -> chunk : ['$1'].
chunks -> chunk chunks : ['$1' | '$2'].

chunk -> head_line head_options head_line code_lines : save_chunk(head, "HEAD", '$2', '$4').
chunk -> section_line name section_options section_line code_lines : save_chunk(section, '$2', '$3', '$5').
chunk -> section_line name section_line code_lines : save_chunk(section, '$2', [], '$4').

head_options -> head_option : ['$1'].
head_options -> head_option head_options : ['$1'| '$2'].

section_options -> section_option : ['$1'].
section_options -> section_option section_options : ['$1' | '$2'].

code_lines -> code_line : ['$1'].
code_lines -> code_line code_lines : ['$1' | '$2'].


Erlang code.

save_chunk(Type, {name, _, Name}, Options, Codes) ->
    save_chunk(Type, Name, Options, Codes);
save_chunk(Type, Name, Options, Codes) ->
    Options2 = [Value || {_, _, Value} <- Options],
    {chunk, Type, Name, Options2, Codes}.

