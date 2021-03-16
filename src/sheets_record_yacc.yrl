Header "%% Copyright (C)"
"%% @private"
"%% @Author Nero.li".

Nonterminals
sheets sheet sheet_record sheet_metas sheet_meta sheet_fields sheet_field
sheet_meta_method sheet_meta_method_main sheet_meta_method_modifiers sheet_meta_method_modifier
sheet_meta_method_params sheet_meta_method_param sheet_meta_method_logic.

Terminals
sheet_def_record sheet_def_end sheet_def_field sheet_def_comment
sheet_def_method sheet_def_param_start sheet_def_param sheet_def_param_delimiter sheet_def_param_end
sheet_def_logic sheet_def_modifier.

Rootsymbol sheets.

sheet_metas -> sheet_meta : ['$1'].
sheet_metas -> sheet_meta sheet_metas : ['$1' | '$2'].

sheet_meta -> sheet_def_comment sheet_meta_method : save_meta('$1', '$2').
sheet_meta -> sheet_def_comment : save_meta('$1').

sheet_meta_method -> sheet_meta_method_main sheet_meta_method_modifiers : save_meta_method('$1', '$2').
sheet_meta_method -> sheet_meta_method_main : save_meta_method('$1').

sheet_meta_method_main -> sheet_def_method sheet_def_param_start sheet_meta_method_params sheet_def_param_end sheet_meta_method_logic : save_method_main('$1', '$3', '$5').
sheet_meta_method_main -> sheet_def_method sheet_def_param_start sheet_meta_method_params sheet_def_param_end : save_method_main('$1', '$3').
sheet_meta_method_main -> sheet_def_method : save_method_main('$1').

sheet_meta_method_params -> sheet_meta_method_param : ['$1'].
sheet_meta_method_params -> sheet_meta_method_param sheet_meta_method_params : ['$1' | '$2'].

sheet_meta_method_param -> sheet_def_param : save_method_param('$1').
sheet_meta_method_param -> sheet_def_param_delimiter sheet_def_param : save_method_param('$2').

sheet_meta_method_modifiers -> sheet_meta_method_modifier : ['$1'].
sheet_meta_method_modifiers -> sheet_meta_method_modifiers sheet_meta_method_modifiers : '$1' ++ '$2'.

sheet_meta_method_modifier -> sheet_def_modifier sheet_def_param_start sheet_meta_method_params sheet_def_param_end sheet_meta_method_logic: save_method_modifier('$1', '$3', '$5').
sheet_meta_method_modifier -> sheet_def_modifier sheet_def_param_start sheet_meta_method_params sheet_def_param_end : save_method_modifier('$1', '$3').
sheet_meta_method_modifier -> sheet_def_modifier : save_method_modifier('$1').

sheet_meta_method_logic -> sheet_def_logic : save_method_logic('$1').

sheets -> sheet : ['$1'].
sheets -> sheet sheets : ['$1' | '$2'].

sheet -> sheet_metas sheet_record : save_sheet('$1', '$2').
sheet -> sheet_record : save_sheet('$1').

sheet_record -> sheet_def_record sheet_fields sheet_def_end : save_record('$1', '$2').

sheet_fields -> sheet_field : ['$1'].
sheet_fields -> sheet_field sheet_fields : ['$1' | '$2'].

sheet_field -> sheet_def_field sheet_metas : save_field('$1', '$2').
sheet_field -> sheet_def_field : save_field('$1').


Erlang code.

save_sheet(Record) -> save_sheet([], Record).
save_sheet(Metas, Record) -> {sheet, Metas, Record}.

save_record({sheet_def_record,_,Name}, Fields) -> {record, Name, Fields}.

save_field(Field) -> save_field(Field, []).
save_field({sheet_def_field,_,{Name, Default}}, Metas) -> {field, Name, Default, Metas}.

save_meta(Define) -> save_meta(Define, undefined).
save_meta({sheet_def_comment,_,Comment}, Method) -> {meta, Comment, Method}.

save_meta_method(Main) -> save_meta_method(Main, []).
save_meta_method(Main, Modifiers) -> {meta_method, Main, Modifiers}.

save_method_main(Define) -> save_method_main(Define, []).
save_method_main(Define, Params) -> save_method_main(Define, Params, undefined).
save_method_main({sheet_def_method,_,Name}, Params, Logic) -> {method_main, Name, Params, Logic}.

save_method_param({sheet_def_param,_,Value}) -> Value.

save_method_modifier(Define) -> save_method_modifier(Define, []).
save_method_modifier(Define, Params) -> save_method_modifier(Define, Params, undefined).
save_method_modifier({sheet_def_modifier,_,Name}, Params, Logic) -> {method_modifier, Name, Params, Logic}.

save_method_logic({sheet_def_logic,_,Content}) -> {method_logic, Content}.