%% @doc Mustche Template Rendering Engine.
%% == The MIT License ==
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%% == User Guide ==
%%
%% See the README at [http://github.com/mojombo/mustache.erl] for additional
%% documentation and usage examples or the overview.
%%
%% @copyright 2009 Tom Preston-Werner <tom@mojombo.com>

-module(mustache).  %% v0.1.0
-author("Tom Preston-Werner").
-author("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").

-export([compile/1, compile/2, compile/3,
         render/1, render/2, render/3, render/4,
         get/2, get/3, escape/1]).

-export([ctx_file_reader/1]).

-record(mstate, {mod        = undefined,
                 section_re = undefined,
                 tag_re     = undefined}).

-type mstate() :: #mstate{}.

%% @doc Returns compiled template.
%%      You may pass the template contents as is or pass a module name
%%      which path basename will provide the template filename (the template
%%      should have ".mustache" extension).

-type ctempl () :: function(). % Compiled template.
-spec compile (list() | binary() | module()) -> ctempl().

compile(Body) when is_binary(Body) ->
  compile(erlang:binary_to_list(Body));
compile(Body) when is_list(Body) ->
  State = #mstate{},
  compile_template(Body, State);
compile(Mod) when is_atom(Mod) ->
  TemplatePath = template_path(Mod),
  compile(Mod, TemplatePath).

%% @doc Returns compiled template which filename is explicity given.

-spec compile (module(), string()) -> ctempl().

compile(Mod, File) when is_atom(Mod)
                        andalso is_list(File) ->
  compile(Mod, fun file:read_file/1, File).

%% @doc Returns compiled template which contents is read by the file reader.
%%      The file reader SHOULD use provided template id to get its contents.

-type template_id () :: any(). % Id of a template (which may be a filename).
-type file_reader () :: fun ((template_id()) -> {ok, list() | binary()}). % Function that reads a template or context "file".
-spec compile (module(), file_reader(), template_id()) -> ctempl().

compile(Mod, FileReader, TemplateId) when is_atom(Mod)
                                    andalso is_function(FileReader) ->
  code:purge(Mod),
  code:load_file(Mod),
  {ok, TemplateBin} = FileReader(TemplateId),
  State = #mstate{mod = Mod},
  compile_template(TemplateBin, State).

%% @private

-spec compile_template (string() | binary(), mstate()) -> ctempl().

compile_template (Body, State) ->
  Template = re:replace(Body, "\"", "\\\\\"", [global, {return,list}]),
  CompiledTemplate = pre_compile(Template, State),
  % io:format("~p~n~n", [CompiledTemplate]),
  % io:format(CompiledTemplate ++ "~n", []),
  {ok, Tokens, _} = erl_scan:string(CompiledTemplate),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  Bindings = erl_eval:new_bindings(),
  {value, Fun, _} = erl_eval:expr(Form, Bindings),
  Fun.

%% @doc Renders the template referenced by the module name. The default context
%%      is empty.
%% @see render/2

-spec render (module()) -> string().

render(Mod) when is_atom(Mod) ->
  TemplatePath = template_path(Mod),
  render(Mod, TemplatePath).

%% @doc Renders a template body (given explicity or as a filename
%%       or using a file reader) using provided context.
%% @see compile/3
%% @see render/3
%% @see render/4

-type tbody () :: binary() | list(). % Template contents.
-type ctx   () :: dict() | ctx_file | {ctx_reader, fun((any()) -> {ok, [{atom(), string()}]})}. % Rendering context.
-spec render (tbody(), dict()) -> string();
             (tbody(), string()) -> string();
             (module(), ctx()) -> string();
             (module(), string()) -> string();
             (module(), {reader, file_reader()}) -> string();
             (module(), ctempl()) -> string().

render(Body, FileName) when (is_list(Body) orelse is_binary(Body))
                            andalso is_list(FileName) ->
  {ok, [Props]} = file:consult(FileName),
  render(Body, parse_ctx(Props));
render(Body, Ctx) when (is_list(Body) orelse is_binary(Body))
                       andalso (not is_atom(Ctx)) ->
  TFun = compile(Body),
  render(undefined, TFun, Ctx);
render(Mod, File) when is_atom(Mod)
                       andalso is_list(File) ->
  render(Mod, fun file:read_file/1, File, dict:new());
render(Mod, ctx_file) when is_atom(Mod) ->
  render(Mod, {ctx_reader, fun ?MODULE:ctx_file_reader/1});
render(Mod, {ctx_reader, CtxReader}) when is_atom(Mod)
                                          andalso is_function(CtxReader) ->
  {ok, Props} = CtxReader(Mod),
  render(Mod, parse_ctx(Props));
render(Mod, {reader, FileReader}) when is_atom(Mod)
                                       andalso is_function(FileReader) ->
  render(Mod, FileReader, template_path(Mod), dict:new());
render(Mod, CompiledTemplate) when is_atom(Mod)
                                   andalso is_function(CompiledTemplate) ->
  render(Mod, CompiledTemplate, dict:new());
render(Mod, Ctx) when is_atom(Mod) ->
  render(Mod, template_path(Mod), Ctx).

%% @doc Renders a template referenced by a module name using file reader
%%      and a context.
%% @see render/4

-spec render (module(), {reader, file_reader()}, ctx()) -> string();
             (module(), string(), string()) -> string();
             (module(), string(), ctx()) -> string();
             (module(), ctempl(), ctx()) -> string().

render(Mod, FileName, CtxFile) when is_atom(Mod)
                                    andalso is_list(FileName)
                                    andalso is_list(CtxFile) ->
  render(Mod, FileName, {ctx_reader, fun (_) ->
                                         {ok, [C]} = file:consult(CtxFile),
                                         {ok, C}
                                     end});
render(Mod, FileName, ctx_file) when is_atom(Mod) andalso is_list(FileName) ->
  render(Mod, FileName, {ctx_reader, fun ?MODULE:ctx_file_reader/1});
render(Mod, FileName, {ctx_reader, CtxReader}) when is_atom(Mod)
                                                    andalso is_list(FileName)
                                                    andalso is_function(CtxReader) ->
  {ok, Props} = CtxReader(Mod),
  render(Mod, FileName, parse_ctx(Props));
render(Mod, Reader = {reader, FileReader}, ctx_file) when is_atom(Mod) 
                                                 andalso is_function(FileReader) ->
  render(Mod, Reader, {ctx_reader, fun ?MODULE:ctx_file_reader/1});
render(Mod, {reader, FileReader}, {ctx_reader, CtxReader}) when is_atom(Mod)
                                                                andalso is_function(FileReader)
                                                                andalso is_function(CtxReader) ->
  {ok, Props} = CtxReader(Mod),
  render(Mod, FileReader, template_path(Mod), parse_ctx(Props));
render(Mod, {reader, FileReader}, Ctx) when is_atom(Mod) 
                                            andalso is_function(FileReader) ->
  render(Mod, FileReader, template_path(Mod), Ctx);
render(Mod, CompiledTemplate, ctx_file) when is_atom(Mod) 
                                        andalso is_function(CompiledTemplate) ->
  render(Mod, CompiledTemplate, {ctx_reader, fun ?MODULE:ctx_file_reader/1});
render(Mod, CompiledTemplate, {ctx_reader, CtxReader}) when is_atom(Mod) 
                                                            andalso is_function(CompiledTemplate)
                                                            andalso is_function(CtxReader) ->
  {ok, Props} = CtxReader(Mod),
  render(Mod, CompiledTemplate, parse_ctx(Props));
render(Mod, CompiledTemplate, Ctx) when is_atom(Mod) 
                                        andalso is_function(CompiledTemplate) ->
  Ctx2 = dict:store('__mod__', Mod, Ctx),
  lists:flatten(CompiledTemplate(Ctx2));
render(Mod, FileName, Ctx) when is_atom(Mod) andalso is_list(FileName) ->
  render(Mod, fun file:read_file/1, FileName, Ctx).

%% @doc Renders a template referenced by a module name. The template content
%%      is provided by a file reader which uses the template id to get the
%%      content. The context is explicit given.
%% @equiv render(Mod, compile(Mod, FileReader, TemplateId), Ctx)
%% @see compile/3
%% @see render/3

-spec render (module(), file_reader(), template_id(), ctx()) -> string().

render(Mod, FileReader, TemplateId, Ctx) when is_atom(Mod) 
                                              andalso is_function(FileReader) ->
  CompiledTemplate = compile(Mod, FileReader, TemplateId),
  render(Mod, CompiledTemplate, Ctx).

%% =============== local functions =====================

-spec ctx_file_reader (string()) -> {ok, [{atom(), list() | binary()}]}.

ctx_file_reader (Mod) when is_list(Mod) ->
  ctx_file_reader(list_to_existing_atom(Mod));
ctx_file_reader (Mod) when is_atom(Mod) ->
  {ok, [Props]} = file:consult(template_path(Mod, ".ctx")),
  {ok, Props}.

-type ctx_list () :: [{atom(), string() | [ctx_list()]}].
-spec parse_ctx (ctx_list()) -> dict().

parse_ctx (List) when is_list(List) ->
  parse_ctx_loop(List, dict:new()).

-spec parse_ctx_loop (ctx_list(), dict()) -> dict().

parse_ctx_loop ([], Dict) ->
  Dict;
parse_ctx_loop ([{Key, Inner = [[{K, _}|_]|_]}|List], Dict) when is_atom(K) ->
  parse_ctx_loop(List, dict:store(Key, [parse_ctx_loop(L, dict:new()) || L <- Inner], Dict));
parse_ctx_loop ([{Key, Inner = [{K, _}|_]}|List], Dict) when is_atom(K) ->
  parse_ctx_loop(List, dict:store(Key, [parse_ctx_loop(Inner, dict:new())], Dict));
parse_ctx_loop ([{Key, Value}|List], Dict) ->
  parse_ctx_loop(List, dict:store(Key, Value, Dict)).

-spec pre_compile (string(), mstate()) -> string().

pre_compile(T, State) ->
  SectionRE = "\{\{\#([^\}]*)}}\s*(.+?){{\/\\1\}\}\s*",
  {ok, CompiledSectionRE} = re:compile(SectionRE, [dotall]),
  TagRE = "\{\{(#|=|!|<|>|\{)?(.+?)\\1?\}\}+",
  {ok, CompiledTagRE} = re:compile(TagRE, [dotall]),
  State2 = State#mstate{section_re = CompiledSectionRE, tag_re = CompiledTagRE},
  "fun(Ctx) -> " ++
    "CFun = fun(A, B) -> A end, " ++
    compiler(T, State2) ++ " end.".

-spec compiler (string(), mstate()) -> string().

compiler(T, State) ->
  compiler(T, State, 0).

-spec compiler (string(), mstate(), integer()) -> string().

compiler(T, State, Level) ->
  Res = re:run(T, State#mstate.section_re),
  case Res of
    {match, [{M0, M1}, {N0, N1}, {C0, C1}]} ->
      Front = string:substr(T, 1, M0),
      Back  = string:substr(T, M0 + M1 + 1),
      Name  = string:substr(T, N0 + 1, N1),
      Content = string:substr(T, C0 + 1, C1),
      "[" ++ compile_tags(Front, State) ++
        " | [" ++ compile_section(Name, Content, State, Level) ++
        " | [" ++ compiler(Back, State, Level) ++ "]]]";
    nomatch ->
      compile_tags(T, State)
  end.

-spec compile_section (string(), string(), mstate(), integer()) -> string().

compile_section(Name, Content, State = #mstate{mod = Mod}, Level) ->
  %% Mod = State#mstate.mod,
  Result = compiler(Content, State, Level+1),
  ListId = "List" ++ integer_to_list(Level),
  %% "begin
  %%    Sub = fun(Ctx) -> " ++ Result ++ " end,"
  %% "  mustache:section_callback(" ++ Name ++ ", Ctx, " ++ atom_to_list(Mod) ++ ", Sub ++ )"
  %% "end".
  "fun() -> "
    "case mustache:get(" ++ Name ++ ", Ctx, " ++ atom_to_list(Mod) ++ ") of "
      %% "true -> " ++
      %%   Result ++ "; "
      %% "false -> "
      %%   "[]; "
      "[] -> [];"          
      ++ ListId ++ " when is_list(" ++ ListId ++ ") -> " ++
        "[fun(Ctx) -> " ++ Result ++ " end(dict:merge(CFun, SubCtx, Ctx)) || SubCtx <- " ++ ListId ++ "]; "
      "Else -> "
        "exit({template, lists:flatten(io_lib:format(\"Bad context for ~p: ~p\", [" ++ Name ++ ", Else]))}) "
    "end "
  "end()".

-spec compile_tags (string(), mstate()) -> string().

compile_tags(T, State) ->
  Res = re:run(T, State#mstate.tag_re),
  case Res of
    {match, [{M0, M1}, K, {C0, C1}]} ->
      Front = string:substr(T, 1, M0),
      Back = string:substr(T, M0 + M1 + 1),
      Content = string:substr(T, C0 + 1, C1),
      Kind = tag_kind(T, K),
      Result = compile_tag(Kind, Content, State),
      "[\"" ++ Front ++ 
        "\" | [" ++ Result ++ 
        " | " ++ compile_tags(Back, State) ++ "]]";
    nomatch ->
      "[\"" ++ T ++ "\"]"
  end.

-spec tag_kind (string(), {integer(), integer()}) -> string().

tag_kind(_T, {-1, 0}) ->
  none;
tag_kind(T, {K0, K1}) ->
  string:substr(T, K0 + 1, K1).

-spec compile_tag (none | string(), string(), mstate()) -> string().

compile_tag(none, Content, #mstate{mod = Mod}) ->
  "mustache:escape(mustache:get(" ++ Content ++ ", Ctx, " ++ atom_to_list(Mod) ++ "))";
compile_tag("{", Content, #mstate{mod = Mod}) ->
  "mustache:get(" ++ Content ++ ", Ctx, " ++ atom_to_list(Mod) ++ ")";
compile_tag("!", _Content, _State) ->
  "[]".

%% @private

-spec template_path (module()) -> string().

template_path(Mod) ->
  template_path(Mod, ".mustache").

%% @private

-spec template_path (module(), string()) -> string().

template_path(Mod, Extension) ->
  ModPath = code:which(Mod),
  re:replace(ModPath, "\.beam$", Extension, [{return, list}]).

%% @private

-spec get (string() | atom(), dict()) -> string() | [].

get(Key, Ctx) when is_list(Key) ->
  {ok, Mod} = dict:find('__mod__', Ctx),
  get(list_to_atom(Key), Ctx, Mod);
get(Key, Ctx) ->
  {ok, Mod} = dict:find('__mod__', Ctx),
  get(Key, Ctx, Mod).

%% @private

-spec get (string() | atom(), dict(), module()) -> string() | [].

get(Key, Ctx, Mod) when is_list(Key) ->
  get(list_to_atom(Key), Ctx, Mod);
get(Key, Ctx, Mod) ->
  % io:format("Key = ~p; Ctx = ~p~n", [Key, Ctx]),
  case dict:find(Key, Ctx) of
    {ok, Val} ->
      % io:format("From Ctx {~p, ~p}~n", [Key, Val]),
      to_s(Val);
    error ->
      case erlang:function_exported(Mod, Key, 1) of
        true ->
          Val = to_s(apply(Mod, Key, [Ctx])),
          % io:format("From Mod/1 {~p, ~p}~n", [Key, Val]),
          Val;
        false ->
          case erlang:function_exported(Mod, Key, 0) of
            true ->
              Val = to_s(apply(Mod, Key, [])),
              % io:format("From Mod/0 {~p, ~p}~n", [Key, Val]),
              Val;
            false ->
              []
          end
      end
  end.

-spec to_s (integer() | float() | boolean() | atom() | list()) -> string().

to_s(Val) when is_integer(Val) ->
  integer_to_list(Val);
to_s(Val) when is_float(Val) ->
  io_lib:format("~.2f", [Val]);
to_s(Val) when is_boolean(Val) ->
  Val;
to_s(Val) when is_atom(Val) ->
  atom_to_list(Val);
to_s(Val) ->
  Val.

%% @private

-spec escape (string()) -> iolist().

escape(HTML) ->
  escape(HTML, []).

%% @private

escape([], Acc) ->
  lists:reverse(Acc);
escape(["<" | Rest], Acc) ->
  escape(Rest, lists:reverse("&lt;", Acc));
escape([">" | Rest], Acc) ->
  escape(Rest, lists:reverse("&gt;", Acc));
escape(["&" | Rest], Acc) ->
  escape(Rest, lists:reverse("&amp;", Acc));
escape([X | Rest], Acc) ->
  escape(Rest, [X | Acc]).
