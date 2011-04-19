-module (mustache_simple_tests).
-compile (export_all).
-include_lib ("eunit/include/eunit.hrl").

body_as_string_test () ->
  Ctx = dict:from_list([{planet, "Earth"}]),
  Result = mustache:render("Hello, {{planet}}", Ctx),
  ?assertEqual("Hello, Earth", Result).

body_as_binary_test () ->
  Ctx = dict:from_list([{planet, "Earth"}]),
  Result = mustache:render(<<"Hello, {{planet}}">>, Ctx),
  ?assertEqual("Hello, Earth", Result).

setup () ->
  {ok, CurrDir} = file:get_cwd(),
  % get the "test" directory
  TestDir       = filename:join([filename:dirname(code:which(?MODULE)), "..", "test"]),
  ?assert(filelib:is_dir(TestDir)),
  % copy any templates files to current dir if current is not the same as "test"
  case TestDir of
    CurrDir -> ok;
    TestDir ->
      lists:foreach(fun (File) ->
                        Target = filename:join(CurrDir, filename:basename(File)),
                        ?assertMatch({ok, _}, file:copy(File, Target))
                    end,
                    filelib:wildcard(filename:join(TestDir, "*.mustache")) 
                    ++ filelib:wildcard(filename:join(TestDir, "*.ctx")))
  end,
  {CurrDir, TestDir}.
  
tear_down ({Dir, Dir}) ->
  ok;
tear_down ({CurrDir, _TestDir}) ->
  % delete any template file
  lists:foreach(fun (File) ->
                    ?assertEqual(ok, file:delete(File))
                end,
                filelib:wildcard(filename:join(CurrDir, "*.mustache"))
                ++ filelib:wildcard(filename:join(CurrDir, "*.ctx"))).

-define (TESTS(Module, Prefix), [F || {F, 1} <- Module:module_info(exports),
                                      lists:sublist(atom_to_list(F), 1, 5) =:= Prefix]).

all_test_ () ->
  all_tests(?MODULE).

all_tests (Module) ->
  {foreach, fun setup/0, fun tear_down/1,
   [fun (Ctx) ->
        {atom_to_list(Fun), fun () -> Module:Fun(Ctx) end}
    end || Fun <- ?TESTS(Module, "test ")]}.

'test view module' (_) ->
  Result = mustache:render(mustache_test_view),
  ?assertEqual("Hello, Earth", Result).
  
  
