-module (custom_context_reader_tests).
-compile (export_all).
-include_lib ("eunit/include/eunit.hrl").

setup () ->
  CurrDir = file:get_cwd(),
  % get the "test" directory
  TestDir = filename:join([filename:dirname(code:which(?MODULE)), "..", "test"]),
  ?assert(filelib:is_dir(TestDir)),
  ?assertEqual(ok, file:set_cwd(TestDir)),
  % copy any templates files to current dir if current is not the same as "test"
  case TestDir of
    CurrDir -> ok;
    TestDir ->
      lists:foreach(fun (File) ->
                        file:copy(File, CurrDir)
                    end, filelib:wildcard(filename:join(TestDir, "*.mustache")))
  end,
  {CurrDir, TestDir}.
  
tear_down ({Dir, Dir}) ->
  ok;
tear_down ({CurrDir, _TestDir}) ->
  % delete any template file
  lists:foreach(fun (File) ->
                    file:remove(File)
                end, filelib:wildcard(filename:join(CurrDir, "*.mustache"))).  

-define (TESTS, [F || {F, 1} <- ?MODULE:module_info(exports), lists:sublist(atom_to_list(F), 1, 5) =:= "test "]).

all_test_ () ->
  {foreach, fun setup/0, fun tear_down/1,
   [fun (Ctx) ->
        ?MODULE:Fun(Ctx)
    end || Fun <- ?TESTS]}.

'test custom context with default file' (_) ->
  Result       = mustache:render(mustache_test_view, ctx_file),
  ?assertEqual(mustache_test_view:result(<<"Hello, {{planet}}">>), Result).
