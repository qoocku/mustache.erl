-module (mustache_custom_context_reader_tests).
-compile (export_all).
-include_lib ("eunit/include/eunit.hrl").

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

-define (TESTS(Prefix), [F || {F, 1} <- ?MODULE:module_info(exports),
                              lists:sublist(atom_to_list(F), 1, 5) =:= Prefix]).

all_test_ () ->
  {foreach, fun setup/0, fun tear_down/1,
   [fun (Ctx) ->
        {atom_to_list(Fun), ?MODULE:Fun(Ctx)}
    end || Fun <- ?TESTS("test ")]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% unit tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'test custom context with default file' (_) ->
  Result = mustache:render(mustache_test_view, ctx_file),
  ?assertEqual(mustache_test_view:result(<<"Hello, {{planet}}">>), Result).

'test custom context with custom reader' (_) ->
  Dict      = dict:from_list([{mustache_test_view, [{planet, "Earth"}]}]),
  CtxReader = fun (Mod) -> {ok, dict:fetch(Mod, Dict)} end,                  
  Result = ?debugVal(mustache:render(mustache_test_view, {ctx_reader, CtxReader})),
  ?assertEqual(mustache_test_view:result(<<"Hello, {{planet}}">>), Result).
  
