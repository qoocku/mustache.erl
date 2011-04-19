-module (mustache_custom_context_reader_tests).
-compile (export_all).
-include_lib ("eunit/include/eunit.hrl").

all_test_ () ->
  mustache_simple_tests:all_tests(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% unit tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'test custom context with default file' (_) ->
  Result = mustache:render(mustache_test_view, ctx_file),  
  ?assertEqual(mustache_test_view:result("Hello, {{planet}}"), Result).

'test custom context with custom reader' (_) ->
  Dict      = dict:from_list([{mustache_test_view, [{planet, "Earth"}]}]),
  CtxReader = fun (Mod) -> {ok, dict:fetch(Mod, Dict)} end,                  
  Result    = mustache:render(mustache_test_view, {ctx_reader, CtxReader}),
  ?assertEqual(mustache_test_view:result("Hello, {{planet}}"), Result).
  
  
