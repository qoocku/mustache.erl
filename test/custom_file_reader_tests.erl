-module (custom_file_reader_tests).
-compile (export_all).

-include_lib ("eunit/include/eunit.hrl").

render_mustache_templates_from_dict_test () ->
  List   = [{"mustache_test_view.mustache", Tmpl = <<"Hello, {{planet}}">>}],
  TD     = dict:from_list(List),
  Result = mustache:render(mustache_test_view, {reader, fun (File) ->
                                                            Key = filename:basename(File),
                                                            {ok, dict:fetch(Key, TD)}
                                                        end}),
  ?assertEqual(mustache_test_view:result(Tmpl), Result).

render_mustache_templates_from_dict_with_ctx_test () ->
  List   = [{"mustache_test_view.mustache", Tmpl = <<"Hello, {{planet}}">>}],
  TD     = dict:from_list(List),
  Ctx    = dict:from_list([{which, "Big"}]),
  Result = mustache:render(mustache_test_view, {reader, fun (File) ->
                                                            Key = filename:basename(File),
                                                            {ok, dict:fetch(Key, TD)}
                                                        end}, Ctx),
  ?assertEqual(mustache_test_view:result(Tmpl, Ctx), Result).

basic_render_mustache_templates_from_dict_test () ->
  List   = [{Id = "mustache_test_view.mustache", Tmpl = <<"Hello, {{planet}}">>}],
  TD     = dict:from_list(List),
  Result = mustache:render(mustache_test_view,
                           fun (File) ->
                               {ok, dict:fetch(File, TD)}
                           end, Id, dict:new()),
  ?assertEqual(mustache_test_view:result(Tmpl), Result).

basic_render_mustache_templates_from_dict_with_ctx_test () ->
  List   = [{Id = "mustache_test_view.mustache", Tmpl = <<"Hello, {{planet}}">>}],
  TD     = dict:from_list(List),
  Ctx    = dict:from_list([{which, "Big"}]),
  Result = mustache:render(mustache_test_view,
                           fun (File) ->
                               {ok, dict:fetch(File, TD)}
                           end, Id, Ctx),
  ?assertEqual(mustache_test_view:result(Tmpl, Ctx), Result).
                                        
