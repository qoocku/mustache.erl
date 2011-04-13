-module (mustache_test_view).
-compile (export_all).

planet () ->
  "Earth".

planet (Ctx) ->
  case dict:find(which, Ctx) of
    error -> planet();
    {ok, Which} -> Which ++ planet()
  end.

result (Tmpl) ->
  mustache:render(Tmpl, dict:from_list([{planet, planet()}])).

result (Tmpl, Ctx) ->
  mustache:render(Tmpl, dict:from_list([{planet, planet(Ctx)}])).

  
                 
