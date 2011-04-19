-module (mustache_run).
-export ([main/1]).

-ifndef (BUILD_TIME).
-define (BUILD_TIME, undefined).
-endif.

-ifndef (VCS_INFO).
-define (VCS_INFO, undefined).
-endif.

-define (USAGE(Options), getopt:usage(Options, "mustache.erl", "INPUT-FILE.mustache")).

main (Args) ->
  Options                 = [{ctx,     $c, "ctx",     string,           "Context file containing Erlang key-value pairs list"},
                             {out,     $o, "out",     string,           "Path to the output file"},
                             {version, $V, "version", {boolean, false}, "Show the tool version info"}],
  {ok,{Params, FreeArgs}} = getopt:parse(Options, Args),
  case lists:keysearch(version, 1, Params) of
    {value, {version, true}} ->
      io:format("mustache.erl [~p] build ~p~n", [?VCS_INFO, ?BUILD_TIME]);
    _ ->
      ok
  end,
  Template                = case FreeArgs of
                              [InputFile] ->
                                {ok, Bin} = file:read_file(InputFile),
                                Bin;
                              _ ->
                                ?USAGE(Options),
                                halt(1)
                            end,
  Output                  = case lists:keysearch(out, 1, Params) of
                              {value, {out, String1}} ->
                                String1;
                              false ->
                                undefined
                            end,
  CtxFile                 = case lists:keysearch(ctx, 1, Params) of
                              false ->
                                ?USAGE(Options),
                                halt(1);
                              {value, {ctx, String2}} ->
                                String2
                            end, 
  %% generate the ouput
  Bytes = mustache:render(Template, CtxFile),
  case Output of
    undefined ->
      file:write(erlang:group_leader(), Bytes);
    Output ->
      file:write_file(Output, Bytes, [binary])
  end.
