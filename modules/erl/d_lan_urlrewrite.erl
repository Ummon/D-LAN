-module(d_lan_urlrewrite).
-export([arg_rewrite/1]).

-include("/usr/lib/yaws/include/yaws_api.hrl"). % Should use 'include_lib' but code: code:lib_dir(yaws). return {error,bad_name}.

arg_rewrite(Arg) ->
   Req = Arg#arg.req,
   {abs_path, Path} = Req#http_request.path,   
   NPath = {abs_path,
      case string:rstr(Path, ".html") of
         0 -> Path;
         N -> "/?p=" ++ string:substr(Path, 2, N - 2) ++ string:substr(Path, N + 5)
      end
   },
   % For testing purpose.
   %{ok, F} = file:open("/tmp/out.txt", [write]),
   %io:format(F, "~p~n", [Path]),
   %file:close(F),   
   Arg#arg{req = Req#http_request{path = NPath}}.
   
