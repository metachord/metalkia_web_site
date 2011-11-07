-module(mt_logoff).

-export([
  main/0
]).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("webmachine/include/webmachine.hrl").

main() ->
  wf:logout(),
  wf:redirect(mtc:get_env(url)).
