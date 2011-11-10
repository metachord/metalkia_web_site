-module(mt_404).

-export([
  main/0
]).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("webmachine/include/webmachine.hrl").

main() -> #template { file="./site/templates/metalkia/404.html" }.
