%%% @copyright  2012 Metachord Ltd.
%%% @author     Max Treskin <mtreskin@metachord.com>


-module(mt_security_handler).
-behaviour(security_handler).

-export([init/2, finish/2]).

-include_lib("nitrogen_core/include/wf.hrl").

init(_Config, State) ->
  case wf:to_list(wf:page_module()) of
    "mt_" ++ _ ->
      {ok, State};
    "static_file" ->
      {ok, State};
    _ ->
      wf_context:page_module(mt_access_denied),
      {ok, State}
  end.

finish(_Config, State) ->
  {ok, State}.
