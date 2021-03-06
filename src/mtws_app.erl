%%% @copyright  2012 Metachord Ltd.
%%% @author     Max Treskin <mtreskin@metachord.com>


-module(mtws_app).

-behaviour(application).

%% Application callbacks
-export([
  start/0,
  start/2,
  stop/1
]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  nitrogen_sup:start_link(),
  mtws_sup:start_link().

%% TODO: remove this
start() ->
  [application:start(A) ||
    A <-
      [
       compiler,
       syntax_tools,
       lager,
       crypto,
       public_key,
       ssl,
       inets,
       oauth,
       gproc,
       riakc,
       metalkia_core,
       metalkia_riak,
       metalkia_web_site,
       exmpp,
       metalkia_xmpp
      ]].


stop(_State) ->
    ok.
