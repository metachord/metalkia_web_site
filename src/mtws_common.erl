-module(mtws_common).

-export([
  set_user/1
]).

-export([
  blog_name/0,
  title/0,
  url/0,
  username/0,
  profile_link/0
]).

-include_lib("nitrogen_core/include/wf.hrl").

-include_lib("metalkia_core/include/mt_log.hrl").

blog_name() ->
  "Metalkia blog".

title() -> "Metalkia".

url() ->
  ?DBG("URL", []),
  "http://metalkia.com".

username() ->
  wf:user().

profile_link() ->
  "#".

set_user(User) ->
  wf:user(User).
