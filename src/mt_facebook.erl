-module(mt_facebook).

-export([
  app_id/0,
  data_perms/0,
  login_redirect_uri/0
]).

app_id() ->
  "255033837865445".

data_perms() ->
  "email,user_checkins".

login_redirect_uri() ->
  "http://metalkia.com/facebook".
