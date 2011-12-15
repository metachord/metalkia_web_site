-module(mt_sso).

-export([
  main/0
]).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-include_lib("metalkia_core/include/mt_records.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").
-include_lib("metalkia_core/include/mt_util.hrl").


main() ->
  User = wf:user(),
  Action = wf:q(action),
  case Action of
    "auth" when User =:= undefined ->
      "";
    "auth" ->
      ReturnUrl = wf:q(return_url),
      AuthBaseUri = wf:q(auth_domain),
      case check_domain(AuthBaseUri) of
        true ->
          VerificationCode = User ++ "__" ++ mtc_util:rand_str(10),
          mtws_session:put_state(VerificationCode, mtws_common:get_state()),
          "<script>"
          "window.top.location = '" ++ AuthBaseUri ++ "/sso" ++
          "?action=verify" ++
          "&code=" ++ mtc_util:uri_encode(VerificationCode) ++
          "&return_url=" ++ mtc_util:uri_encode(ReturnUrl) ++
          "'</script>";
        _ -> ""
      end;
    "verify" ->
      Code = wf:q(code),
      ReturnUrl = wf:q(return_url),
      ?DBG("Code: ~p", [Code]),
      ?DBG("ReturnUrl: ~p", [ReturnUrl]),
      mtws_common:set_state(mtws_session:get_state(Code)),
      wf:redirect(ReturnUrl)
  end.

check_domain(_Domain) ->
  %% TODO: Check domain
  true.
