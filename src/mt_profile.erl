%% -*- mode: nitrogen -*-
-module (mt_profile).

-export([
  main/0,
  title/0,
  body/0,
  author/0,
  url/0,
  event/1
]).

%% For template
-export([
  form_label/1,
  form_entry/1
]).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-include_lib("metalkia_core/include/mt_records.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").
-include_lib("metalkia_core/include/mt_util.hrl").

-include("mtws.hrl").


main() -> #template { file="./site/templates/metalkia/bare.html" }.

title() -> "Add new post".

author() ->
  ?DBG("Author", []),
  "Zert".

url() ->
  ?DBG("URL", []),
  "http://metalkia.com".

body() ->
  PathInfo = wf_context:path_info(),
  ?DBG("PathInfo: ~p", [PathInfo]),
  ?DBG("User: ~p", [wf:user()]),
  Email = mtc:get_env(test_email, mtws_common:get_email()),
  wf:session(email_trusted, Email),
  if
    Email =:= undefined ->
      wf:redirect(mtws_common:blog_link());
    true ->
      [
        #gravatar{email = Email, rating = "g"},
        #p{},
        row(username),
        row(email),
        row(password),
        row(name),
        #button{text = "Save profile", postback = "save-profile"}
      ]
  end.

row(Tag) ->
  #panel{class = "clearfix", body = [
    #template{file = "./site/templates/metalkia/form_row.tpl", bindings = [{'Tag', Tag}]}
  ]}.


form_label(Tag) ->
  Text =
  case Tag of
    username -> "Metalkia Username";
    email -> "Email";
    name -> "Name";
    password -> "Password";
    _ -> "-undefined-"
  end,
  #label{text = Text}.

form_entry(Tag) ->
  case Tag of
    username ->
      username_entry("", "");
    email ->
      Email = wf:session(email_trusted),
      email_entry(Email, "", false);
    name ->
      Name = mtws_common:name(),
      #textbox{id = "input-name", text = Name};
    password ->
      #password{id = "input-password"};
    _ -> ""
  end.

event("check-username") ->
  UserName = normalize_input(wf:q("input-username")),
  ?DBG("Check UserName: ~p", [UserName]),
  case mtc_entry:sget(mt_person, ?a2b(UserName)) of
    #mt_person{} ->
      wf:replace("entry-username", username_entry(UserName, "warning"));
    _ ->
      wf:replace("entry-username", username_entry(UserName, "approved"))
  end;
event("verify-email") ->
  Email = normalize_input(wf:q("input-email")),
  ?DBG("Verify Email: ~p", [Email]),
  EmailProf = mtws_common:get_email(),
  if
    Email =:= EmailProf ->
      ?DBG("Email address is the same as in existing profile", []),
      wf:replace("entry-email", email_entry(Email, "approved", true));
    true ->
      wf:session(email_to_verify, Email),
      Code = email_verification(Email),
      wf:session(email_verify_code, Code),
      wf:insert_after("entry-email", email_verify_entry("", ""))
  end;
event("verify-email-check") ->
  Code = normalize_input(wf:q("input-email-verify")),
  Email = normalize_input(wf:q("input-email")),
  ?DBG("Verification code: ~p", [Code]),
  CodeSess = wf:session(email_verify_code),
  if
    CodeSess =:= Code ->
      wf:replace("entry-email", email_entry(Email, "approved", true)),
      wf:session(email_trusted, wf:session(email_to_verify)),
      wf:remove("entry-email-verify");
    true ->
      wf:replace("entry-email-verify", email_verify_entry(Code, "warning"))
  end;
event("save-profile") ->
  UserName = normalize_input(wf:q("input-username")),
  UserNameValid = wf:user(),
  UserNameValidBin = ?a2b(UserNameValid),

  Email = wf:session(email_trusted),
  Password = wf:q("input-password"),
  case mtc_entry:sget(mt_person, ?a2b(UserName)) of
    #mt_person{id = UserNameValidBin} = StoredPerson ->
      Person = StoredPerson#mt_person{
        password_sha1 = if Password =/= undefined -> crypto:sha(Password); true -> undefined end,
        email = ?a2b(Email)
      },
      ?DBG("Update profile:~n~p", [Person]),
      mtc_entry:supdate(Person),
      mtws_common:update_external_profile(UserNameValidBin);
    #mt_person{id = _Other} ->
      ?DBG("Bad username: ~p", [UserName]),
      wf:replace("entry-username", username_entry(UserName, "warning"));
    _ ->
      Person = #mt_person{
        id = ?a2b(UserName),
        username = ?a2b(UserName),
        password_sha1 = if Password =/= undefined -> crypto:sha(Password); true -> undefined end,
        email = ?a2b(Email)
      },
      ?DBG("Save profile:~n~p", [Person]),
      MetalkiaId = mtc_entry:sput(Person),
      wf:user(UserName),
      mtws_common:set_email(Email),
      mtws_common:update_external_profile(MetalkiaId)
  end.


%%
username_entry(UserName, Class) ->
  UserNameValid = wf:user(),
  #panel{id = "entry-username", body = [
    #textbox{id = "input-username", class = "mt-textbox " ++ Class,
      text = if UserNameValid =:= undefined -> UserName; true -> UserNameValid end},
    if
      UserNameValid =:= undefined ->
        [#button{text = "Check", postback = "check-username"}];
      true -> []
    end
  ]}.

email_entry(Email, Class, IsVerified) ->
  #panel{id = "entry-email", body = [
    #textbox{id = "input-email", class = "mt-textbox " ++ Class, text = Email} |
    if
      IsVerified -> [];
      true -> [#button{text = "Verify", postback = "verify-email"}]
    end
  ]}.

email_verify_entry(Code, Class) ->
  #panel{id = "entry-email-verify", body = [
    #textbox{id = "input-email-verify", class = "mt-textbox " ++ Class, text = Code},
    #button{text = "Ok", postback = "verify-email-check"}
  ]}.

email_verification(Email) ->
  F =
  "to:~p\n"
  "from:noreply@metalkia.com\n"
  "subject:Email verification\n"
  "\n"
  "Verification code: ~p\n"
  "Please entry this code in verification input form in profile",

  Code = mtc_util:rand_str(10),
  Port = open_port({spawn, mtc:get_env(sendmail, "/usr/sbin/sendmail -t")}, [use_stdio, exit_status, binary]),
  port_command(Port, unicode:characters_to_binary(io_lib:format(F, [Email, Code]))),
  port_close(Port),
  Code.


normalize_input(Input) ->
  re:replace(string:strip(Input, both), "\\s+", " ", [global, {return, list}]).
