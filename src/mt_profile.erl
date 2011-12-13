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

title() -> "Profile".

author() ->
  mtws_common:user_or_name().

url() ->
  mtc:get_env(url).

body() ->
  PathInfo = wf_context:path_info(),
  User = wf:user(),
  RequestedUser =
  case dict:find(username, PathInfo) of
    {ok, RU} -> RU;
    error -> none
  end,
  Email = mtc:get_env(test_email, mtws_common:get_email()),
  wf:session(email_trusted, Email),
  if
    ((RequestedUser =:= none) andalso (Email =/= undefined)) orelse
    RequestedUser =:= User ->
      %% Edit mode
      Profile = undefined,                      % FIXME
      [
        #gravatar{email = Email, rating = "g"},
        #flash{},
        #p{},
        row(edit, Profile, username),
        row(edit, Profile, email),
        row(edit, Profile, password),
        row(edit, Profile, name),
        #button{text = "Save profile", postback = "save-profile"}
      ];
    true ->
      %% Readonly mode
      case dict:find(profile, PathInfo) of
        {ok, Profile} ->
          [
            #gravatar{email = Profile#mt_person.email, rating = "g"},
            #p{},
            row(view, Profile, username),
            row(view, Profile, email),
            row(view, Profile, name)
          ];
        error ->
          []
      end
  end.

row(Mode, Profile, Tag) ->
  #panel{class = "clearfix", body = [
    #template{file = "./site/templates/metalkia/form_row.tpl", bindings = [{'Val', {Mode, Profile, Tag}}]}
  ]}.


form_label({Mode, _, Tag}) ->
  App = if Mode =:= edit -> " *"; true -> "" end,
  Text =
  case Tag of
    username -> "Metalkia Username" ++ App;
    email -> "Email" ++ App;
    name -> "Name" ++ App;
    password -> "Password";
    _ -> "-undefined-"
  end,
  #label{text = Text}.

form_entry({Mode, Profile, Tag}) ->
  case Mode of
    edit ->
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
      end;
    view ->
      case Tag of
        username -> Profile#mt_person.username;
        email -> Profile#mt_person.email;
        name -> Profile#mt_person.name
      end
  end.

event("check-username") ->
  check_username(),
  ok;
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
  UserNameValid = wf:user(),
  UserNameValidBin = ?a2b(UserNameValid),
  Name = normalize_input(wf:q("input-name")),

  Email = wf:session(email_trusted),
  Password = wf:q("input-password"),
  case check_username() of
    {ok, UserName} ->
      case mtc_entry:sget(mt_person, ?a2b(UserName)) of
        #mt_person{id = UserNameValidBin} = StoredPerson ->
          Person = StoredPerson#mt_person{
            password_sha1 = if Password =/= undefined -> crypto:sha(Password); true -> undefined end,
            email = ?a2b(Email),
            name = ?a2b(Name)
          },
          ?DBG("Update profile:~n~p", [Person]),
          mtc_entry:supdate(Person),
          mtws_common:update_external_profile(UserNameValidBin),
          wf:flash("Profile updated");
        #mt_person{id = _Other} ->
          ?DBG("Bad username: ~p", [UserName]),
          wf:replace("entry-username", username_entry(UserName, "warning"));
        _ ->
          Person = #mt_person{
            id = ?a2b(UserName),
            username = ?a2b(UserName),
            password_sha1 = if Password =/= undefined -> crypto:sha(Password); true -> undefined end,
            email = ?a2b(Email),
            name = ?a2b(Name)
          },
          ?DBG("Save profile:~n~p", [Person]),
          MetalkiaId = mtc_entry:sput(Person),
          wf:user(UserName),
          mtws_common:set_email(Email),
          mtws_common:update_external_profile(MetalkiaId),
          wf:flash("New profile saved")
      end;
    error ->
      ok
  end.


%%

%% Warning: side-effects on cliens-side
check_username() ->
  UserName = normalize_input(wf:q("input-username")),
  ?DBG("Check UserName: ~p", [UserName]),
  UNRegexp = "^[a-z][a-z_0-9]+$",
  case re:run(UserName, UNRegexp, [{capture, [0], list}]) of
    {match, [UserName]} ->
      %% TODO: Check username stop-list
      case mtc_entry:sget(mt_person, ?a2b(UserName)) of
        #mt_person{} ->
          wf:replace("entry-username", username_entry(UserName, "warning")),
          error;
        _ ->
          wf:replace("entry-username", username_entry(UserName, "approved")),
          {ok, UserName}
      end;
    _ ->
      wf:flash(wf:f("Username regexp: ~p", [UNRegexp])),
      wf:replace("entry-username", username_entry(UserName, "warning")),
      error
  end.


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
  Code = mtc_util:rand_str(10),
  Text = io_lib:format(
    "Verification code: ~p\n"
    "Please entry this code in verification input form in profile",
    [Code]),
  mtc_notify:send(email, Email, {"noreply@metalkia.com", "Email verification", Text}),
  Code.


normalize_input(Input) ->
  re:replace(string:strip(Input, both), "\\s+", " ", [global, {return, list}]).
