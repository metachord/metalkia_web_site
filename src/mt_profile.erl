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
  Email = mtws_common:get_email(),
  if
    Email =:= undefined ->
      wf:redirect(mtws_common:blog_link());
    true ->
      [
        #gravatar{email = Email, rating = "g"},
        #p{},
        row(username),
        row(email),
        row(name)
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
    _ -> "-undefined-"
  end,
  #label{text = Text}.

form_entry(Tag) ->
  case Tag of
    username ->
      username_entry("", "");
    email ->
      Email = mtws_common:get_email(),
      #textbox{text = Email};
    name ->
      Name = mtws_common:username(),
      #textbox{text = Name};
    _ -> ""
  end.

event("check-username") ->
  UserName = wf:q("input-username"),
  ?DBG("Check UserName: ~p", [UserName]),
  case mtc_entry:sget(mt_person, list_to_binary(UserName)) of
    #mt_person{} ->
      wf:replace("entry-username", username_entry(UserName, "warning"));
    _ ->
      wf:replace("entry-username", username_entry(UserName, "approved"))
  end.


%%
username_entry(UserName, Class) ->
  #panel{id = "entry-username", body = [
    #textbox{id = "input-username", class = "mt-textbox " ++ Class, text = UserName},
    #button{text = "Check", postback = "check-username"}
  ]}.
