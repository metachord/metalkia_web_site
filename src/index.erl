%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

-record(comment, {
  id,
  level = 0,
  body = ""
}).

main() -> #template { file="./site/templates/metalkia/bare.html" }.

title() -> "Welcome to Nitrogen".

body() ->
    #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
    ]}.

inner_body() ->
  inner_body(#comment{id="1234"}).

inner_body(#comment{id = Id, level = Level, body = Body}) ->
  [
    #panel{style="margin-left: 50px;", body = [
      #hidden{id="level-"++Id, text=Level},
      #panel{body = [
        #span{style="color:#ff0000;", body = Id},
        #br{},
        #span{body = Body}
      ]},
      default_items(Id),
      #hr{},
      #panel{id="pan-"++Id}
    ]}
  ].

default_items(Id) ->
  #link{id="comment-"++Id, text="Comment", postback="comment-"++Id}.

comment_items(Id) ->
  #panel{id = "comment-items-"++Id,
    body = [
      #textarea{id="textarea-"++Id},
      #button{id=submit, text="Submit",postback="click-" ++ Id}
  ]}.

event("click-" ++ Id) ->
  Level = wf:q("level-"++Id),
  Text = wf:q("textarea-"++Id),
  ?PRINT([{cid, Id}, {level, Level}, {text, Text}]),
  {_, _, A} = now(),
  NewId = Id ++ "-" ++ integer_to_list(A),
  ?PRINT(NewId),
  wf:insert_bottom("pan-"++Id,
    inner_body(#comment{
      id = NewId,
      level = list_to_integer(Level)+1,
      body = Text}
  )),
  wf:replace("comment-items-" ++ Id, default_items(Id));
event(("comment-" ++ Id) = Target) ->
  wf:replace(Target, comment_items(Id)).

