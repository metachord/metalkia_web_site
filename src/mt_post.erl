%% -*- mode: nitrogen -*-
-module (mt_post).

-include_lib("nitrogen_core/include/wf.hrl").


-include_lib("metalkia_core/include/mt_records.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").

-export([
    main/0,
    title/0,
    body/0,
    event/1]).

-record(comment, {
  id,
  path = "",
  parents = [],
  level = 0,
  body = ""
}).

main() -> #template { file="./site/templates/metalkia/bare.html" }.

title() -> "Add new post".

body() ->
  Path = wf_context:path_info(),
  %%?DBG("Context: ~p", [wf_context:disp_path()]),
  #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body(Path) }
  ]}.

inner_body(Path) ->
  Id = Path, % FIXME
  case mtriak:get_post(Id) of
    #mt_post{} = Post ->
      ?DBG("Post:~n~p", [Post]),
      [
        #panel{style="margin-left: 50px;", body = [
          #panel{body = [
            #hidden{id="post-id", text=Id},
            #span{body = Post#mt_post.body}
          ]},
          default_items(Id),
          #hr{}
        ]},
        #panel{id="pan-"++Id} |
        comment_tree(lists:keysort(#mt_comment.parents, Post#mt_post.comments))
      ];
    _ ->
      [
        #panel{style="margin-left: 50px;", body = [
          post_items(),
          #hr{}
        ]},
        #panel{id="pan"}
      ]
  end.

comment_tree(Comments) ->
  comment_tree(Comments, []).

comment_tree([], Tree) ->
  Tree;
comment_tree([#mt_comment{parents = Parents, body = Body}|Comments], Tree) ->
  Path = parents_to_path(Parents),
  [CID|_] = lists:reverse(Path),
  E = comment_body(#comment{
    id = CID,
    parents = Parents,
    path = Path,
    level = length(Parents)-1,
    body = Body}),
  comment_tree(Comments, Tree ++ [E]).


comment_body(#comment{id = _Id, parents = Parents, path = Path, level = Level, body = Body}) ->
  [PostId|_] = Parents,
  [CommentId|_] = lists:reverse(Parents),
  Margin = Level*50+50,
  [
    #panel{style="margin-left: "++integer_to_list(Margin)++"px;", body = [
      #hidden{id="level-"++Path, text=Level},
      #panel{body = [
        #span{style="color:#ff0000;", body = "#"++integer_to_list(PostId)++"/"++integer_to_list(CommentId)},
        #br{},
        #span{body = Body}
      ]},
      default_items(Path),
      #hr{}
    ]},
    #panel{id="pan-"++Path}
  ].

default_items(Path) ->
  #link{id="comment-"++Path, text="Comment", postback="comment-"++Path}.

post_items() ->
  #panel{id = "comment-items",
    body = [
      #textarea{id="textarea"},
      #button{id=submit, text="Submit",postback="add-post"}
  ]}.

comment_post_items(Id) ->
  #panel{id = "comment-items-"++Id,
    body = [
      #textarea{id="textarea-"++Id},
      #button{id=submit, text="Submit",postback="add-comment-" ++ Id}
  ]}.


event("add-post") ->
  Text = wf:q("textarea"),
  ?PRINT(Text),
  Id = integer_to_list(mtriak:add_post(#mt_post{body = Text})),
  wf:replace("comment-items",
    #panel{body = [
      #hidden{id="post-id", text=Id},
      #span{body = Text},
      #br{},
      comment_post_items(Id)
  ]}),
  wf:replace("pan", #panel{id = "pan-"++Id});
event("add-comment-" ++ Path) ->
  LevelQ = wf:q("level-"++Path),
  Level = if LevelQ == undefined -> "0"; true -> LevelQ end,
  Text = wf:q("textarea-"++Path),
  PostId = wf:q("post-id"),
  ?PRINT([{path, Path}, {post_id, PostId}, {level, Level}, {text, Text}]),
  %%NewId = integer_to_list(mtriak:new_id(<<"comments">>)),
  Parents = path_to_parents(Path),
  NewCID = mtriak:add_comment(PostId, Path, #mt_comment{body = Text, parents = Parents}),
  CID = integer_to_list(NewCID),
  NewId = Path++"-"++CID,
  ?PRINT(NewId),
  wf:insert_bottom("pan-"++Path,
    comment_body(#comment{
      id = CID,
      parents = Parents ++ [NewCID],
      path = NewId,
      level = list_to_integer(Level)+1,
      body = Text}
  )),
  wf:replace("comment-items-" ++ Path, default_items(Path));
event(("comment-" ++ Id) = Target) ->
  wf:replace(Target, comment_post_items(Id)).

path_to_parents(Path) ->
  [list_to_integer(I) || I <- string:tokens(Path, "-")].

parents_to_path(Parents) ->
  string:join([integer_to_list(P) || P <- Parents], "-").
