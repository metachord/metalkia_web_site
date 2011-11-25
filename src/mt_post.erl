%% -*- mode: nitrogen -*-
-module (mt_post).

-export([
  main/0,
  title/0,
  body/0,
  event/1,
  author/0,
  url/0
]).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-include_lib("metalkia_core/include/mt_records.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").
-include_lib("metalkia_core/include/mt_util.hrl").

-include("mtws.hrl").

-include("elements/mt_elements.hrl").

-record(comment, {
  post_id,
  id,
  path = "",
  parents = [],
  level = 0,
  body = ""
}).

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
  case dict:find(post_id, PathInfo) of
    {ok, PostId} ->
      #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body(PostId) }
      ]};
    _ ->
      posts_list()
  end.

inner_body(Path) ->
  Id = Path, % FIXME
  ?DBG("PostId: ~p", [Id]),
  case mtc_entry:sget(mt_post, ?a2b(Id)) of
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

posts_list() ->
  PathInfo = wf_context:path_info(),
  case dict:find(streams, PathInfo) of
    {ok, Streams} ->
      [[
        #panel{style="margin-left: 50px;", body = [
          #panel{body = [
            #span{body = Post#mt_post.body}
          ]},
          default_items(?a2l(Id)),
          #hr{}
        ]},
        #panel{id="pan-"++?a2l(Id)} |
        [] %%comment_tree(lists:keysort(#mt_comment.parents, Post#mt_post.comments))
      ] ||
        #mt_post{id = Id} = Post <- [mtc_entry:sget(mt_post, Key) ||
          Key <- lists:reverse(lists:umerge(
              [lists:umerge(
                [lists:sort(mtc_entry:sget(tags, UserName, Tag)) ||
                Tag <- Tags]) ||
                #mt_stream{username = UserName, tags = Tags} <- Streams]))]];
    _ ->
      []
  end.

comment_tree(Comments) ->
  comment_tree(Comments, []).

comment_tree([], Tree) ->
  Tree;
comment_tree([#mt_comment{post_id = PostId, id = CID, parents = Parents, body = Body}|Comments], Tree) ->
  Path = parents_to_path(PostId, Parents),
  %%[CID|_] = lists:reverse(Path),
  E = comment_body(#comment{
    post_id = PostId,
    id = CID,
    parents = Parents,
    path = Path,
    level = length(Parents),
    body = Body}),
  comment_tree(Comments, Tree ++ [E]).


comment_body(#comment{post_id = PostId, id = Id, parents = Parents, path = _Path, level = Level, body = Body}) ->
  Margin = Level*50+50,
  Anchor = integer_to_list(Id),
  [
    #panel{style="position:relative; margin-left: "++integer_to_list(Margin)++"px;", body = [
      #hidden{id="level-"++parents_to_path(PostId, Parents), text=Level},
      #panel{body = [
        #span{
          style="color:#ff0000;",
          body =
            %% [#link{
            %% text = "#"++integer_to_list(PostId)++"/"++Anchor,
            %% url = "#c"++Anchor, id="c"++Anchor,
            %% class = "comment-anchor-link"}]
          "<a href=\"#" ++ Anchor ++ "\" "
          "id=\"" ++ Anchor ++ "\" "
          "class=\"comment-anchor-link link\" "
          "target=\"_self\">" ++ "#"++binary_to_list(PostId)++"/"++Anchor ++ "</a>"
        },
        #br{},
        #span{body = Body}
      ]},
      default_items(parents_to_path(PostId, Parents)),
      #hr{}
    ]},
    #panel{id="pan-"++parents_to_path(PostId, Parents)}
  ].

default_items(Path) ->
  #link{id="comment-"++Path, text="Comment", postback="comment-"++Path}.

post_items() ->
  #panel{id = "comment-items",
    body = [
      #textarea{id="textarea", class="post-input"},
      #tagsinput{id="tags-input"},
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
  Tags = wf:q("tags-input"),
  Sanit = fun(T) ->
    StripList = "[<>=&$#@!*%];\"\'",
    re:replace(mtws_sanitizer:sanitize(T), StripList, "", [global, {return, list}, unicode])
  end,

  ?DBG("Tags: ~p", [Tags]),
  UserName = ?a2b(wf:user()),                   % FIXME
  Author = #mt_author{
    id = UserName,
    name = UserName
  },
  IdBin = mtc_entry:sput(#mt_post{
    author = Author,
    body = case Text of undefined -> Text; _ -> mtws_sanitizer:sanitize(Text) end,
    origin = ?MT_ORIGIN,
    tags = [unicode:characters_to_binary(Sanit(T)) || T <- string:tokens(unicode:characters_to_list(list_to_binary(Tags)), ",")]
  }),
  Id = binary_to_list(IdBin),
  wf:redirect("/post/"++Id);
event("add-comment-" ++ Path) ->
  LevelQ = wf:q("level-"++Path),
  Level = if LevelQ == undefined -> "0"; true -> LevelQ end,
  Text = wf:q("textarea-"++Path),
  [PostId|_] = string:tokens(Path, "-"),
  ?PRINT([{path, Path}, {post_id, PostId}, {level, Level}, {text, Text}]),
  Parents = path_to_parents(Path),

  SanText = case Text of undefined -> Text; _ -> mtws_sanitizer:sanitize(Text) end,
  UserName = ?a2b(wf:user()),                   % FIXME
  Author = #mt_author{
    id = UserName,
    name = UserName
  },
  Comment = #mt_comment{
    post_id = ?a2b(PostId),
    author = Author,
    body = SanText,
    parents = Parents
  },
  NewCID = mtc_entry:sput(Comment),
  CID = list_to_integer(binary_to_list(NewCID)),
  NewId = Path++"-"++CID,
  ?PRINT(NewId),
  wf:insert_bottom("pan-"++Path,
    comment_body(#comment{
      post_id = ?a2b(PostId),
      id = CID,
      parents = Parents ++ [CID],
      path = NewId,
      level = list_to_integer(Level)+1,
      body = SanText}
  )),
  wf:replace("comment-items-" ++ Path, default_items(Path));
event(("comment-" ++ Id) = Target) ->
  wf:replace(Target, comment_post_items(Id)).

path_to_parents(Path) ->
  [_PostId|Result] = string:tokens(Path, "-"),
  [list_to_integer(L) || L <- Result].

parents_to_path(PostId, Parents) ->
  binary_to_list(PostId) ++ "-" ++ string:join([integer_to_list(P) || P <- Parents], "-").
