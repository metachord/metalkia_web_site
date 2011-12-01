%% -*- mode: nitrogen -*-
-module (mt_post).

-export([
  main/0,
  title/0,
  body/0,
  header/0,
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

title() -> mtws_common:blog_name().

author() ->
  %% FIXME: possible store author name in context?
  PathInfo = wf_context:path_info(),
  case dict:find(post_id, PathInfo) of
    {ok, Id} ->
      %% Get Author of post
      case mtc_entry:sget(mt_post, ?a2b(Id)) of
        #mt_post{author = #mt_author{id = PersonId}} ->
          case mtc_entry:sget(mt_person, ?a2b(PersonId)) of
            #mt_person{username = UserName, name = Name} ->
              if
                Name =:= undefined -> ?a2l(UserName);
                true -> unicode:characters_to_list(Name)
              end;
            _ ->
              ""
          end;
        _ -> ""
      end;
    _ -> ""
  end.

url() ->
  PathInfo = wf_context:path_info(),
  Prefix =
  case dict:find(blog, PathInfo) of
    {ok, default} ->
      mtc:get_env(url);
    {ok, BlogName} ->
      BlogName;
    _ ->
      mtc:get_env(url)
  end,
  case dict:find(post_id, PathInfo) of
    {ok, PostId} ->
      Prefix ++ "/post/" ++ PostId;
    _ ->
      Prefix
  end.


header() ->
  "".

body() ->
  PathInfo = wf_context:path_info(),
  case dict:find(post_id, PathInfo) of
    {ok, PostId} ->
      inner_body(PostId);
    _ ->
      posts_list()
  end.

inner_body(Path) ->
  Id = Path, % FIXME
  ?DBG("PostId: ~p", [Id]),
  User = wf:user(),
  case mtc_entry:sget(mt_post, ?a2b(Id)) of
    #mt_post{author = #mt_author{id = PersonId}} = Post ->
      Email =
      case mtc_entry:sget(mt_person, PersonId) of
        #mt_person{email = EmailBin} -> ?a2l(EmailBin);
        _ -> undefined
      end,

      ?DBG("Post:~n~p", [Post]),
      [
        #panel{style="margin-left: 50px;", body = [
          #gravatar{email = Email, rating = "g"},
          #panel{body = [
            #hidden{id="post-id", text=Id},
            #span{body = Post#mt_post.body}
          ]},
          default_items(Id),
          share_handlers(),
          #hr{}
        ]},
        #panel{id="pan-"++Id} |
        if
          User =/= undefined ->
            comment_tree(lists:keysort(#mt_comment_ref.parents, Post#mt_post.comments));
          true ->
            []
        end
      ];
    _ ->
      if
        User =/= undefined ->
          [
            #panel{style="margin-left: 50px;", body = [
              post_items(),
              #hr{}
            ]},
            #panel{id="pan"}
          ];
        true ->
          %% Redirect?
          []
      end
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
comment_tree([#mt_comment_ref{parents = _Parents, comment_key = CKey}|Comments], Tree) ->
  E =
  case mtc_entry:sget(mt_comment, CKey) of
    #mt_comment{post_id = PostId, id = CID, parents = Parents, body = Body} ->
      Path = parents_to_path(PostId, Parents),
      comment_body(#comment{
        post_id = PostId,
        id = CID,
        parents = Parents,
        path = Path,
        level = length(Parents),
        body = Body});
    _Other ->
      ?ERR("Bad comment record for key ~p: ~p", [CKey, _Other]),
      []
  end,
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
  [PostId|Parents] = path_to_parents(Path),
  User = wf:user(),
  #panel{class = "post-handlers", body = [
    #link{text="Link", url="/post/"++?a2l(PostId) ++
      if length(Parents) > 0 -> [CommentId|_] = lists:reverse(Parents), "#"++?a2l(CommentId); true -> "" end},
    if
      User =/= undefined ->
        #link{id="comment-"++Path, text="Comment", postback="comment-"++Path};
      true ->
        []
    end
  ]}.

share_handlers() ->
  #panel{class = "post-handlers", body = [
    #template{file="./site/templates/metalkia/share-buttons.tpl"}
  ]}.


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
  User = wf:user(),
  if
    User =/= undefined ->
      Text = wf:q("textarea"),
      Tags = wf:q("tags-input"),
      Sanit = fun(T) ->
        StripList = "[<>=&$#@!*%];\"\'`",
        re:replace(mtws_sanitizer:sanitize(T), StripList, "", [global, {return, list}, unicode])
      end,

      ?DBG("Tags: ~p", [Tags]),
      UserName = ?a2b(User),                        % FIXME
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
    true ->
      pass
  end;
event("add-comment-" ++ Path) ->
  User = wf:user(),
  if
    User =/= undefined ->
      LevelQ = wf:q("level-"++Path),
      Level = if LevelQ == undefined -> "0"; true -> LevelQ end,
      Text = wf:q("textarea-"++Path),
      [PostId|_] = string:tokens(Path, "-"),
      ?PRINT([{path, Path}, {post_id, PostId}, {level, Level}, {text, Text}]),
      [_PostId|Parents] = path_to_parents(Path),

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
    true ->
      pass
  end;

event(("comment-" ++ Id) = Target) ->
  User = wf:user(),
  if
    User =/= undefined ->
      wf:replace(Target, comment_post_items(Id));
    true ->
      pass
  end.

path_to_parents(Path) ->
  Result = string:tokens(Path, "-"),
  [list_to_integer(L) || L <- Result].

parents_to_path(PostId, Parents) ->
  binary_to_list(PostId) ++ "-" ++ string:join([integer_to_list(P) || P <- Parents], "-").
