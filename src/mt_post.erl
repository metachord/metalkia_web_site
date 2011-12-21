%% -*- mode: nitrogen -*-
-module (mt_post).

-export([
  main/0,
  title/0,
  body/0,
  header/0,
  event/1,
  author/0
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
  level = 0
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
                true -> ?a2l(Name)
              end;
            _ ->
              ""
          end;
        _ -> ""
      end;
    _ -> ""
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

inner_body(post_add) ->
  User = wf:user(),
  if
    User =/= undefined ->
      [
        #panel{id="pan", style="margin-left: 50px;", body = [
          post_items(),
          #hr{}
        ]}
      ];
    true ->
      %% Redirect?
      []
  end;

inner_body(Id) ->
  PathInfo = wf_context:path_info(),
  User = wf:user(),
  PageUserName =
  case dict:find(username, PathInfo) of
    {ok, none} -> none;
    {ok, PgUN} -> ?a2b(PgUN);
    _ -> undefined
  end,
  case mtc_entry:sget(mt_post, ?a2b(Id)) of
    #mt_post{author = #mt_author{id = PersonId}} = Post
    when
      (is_binary(PageUserName) and (PageUserName =:= PersonId)) orelse
      (PageUserName =:= none) ->
      Email =
      case mtc_entry:sget(mt_person, PersonId) of
        #mt_person{email = EmailBin} -> ?a2l(EmailBin);
        _ -> undefined
      end,
      [
        #panel{id="pan-"++Id, style="margin-left: 50px;", body = [
          #link{body = [
            #gravatar{email = Email, rating = "g"}],
            url = mtws_common:user_blog(PersonId)
          },
          #panel{body = [
            #hidden{id="post-id", text=Id},
            post_body(Post)
          ]},
          #br{},
          share_handlers(),
          case ?a2l(PersonId) of
            User ->
              authors_handlers(Post);
            _ -> []
          end,
          default_items(Id, length(Post#mt_post.comments)),
          #hr{}
        ]},
        comment_tree(lists:keysort(#mt_comment_ref.parents, Post#mt_post.comments))
      ];
    _ ->
      wf:status_code(404),
      "Post not found"
  end.

posts_list() ->
  PathInfo = wf_context:path_info(),
  case dict:find(streams, PathInfo) of
    {ok, Streams} ->
      ReqStreams =
      case Streams of
        [] ->
          case dict:find(username, PathInfo) of
            {ok, UN} ->
              [#mt_stream{username = ?a2b(UN), tags = []}];
            _ -> []
          end;
        _ -> Streams
      end,

      Keys =
      lists:reverse(lists:umerge(
        [begin
          case Tags of
            [] ->
              UserPostsBucket = iolist_to_binary([UserName, "-", "posts"]),
              {ok, ListKeys} = mtriak:list_keys(UserPostsBucket),
              lists:reverse(ListKeys);
            _ ->
              lists:umerge([lists:sort(mtc_entry:sget(tags, UserName, Tag)) || Tag <- Tags])
          end
        end ||
          #mt_stream{username = UserName, tags = Tags} <- ReqStreams])),
      [
        begin
          BodyToCut =
          case re:run(Post#mt_post.body, "<mt-cut(\\s*([a-z]+)=[\"\']([^\"\']*?)[\"\']\\s*)*?>", [unicode, global]) of
            {match, [[{PStart, _PStop}|_]]} ->
              ?io2b([
                binary:part(Post#mt_post.body, 0, PStart),
                "<strong>(<a href=\"", post_link(Post#mt_post.id), "\">"
                "Read more"
                "</a>)</strong>"
              ]);
            _ ->
              Post#mt_post.body
          end,
          [#panel{id="pan-"++?a2l(Id), style="margin-left: 50px;", body = [
            #panel{body = [
              #panel{class = "post-body", body = BodyToCut}
            ]},
            default_items_post(Post),
            #hr{}
          ]}]
        end
      || #mt_post{id = Id} = Post <-
        lists:reverse(lists:keysort(#mt_post.timestamp,
            [mtc_entry:sget(mt_post, Key) ||
            Key <- Keys]))
      ];
    _ ->
      []
  end.

post_body(Post) ->
  #panel{id = "post-body", class = "post-body", body = Post#mt_post.body}.

comment_tree(Comments) ->
  comment_tree(Comments, []).

comment_tree([], Tree) ->
  Tree;
comment_tree([#mt_comment_ref{parents = _Parents, comment_key = CKey}|Comments], Tree) ->
  E =
  case mtc_entry:sget(mt_comment, CKey) of
    #mt_comment{post_id = PostId, id = CID, parents = Parents} = Comment->
      Path = parents_to_path(PostId, Parents),
      comment_body(Comment, #comment{
        post_id = PostId,
        id = CID,
        parents = Parents,
        path = Path,
        level = length(Parents)
      });
    _Other ->
      ?ERR("Bad comment record for key ~p: ~p", [CKey, _Other]),
      []
  end,
  comment_tree(Comments, Tree ++ [E]).

comment_body(#mt_comment{author = #mt_author{id = PersonId},
                         body = Body
                        },
             #comment{post_id = PostId,
                      id = Id,
                      parents = Parents,
                      path = _Path,
                      level = Level
                     }) ->
  Margin = Level*50+50,
  Anchor = integer_to_list(Id),

  {UserName, RealName, Email} =
  case mtc_entry:sget(mt_person, ?a2b(PersonId)) of
    #mt_person{username = UN, name = Name, email = EM} ->
      if
        Name =:= undefined -> {?a2l(UN), ?a2l(UN), ?a2l(EM)};
        true -> {?a2l(UN), Name, ?a2l(EM)}
      end;
    _Other ->
      ?ERR("Unknown person: ~p", [PersonId]),
      {"", "", ""}
  end,

  %% {"http","metalkia.com","/test/ttt","a=55","asd"}
  {UScheme, UHost, _UPath, _UArgs, _UPart} = mochiweb_util:urlsplit(mtc:get_env(url)),
  UserProfileUrl = mochiweb_util:urlunsplit({UScheme, UserName ++ "." ++ UHost, ["/profile"], "", ""}),

  [
    #panel{id="pan-"++parents_to_path(PostId, Parents), class = "comment-box", style="margin-left: "++integer_to_list(Margin)++"px;", body = [
      #panel{class = "user-identify-box", body = [
        #panel{class = "user-name", body = [
          #link{body = [
            #panel{class = "user-real-name", text = RealName},
            #panel{class = "user-profile-name", text = ["(", UserName, ")"]}
          ], url = UserProfileUrl}]},
        #panel{class = "user-avatar", body = [#gravatar{email = Email, rating = "g"}]}
      ]},
      #panel{body = [
        #panel{
          class = "comment-anchor",
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
        #panel{class = "comment-body", body = Body}
      ]},
      #hidden{id="level-"++parents_to_path(PostId, Parents), text=Level},
      default_items(parents_to_path(PostId, Parents))
    ]}
  ].

post_link(PostId) ->
  PathInfo = wf_context:path_info(),
  ?io2b([
    case dict:find(blog, PathInfo) of
      {ok, {BN, local}} ->
        ["/blog/", BN];
      {ok, {_BN, cname}} ->
        [];
      {ok, default} ->
        [];
      error ->
        []
    end,
    "/post/", ?a2l(PostId)]).


default_items_post(Post) ->
  Path = ?a2l(Post#mt_post.id),
  CommentsNum = length(Post#mt_post.comments),

  [PostId|_Parents] = path_to_parents(Path),
  #panel{class = "post-handlers", body = [
    #link{text="Comments" ++
      if
        is_integer(CommentsNum) andalso CommentsNum > 0 -> " (" ++ integer_to_list(CommentsNum) ++ ")";
        true -> ""
      end, url=post_link(PostId)}
  ]}.

default_items(Path) ->
  default_items(Path, undefined).

default_items(Path, CommentsNum) ->
  [PostId|Parents] = path_to_parents(Path),
  User = wf:user(),
  #panel{class = "post-handlers", body = [
    #link{text="Link", url=?io2b([post_link(PostId),
      if length(Parents) > 0 -> [CommentId|_] = lists:reverse(Parents), "#"++?a2l(CommentId); true -> "" end])},
    if
      User =/= undefined ->
        #link{id="comment-"++Path, text="Comment", postback="comment-"++Path};
      true ->
        []
    end,
    if
      is_integer(CommentsNum) andalso CommentsNum > 0 ->
        #span{text = "(" ++ integer_to_list(CommentsNum) ++ ")"};
      true ->
        ""
    end
  ]}.

authors_handlers(Post) ->
  #panel{class = "post-handlers", body = [
    #link{text = "Edit", postback = {post_edit, Post}}
  ]}.

share_handlers() ->
  #panel{class = "post-handlers", body = [
    #template{file="./site/templates/metalkia/share-buttons.tpl"}
  ]}.


post_items() ->
  TagList =
  case dict:find(streams, wf:path_info()) of
    {ok, Streams} ->
      case [Tags || #mt_stream{username = UserName, tags = Tags} <- Streams, UserName =:= ?a2b(wf:user())] of
        [] -> [];
        [T|_] -> T
      end;
    error ->
      []
  end,
  post_editor("comment-items", "", TagList, "Submit", add_post, "Cancel", cancel_add).


post_editor(Id, Body, TagList, Submit, SubmitPostback, Cancel, CancelPostback) ->
  #panel{id = Id,
    body = [
      #textarea{id="textarea", class="post-input", text = Body},
      #tagsinput{id="tags-input", text = unicode:characters_to_binary(string:join([unicode:characters_to_list(T) || T <- TagList], ","))},
      #button{id = submit, text = Submit, postback = SubmitPostback},
      #button{id = cancel, text = Cancel, postback = CancelPostback}
  ]}.

comment_post_items(Id) ->
  #panel{id = "comment-items-"++Id,
    body = [
      #textarea{id="textarea-"++Id},
      #button{id=submit, text="Submit",postback="add-comment-" ++ Id}
  ]}.

sanit(T) ->
  StripList = "[<>=&$#@!*%];\"\'`",
  re:replace(mtws_sanitizer:sanitize(T), StripList, "", [global, {return, list}, unicode]).

sanitize_fix(Text) ->
  case re:run(Text, "^\\s*<\\s*p\\s*>.*", [{capture, [0], list}]) of
    nomatch -> iolist_to_binary(["<p>", Text, "</p>"]);
    {match, _} -> iolist_to_binary([Text])
  end.

event({post_edit, Post}) ->
  wf:replace("post-body",post_editor("post-body", Post#mt_post.body, Post#mt_post.tags, "Save", {save_post, Post}, "Cancel", {cancel_edit, Post})),
  ok;
event({cancel_edit, Post}) ->
  wf:replace("post-body", post_body(Post)),
  ok;
event({save_post, #mt_post{id = PostId, author = #mt_author{id = PostAuthorId}} = Post}) ->
  User = wf:user(),
  UserBin = list_to_binary(User),
  if
    PostAuthorId =/= UserBin ->
      error;
    true ->
      Text = wf:q("textarea"),
      Tags = wf:q("tags-input"),
      mtc_entry:supdate(Post#mt_post{
        body = case Text of undefined -> Text; _ -> unicode:characters_to_binary(mtws_sanitizer:sanitize(sanitize_fix(Text))) end,
        tags = [unicode:characters_to_binary(sanit(unicode:characters_to_binary(T))) || T <- string:tokens(unicode:characters_to_list(list_to_binary(Tags)), ",")],
        origin = ?MT_ORIGIN
      })
  end,
  wf:redirect("/post/"++?a2l(PostId)),
  ok;
event(cancel_add) ->
  wf:redirect(mtws_common:user_blog(wf:user()));
event(add_post) ->
  User = wf:user(),
  if
    User =/= undefined ->
      Text = wf:q("textarea"),
      Tags = wf:q("tags-input"),

      ?DBG("Tags: ~p", [Tags]),
      UserName = ?a2b(User),                        % FIXME
      Author = #mt_author{
        id = UserName,
        name = UserName
      },
      IdBin = mtc_entry:sput(#mt_post{
        author = Author,
        body = case Text of undefined -> Text; _ -> unicode:characters_to_binary(mtws_sanitizer:sanitize(sanitize_fix(Text))) end,
        origin = ?MT_ORIGIN,
        tags = [unicode:characters_to_binary(sanit(unicode:characters_to_binary(T))) || T <- string:tokens(unicode:characters_to_list(list_to_binary(Tags)), ",")]
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
      [_PostId|Parents] = path_to_parents(Path),

      SanText = case Text of undefined -> Text; _ -> unicode:characters_to_binary(mtws_sanitizer:sanitize(iolist_to_binary(["<p>", Text, "</p>"]))) end,
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
      wf:insert_bottom("pan-"++Path,
        comment_body(Comment,
          #comment{
            post_id = ?a2b(PostId),
            id = CID,
            parents = Parents ++ [CID],
            path = NewId,
            level = list_to_integer(Level)+1}
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
