%% -*- mode: nitrogen -*-
-module (mt_post).

-export([
  main/0,
  title/0,
  body/0,
  feed/0,
  header/0,
  event/1,
  author/0
]).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-include_lib("metalkia_core/include/mt_records.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").
-include_lib("metalkia_core/include/mt_util.hrl").

-include_lib("xmerl/include/xmerl.hrl").

-include("mtws.hrl").

-include("elements/mt_elements.hrl").

-record(comment, {
  post_id,
  id,
  path = "",
  parents = [],
  level = 0
}).

main() ->
  PathInfo = wf_context:path_info(),
  case dict:find(content, PathInfo) of
    {ok, feed} ->
      feed();
    _ ->
      #template { file="./site/templates/metalkia/bare.html" }
  end.

feed() ->
  {Upd, Content} = atom_entries(),

  Header =
  [
    #xmlElement{
      name = generator,
      attributes = [
        #xmlAttribute{name = uri, value = "http://metalkia.com"}
      ],
      content = [#xmlText{value = "Metalkia Atom Feed"}]
    },
    #xmlElement{
      name = updated,
      content = [#xmlText{value = ?a2l(mtc_util:ts2rfc3339(Upd))}]
    },
    #xmlElement{
      name = link,
      attributes = [
        #xmlAttribute{name = rel, value = "self"},
        #xmlAttribute{name = type, value = "application/atom+xml"},
        #xmlAttribute{name = href, value = mtws_common:url()}
      ]
    },
    #xmlElement{
      name = id,
      content = [#xmlText{value = mtws_common:url()}]
    },
    #xmlElement{
      name = title,
      content = [#xmlText{value = mtws_common:blog_name()}]
    }
  ],

  Feed =
  #xmlElement{
    name = feed,
    attributes = [
      #xmlAttribute{name = xmlns,value = "http://www.w3.org/2005/Atom"},
      #xmlAttribute{name = 'xml:base',value = "http://metalkia.com"},
      #xmlAttribute{name = 'xml:lang',value = "en"}],
    content = Header ++ Content
  },
  xmerl:export_simple([Feed], xmerl_xml).

atom_entries() ->
  atom_entries(0, [],
    lists:keysort(#mt_post.timestamp,
      [mtc_entry:sget(mt_post, Key) ||
      Key <- streams_keys()])).


atom_entries(Upd, Acc, []) ->
  {Upd, Acc};
atom_entries(Upd, Acc, [
  #mt_post{
    author = #mt_author{id = AuthorId},
    timestamp = Published,
    last_mod = LastMod,
    title = Title
  } = Post | Posts]) ->
  Updated = if LastMod =:= undefined -> Published; true -> LastMod end,
  PostUrl = mtws_common:base_uri() ++ ?a2l(post_link(Post#mt_post.id)),
  PostTitle =
  if
    Title =:= undefined ->
      ?io2b(["Post #", ?a2l(Post#mt_post.id)]);
    true ->
      Title
  end,

  Res =
  #xmlElement{
    name = entry,
    attributes = [
      #xmlAttribute{name = 'xmlns', value = "http://www.w3.org/2005/Atom"},
      #xmlAttribute{name = 'xmlns:gd', value = "http://schemas.google.com/g/2005"},
      #xmlAttribute{name = 'xmlns:lang', value = "en"}
    ],
    content = [
      #xmlElement{name = id, content = [#xmlText{value = PostUrl}]},
      #xmlElement{name = updated, content = [#xmlText{value = ?a2l(mtc_util:ts2rfc3339(Updated))}]},
      #xmlElement{name = published, content = [#xmlText{value = ?a2l(mtc_util:ts2rfc3339(Published))}]},
      #xmlElement{name = author, content = [
        #xmlElement{name = name, content = [#xmlText{value = ?a2l(AuthorId)}]},
        #xmlElement{name = uri, content = [#xmlText{value = mtws_common:user_blog(AuthorId, ["/profile"])}]}
      ]},
      #xmlElement{
        name = link,
        attributes = [
          #xmlAttribute{name = rel, value = "alternate"},
          #xmlAttribute{name = type, value = "text/html"},
          #xmlAttribute{name = href, value = PostUrl}
        ]
      },
      #xmlElement{name = title, content = [#xmlText{value = ?a2l(PostTitle)}]},
      #xmlElement{
        name = content,
        attributes = [#xmlAttribute{name = type, value = "html"}],
        content = [#xmlText{value = ?a2l(cutted_body(Post))}]
      }
    ]
  },
  NewUpd = max(Updated, Upd),
  atom_entries(NewUpd, [Res | Acc], Posts).

title() ->
  PathInfo = wf_context:path_info(),
  [mtws_common:blog_name(),
    case dict:find(post_id, PathInfo) of
      {ok, post_add} ->
        [": ", "New post"];
      {ok, Id} ->
        case mtc_entry:sget(mt_post, ?a2b(Id)) of
          #mt_post{title = undefined} ->
            [": ", "#", Id];
          #mt_post{title = Title} ->
            [": ", Title];
          _Other ->
            ?ERR("Bad post entry for id ~p: ~p", [Id, _Other]),
            []
        end;
      _ ->
        []
    end].

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
  _PageUserName =
  case dict:find(username, PathInfo) of
    {ok, none} -> none;
    {ok, PgUN} -> ?a2b(PgUN);
    _ -> undefined
  end,
  case mtc_entry:sget(mt_post, ?a2b(Id)) of
    #mt_post{author = #mt_author{id = PersonId}} = Post ->
      {Email, RealName} =
      case mtc_entry:sget(mt_person, PersonId) of
        #mt_person{email = EmailBin, name = NameBin} -> {?a2l(EmailBin), ?a2l(NameBin)};
        _ -> {undefined, ""}
        end,
      [
        #panel{id="pan-"++Id, class="post-author", style="margin-left: 50px;", body = [
          #link{body = [
            #gravatar{email = Email, rating = "g"}],
            url = mtws_common:user_blog(PersonId)
          },
          user_name_profile(PersonId, RealName),
          #link{body = post_date(Post), url = post_link(Post#mt_post.id)},
          #panel{body = [
            #hidden{id="post-id", text=Id},
            post_body(Post)
          ]},
          #br{},
          share_handlers(),
          #br{},
          case ?a2l(PersonId) of
            User ->
              authors_handlers(Post);
            _ -> []
          end,
          default_items(Id, length(Post#mt_post.comments)),
          #hr{}
        ]},
        #panel{id="hpan-"++Id},
        comment_tree(lists:keysort(#mt_comment_ref.parents, Post#mt_post.comments))
      ];
    _ ->
      wf:status_code(404),
      "Post not found"
  end.

streams_keys() ->
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
          #mt_stream{username = UserName, tags = Tags} <- ReqStreams]));
    _ -> []
  end.

cutted_body(Post) ->
  Body = if Post#mt_post.body_html =:= undefined -> Post#mt_post.body; true -> Post#mt_post.body_html end,
  case re:run(Body, "<mt-cut(\\s*([a-z]+)=[\"\']([^\"\']*?)[\"\']\\s*)*?>", [unicode, global]) of
    {match, [[{PStart, _PStop}|_]]} ->
      ?io2b([
        binary:part(Body, 0, PStart),
        "<strong>(<a href=\"", post_link(Post#mt_post.id), "\">"
        "Read more"
        "</a>)</strong>"
      ]);
    _ ->
      Body
  end.


posts_list() ->
  Keys = streams_keys(),
  [
    begin
      [#panel{id="pan-"++?a2l(Id), style="margin-left: 50px;", body = [
        #panel{body = [
          #panel{class = "post", body = [
            #panel{class = "post-author-compact", body = [
              begin
                #mt_person{name = RealName} = mtc_entry:sget(mt_person, Post#mt_post.author#mt_author.id),
                {N, UN} = if
                  RealName =:= undefined orelse RealName =:= <<>> ->
                    {Post#mt_post.author#mt_author.id, Post#mt_post.author#mt_author.id};
                  true ->
                    {RealName, Post#mt_post.author#mt_author.id}
                end,
                #link{body = N, url = mtws_common:user_blog(UN, ["/profile"])}
              end
            ]},
            #link{body = post_date(Post), url = post_link(Post#mt_post.id)},
            #panel{class = "post-title", body = [#link{body = Post#mt_post.title, url = post_link(?a2l(Post#mt_post.id))}]},
            #panel{class = "post-body", body = [cutted_body(Post)]}
          ]}
        ]},
        default_items_post(Post),
        #hr{}
      ]}]
    end
  || #mt_post{id = Id} = Post <-
    lists:reverse(lists:keysort(#mt_post.timestamp,
        [mtc_entry:sget(mt_post, Key) ||
        Key <- Keys]))
  ].

post_body(Post) ->
  Body = if Post#mt_post.body_html =:= undefined -> Post#mt_post.body; true -> Post#mt_post.body_html end,
  #panel{id = "post", class = "post", body = [
    #panel{class = "post-title", body = Post#mt_post.title},
    #panel{class = "post-body", body = Body}
  ]}.

ts2str(T) ->
  {{Y,M,D},{H,Min,Sec}} = mtc_util:ts2dt(T),
  io_lib:format("~4..0b/~2..0b/~2..0b ~2..0b:~2..0b:~2..0b", [Y, M, D, H, Min, Sec]).


post_date(#mt_post{timestamp = Ts, last_mod = LastMod}) ->
  if
    (Ts =:= LastMod) orelse (LastMod =:= undefined) ->
      #panel{class = "post-date", body = [ts2str(Ts)]};
    true ->
      #panel{class = "post-date", body = [ts2str(Ts), [" (mod: ", ts2str(LastMod), ")"]]}
  end.

comment_date(#mt_comment{timestamp = Ts}) ->
  #panel{class = "comment-date", body = [ts2str(Ts)]}.

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

comment_body(#mt_comment{author = #mt_author{id = PersonId}} = Comment,
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
  Body = if Comment#mt_comment.body_html =:= undefined -> Comment#mt_comment.body; true -> Comment#mt_comment.body_html end,
  [
    #panel{id="pan-"++parents_to_path(PostId, Parents), class = "comment-box", style="margin-left: "++integer_to_list(Margin)++"px;", body = [
      #panel{class = "user-identify-box", body = [
        user_name_profile(UserName, RealName),
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
        #link{body = comment_date(Comment), url = "#"++Anchor},
        #panel{class = "comment-body", body = Body}
      ]},
      #hidden{id="level-"++parents_to_path(PostId, Parents), text=Level},
      default_items(parents_to_path(PostId, Parents))
    ]},
    #panel{id="hpan-"++parents_to_path(PostId, Parents)}
  ].

user_name_profile(UserName, RealName) ->
  UserProfileUrl = mtws_common:user_blog(UserName, ["/profile"]),
  #panel{class = "user-name", body = [
    #link{body = [
      #panel{class = "user-real-name", text = RealName},
      #panel{class = "user-profile-name", text = ["(", UserName, ")"]}
    ], url = UserProfileUrl}
  ]}.

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
  Title = "",
  Body = "",
  Format = "markdown",                          % TODO: get from profile
  post_editor("comment-items", Title, Body, Format, TagList, "Submit", add_post, "Cancel", cancel_add).


post_editor(Id, Title, Body, Format, TagList, Submit, SubmitPostback, Cancel, CancelPostback) ->
  #panel{id = Id,
    body = [
      #textbox{id="post-title", text = Title},
      #br{},
      #textarea{id="textarea", class="post-input", text = Body},
      #tagsinput{id="tags-input", text = unicode:characters_to_binary(string:join([unicode:characters_to_list(T) || T <- TagList], ","))},
      #br{},
      #dropdown{id=input_format, options =
        [#option{text=TVal, value=Val, selected=Val=:=Format} || {TVal, Val} <- [{"HTML", "html"}, {"Markdown", "markdown"}]]},
      #br{},
      #button{id = submit, text = Submit, postback = SubmitPostback},
      #button{id = cancel, text = Cancel, postback = CancelPostback}
  ]}.

comment_post_items(Id, Format) ->
  #panel{id = "comment-items-"++Id, class = "comment-add-box",
    body = [
      #textarea{id="textarea-"++Id},
      #br{},
      #dropdown{id=input_format, options =
        [#option{text=TVal, value=Val, selected=Val=:=Format} || {TVal, Val} <- [{"HTML", "html"}, {"Markdown", "markdown"}]]},
      #br{},
      #button{id=submit, text="Submit",postback="add-comment-" ++ Id},
      #button{text="Cancel",postback="cancel-comment-" ++ Id}
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
  wf:replace("post",post_editor("post", Post#mt_post.title, Post#mt_post.body, mt_format(Post#mt_post.format), Post#mt_post.tags, "Save", {save_post, Post}, "Cancel", {cancel_edit, Post})),
  ok;
event({cancel_edit, Post}) ->
  wf:replace("post", post_body(Post)),
  ok;
event({save_post, #mt_post{id = PostId, author = #mt_author{id = PostAuthorId}} = Post}) ->
  User = wf:user(),
  UserBin = list_to_binary(User),
  if
    PostAuthorId =/= UserBin ->
      error;
    true ->
      Title = wf:q("post-title"),
      Text = wf:q("textarea"),
      Tags = wf:q("tags-input"),
      Format = wf:q(input_format),
      mtc_entry:supdate(Post#mt_post{
        title = case Title of undefined -> Title; _ -> unicode:characters_to_binary(mtws_sanitizer:sanitize("text", sanitize_fix(Title))) end,
        body = Text,
        body_html = case Text of undefined -> Text; _ -> unicode:characters_to_binary(mtws_sanitizer:sanitize(Format, if Format =:= "html" -> sanitize_fix(Text); true -> ?a2b(Text) end)) end,
        tags = [unicode:characters_to_binary(sanit(unicode:characters_to_binary(T))) || T <- string:tokens(unicode:characters_to_list(list_to_binary(Tags)), ",")],
        format = mt_format(Format),
        origin = ?MT_ORIGIN
      })
  end,
  wf:redirect(redirect_url(PostId));
event(cancel_add) ->
  wf:redirect(redirect_url());
event(add_post) ->
  User = wf:user(),
  if
    User =/= undefined ->
      Title = wf:q("post-title"),
      Text = wf:q("textarea"),
      Tags = wf:q("tags-input"),
      Format = wf:q(input_format),

      ?DBG("Tags: ~p", [Tags]),
      UserName = ?a2b(User),                        % FIXME
      Author = #mt_author{
        id = UserName,
        name = UserName
      },
      IdBin = mtc_entry:sput(#mt_post{
        author = Author,
        title = case Title of undefined -> Title; _ -> unicode:characters_to_binary(mtws_sanitizer:sanitize("text", sanitize_fix(Title))) end,
        body = Text,
        body_html = case Text of undefined -> Text; _ -> unicode:characters_to_binary(mtws_sanitizer:sanitize(Format, if Format =:= "html" -> sanitize_fix(Text); true -> ?a2b(Text) end)) end,
        format = mt_format(Format),
        origin = ?MT_ORIGIN,
        tags = [unicode:characters_to_binary(sanit(unicode:characters_to_binary(T))) || T <- string:tokens(unicode:characters_to_list(list_to_binary(Tags)), ",")]
      }),
      wf:redirect(redirect_url(IdBin));
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
      Format = wf:q(input_format),
      [PostId|_] = string:tokens(Path, "-"),
      [_PostId|Parents] = path_to_parents(Path),

      UserName = ?a2b(wf:user()),                   % FIXME
      Author = #mt_author{
        id = UserName,
        name = UserName
      },
      Comment = #mt_comment{
        post_id = ?a2b(PostId),
        author = Author,
        body = Text,
        body_html = case Text of undefined -> Text; _ -> unicode:characters_to_binary(mtws_sanitizer:sanitize(Format, if Format =:= "html" -> sanitize_fix(Text); true -> ?a2b(Text) end)) end,
        format = mt_format(Format),
        timestamp = mtc_util:timestamp(),
        parents = Parents
      },

      PostUrl = mtws_common:base_uri() ++ ?a2l(post_link(PostId)),
      NotifyCompFun =
      fun(P, C) ->
        mtws_common:html_notify(P, C, PostUrl)
      end,

      NewCID = mtc_entry:sput(Comment, NotifyCompFun),
      CID = list_to_integer(binary_to_list(NewCID)),
      NewId = Path++"-"++CID,
      wf:insert_bottom("hpan-"++Path,
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

event(("cancel-comment-" ++ Id)) ->
  User = wf:user(),
  if
    User =/= undefined ->
      Target = "comment-" ++ Id,
      wf:replace("comment-items-" ++ Id, #link{id=Target, text="Comment", postback=Target});
    true ->
      pass
  end;
event(("comment-" ++ Id) = Target) ->
  User = wf:user(),
  if
    User =/= undefined ->
      Format = "markdown",                      % TODO: get from profile
      wf:replace(Target, comment_post_items(Id, Format));
    true ->
      pass
  end.

path_to_parents(Path) ->
  Result = string:tokens(Path, "-"),
  [list_to_integer(L) || L <- Result].

parents_to_path(PostId, Parents) ->
  binary_to_list(PostId) ++ "-" ++ string:join([integer_to_list(P) || P <- Parents], "-").

redirect_url() ->
  redirect_url(undefined).
redirect_url(Id) ->
  PathInfo = wf_context:path_info(),
  PostSuff = if Id =/= undefined -> ["/post/", ?a2l(Id)]; true -> ["/"] end,
  case {dict:find(blog_id, PathInfo), dict:find(blog, PathInfo)} of
    {{ok, BN}, _} ->
      mtws_common:user_blog(wf:user(), ["/blog/", BN, PostSuff]);
    {_, {ok, {_BlogName, cname}}} ->
      PostSuff;
    _ ->
      mtws_common:user_blog(wf:user(), PostSuff)
  end.

mt_format(Format) ->
  case Format of
    "markdown" ->
      ?mtc_schema_Mt_format_MARKDOWN;
    "html" ->
      ?mtc_schema_Mt_format_HTML;
    ?mtc_schema_Mt_format_MARKDOWN ->
      "markdown";
    ?mtc_schema_Mt_format_HTML ->
      "html";
    undefined ->
      "html"
  end.
