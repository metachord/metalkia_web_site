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

-export([
  init/1,
  is_protected/1,
  realm/0,
  authenticate/3,
  is_authenticated/2,
  %%is_authorized/2,
  content_types_provided/2,
  to_html/2,
  allowed_methods/2,
  post_is_create/2,
  process_post/2
]).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-include_lib("metalkia_core/include/mt_records.hrl").
-include_lib("metalkia_core/include/mt_log.hrl").
-include_lib("metalkia_core/include/mt_util.hrl").

-include("mtws.hrl").

-record(comment, {
  post_id,
  id,
  path = "",
  parents = [],
  level = 0,
  body = ""
}).

-record(state, {page_module}).

init([]) ->
  PageModule = ?MODULE,
  ?DBG("PageModule: ~p", [PageModule]),
  State = #state { page_module=PageModule },
  {ok, State}.

allowed_methods(ReqData, State) ->
  {['HEAD', 'GET', 'POST'], ReqData, State}.

content_types_provided(Request, Context) ->
  {[{"text/html", to_html}],
    Request, Context}.

is_protected(_Module) ->
  ?DBG("Module: ~p", [_Module]),
  true.

realm() ->
  "Metalkia".

is_authenticated(_Module, _User) ->
  ?DBG("Module: ~p, ~p", [_Module, _User]),
  false.

authenticate(_Module, _User, _Password) ->
  ?DBG("Module: ~p, ~p:~p", [_Module, _User, _Password]),
  false.


%% is_authorized(Request, Context) ->
%%   case wrq:get_req_header("Authorization", Request) of
%%     "Basic "++Base64 ->
%%       Str = base64:mime_decode_to_string(Base64),
%%       case string:tokens(Str, ":") of
%%         ["authdemo", "demo1"] ->
%%           {true, Request, Context};
%%         _ ->
%%           {"Basic realm=webmachine", Request, Context}
%%       end;
%%     _ ->
%%       case proplists:get_value(is_protected, Context, false) of
%%         false -> {true, Request, Context};
%%         _ -> {"Basic realm=webmachine", Request, Context}
%%       end
%%   end.


post_is_create(ReqData, State) ->
  {false, ReqData, State}.

to_html(ReqData, State) ->
  PageModule = State#state.page_module,
  {ok, Data, ReqData1} = do_nitrogen(PageModule, ReqData),
  {Data, ReqData1, State}.

process_post(ReqData, State) ->
  PageModule = State#state.page_module,
  {ok, Data, ReqData1} = do_nitrogen(PageModule, ReqData),
  ReqData2 = wrq:set_resp_body(Data, ReqData1),
  {true, ReqData2, State}.

do_nitrogen(PageModule, Req) ->
  % Make request and response bridges...
  RequestBridge = simple_bridge:make_request(webmachine_request_bridge, Req),
  ResponseBridge = simple_bridge:make_response(webmachine_response_bridge, Req),
  nitrogen:init_request(RequestBridge, ResponseBridge),
  nitrogen:handler(mt_session_handler, PageModule),
  nitrogen:handler(mt_security_handler, PageModule),
  %%nitrogen:handler(http_basic_auth_security_handler, PageModule),
  nitrogen:handler(mt_route_handler, PageModule),
  nitrogen:handler(mt_identity_handler, PageModule),

  PathInfo = wrq:path_info(Req),
  wf_context:path_info(PathInfo),

  %% wf:state(state_key, "ValueState"),
  %% wf:session(sess_key, "Value"),
  %% wf:user("User"),
  %% ?DBG("PathInfo: ~p", [PathInfo]),
  %% ?DBG("User: ~p", [wf:user()]),

  nitrogen:run().


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
  ?DBG("Path: ~p", [PathInfo]),
  ?DBG("User: ~p", [wf:user()]),
  #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body(dict:fetch(post_id, PathInfo)) }
  ]}.

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
  Author = #mt_person{
    id = 1,
    name = <<"Zert">>
  },
  IdBin = mtc_entry:sput(#mt_post{
    author = Author,
    body = Text,
    origin = ?MT_ORIGIN
  }),
  Id = binary_to_list(IdBin),
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
  Parents = path_to_parents(Path),

  Author = #mt_person{
    id = 1,
    name = <<"Zert">>
  },
  Comment = #mt_comment{
    post_id = ?a2b(PostId),
    author = Author,
    body = Text,
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
      body = Text}
  )),
  wf:replace("comment-items-" ++ Path, default_items(Path));
event(("comment-" ++ Id) = Target) ->
  wf:replace(Target, comment_post_items(Id)).

path_to_parents(Path) ->
  [_PostId|Result] = string:tokens(Path, "-"),
  [list_to_integer(L) || L <- Result].

parents_to_path(PostId, Parents) ->
  binary_to_list(PostId) ++ "-" ++ string:join([integer_to_list(P) || P <- Parents], "-").
