-module(mtws_dispatch).

-export([
  rules/0
]).

rules() ->
  [
    %% Static content handlers...
    {["css", '*'], static_resource, [{root, "./site/static/metalkia/css"}]},
    {["images", '*'], static_resource, [{root, "./site/static/metalkia/images"}]},
    {["nitrogen", '*'], static_resource, [{root, "./site/static/nitrogen"}]},
    {["mt", '*'], static_resource, [{root, "./site/static/metalkia"}]},
    {["post", post_id], mt_post, []},
    {["logoff"], mt_logoff, []},
    {["facebook"], mt_facebook, []},

    %% Add routes to your modules here. The last entry makes the
    %% system use the dynamic_route_handler, which determines the
    %% module name based on the path. It's a good way to get
    %% started, but you'll likely want to remove it after you have
    %% added a few routes.
    %%
    %% p.s. - Remember that you will need to RESTART THE VM for
    %%        dispatch changes to take effect!!!
    %%
    %% {["path","to","module1",'*'], nitrogen_webmachine, module_name_1}
    %% {["path","to","module2",'*'], nitrogen_webmachine, module_name_2}
    %% {["path","to","module3",'*'], nitrogen_webmachine, module_name_3}
    {['*'], static_resource, [{root, "./site/static/metalkia"}]}
    %%{['*'], nitrogen_webmachine, dynamic_route_handler}
  ].



