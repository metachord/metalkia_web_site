%%% @copyright  2012 Metachord Ltd.
%%% @author     Max Treskin <mtreskin@metachord.com>


%% -*- mode: nitrogen -*-
-module (mt_index).

-export([
  main/0,
  header/0,
  body/0,
  author/0,
  event/1
]).

-include_lib("nitrogen_core/include/wf.hrl").

-include_lib("metalkia_core/include/mt_log.hrl").
-include_lib("metalkia_core/include/mt_records.hrl").

main() -> #template { file="./site/templates/metalkia/bare.html" }.

author() ->
  "Metachord".

header() ->
  "".


body() ->
    {ok, Users} = mtriak:list_keys(<<"persons">>),
    UL = #list{body = [#listitem{body = #link{text = UN, url = mtws_common:user_blog(UN)}} || UN <- lists:sort(Users)]},

    {ok, CNames} = mtriak:list_keys(<<"cnames">>),
    CL = #list{body = [#listitem{body = #link{text = CN, url = iolist_to_binary(["http://", CN])}} || CN <- lists:sort(CNames)]},
    #panel{class = "link-list", body = [
        #panel{class = "link-list-users", body = [#panel{class = "link-list-header", body = [<<"User blogs">>]}, UL]},
        #panel{class = "link-list-cnames", body = [#panel{class = "link-list-header", body = [<<"Domain blogs">>]}, CL]}
    ]}.




event(_) ->
  ok.

