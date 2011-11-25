%%%-------------------------------------------------------------------
%%% @author Maxim Treskin <mtreskin@metachord.com>
%%% @copyright (C) 2011, Maxim Treskin
%%% @doc
%%%
%%% @end
%%% Created : 24 Nov 2011 by Maxim Treskin <mtreskin@metachord.com>
%%%-------------------------------------------------------------------
-module(mtws_sanitizer).

-behaviour(gen_server).

-include_lib("metalkia_core/include/mt_log.hrl").

-export([start_link/0]).

-export([sanitize/1]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE).

-record(state, {
          port_path,
          port
         }).

%%% API
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

sanitize(Text) ->
  gen_server:call(?SERVER, {sanitize, Text}).

%%% gen_server callbacks

init([]) ->
  {ok, App} = application:get_application(?MODULE),
  HS = filename:join([code:priv_dir(App), "html_sanitizer"]),
  {ok, #state{port_path = HS}}.

handle_call({sanitize, Text}, _From,
            #state{port_path = HS} = State) ->
  Port = open_port({spawn, HS}, [{packet, 4}, binary, use_stdio]),
  port_command(Port, unicode:characters_to_binary(Text)),
  receive
    {Port, {data, Reply}} ->
      port_close(Port),
      {reply, unicode:characters_to_list(Reply), State};
    {'EXIT', Port, Reason} ->
      {stop, Reason, State}
  after 2000 ->
      {stop, port_timeout, State}
  end;
handle_call(Request, _From, State) ->
  Error = {unknown_call, Request},
  {stop, Error, {error, Error}, State}.

handle_cast(_Msg, State) ->
  ?DBG("Unhandled cast: ~p", [_Msg]),
  {noreply, State}.

handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
  ?DBG("Data from port: ~p", [Data]),
  port_close(Port),
  {noreply, State#state{port = undefined}};
handle_info(_Info, State) ->
  ?DBG("Unhandled info: ~p", [_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Internal functions
