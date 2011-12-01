%%%-------------------------------------------------------------------
%%% @author Maxim Treskin <mtreskin@metachord.com>
%%% @copyright (C) 2011, Maxim Treskin
%%% @doc
%%%
%%% @end
%%% Created : 30 Nov 2011 by Maxim Treskin <mtreskin@metachord.com>
%%%-------------------------------------------------------------------
-module(mtws_session).

-behaviour(gen_server).

-export([start_link/0]).

-export([
         put_state/2,
         put_state/3,
         get_state/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE).

-define(SSO_TO, timer:seconds(20)).

-record(state, {
          tid    :: ets:tid()
         }).

%%% API
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

put_state(Code, Value) ->
  put_state(Code, Value, ?SSO_TO).

put_state(Code, Value, TimeOut) ->
  gen_server:call(?SERVER, {put, Code, Value, TimeOut}).

get_state(Code) ->
  gen_server:call(?SERVER, {get, Code}).

%%% gen_server callbacks

init([]) ->
  Tid = ets:new(session_states, [named_table]),
  {ok, #state{
     tid = Tid
    }}.

handle_call({put, Code, Value, TimeOut}, _From,
            #state{tid = Tid} = State) ->
  ets:insert(Tid, {Code, Value}),
  timer:send_after(TimeOut, {drop, Code}),
  {reply, ok, State};
handle_call({get, Code}, _From,
            #state{tid = Tid} = State) ->
  Value =
    case ets:lookup(Tid, Code) of
      [{Code, V}] -> V;
      _ -> undefined
    end,
  ets:delete(Tid, Code),
  {reply, Value, State};
handle_call(Request, _From, State) ->
  Error = {unknown_call, Request},
  {stop, Error, {error, Error}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({drop, Code},
            #state{tid = Tid} = State) ->
  ets:delete(Tid, Code),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Internal functions
