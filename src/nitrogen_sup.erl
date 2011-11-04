%% -*- mode: nitrogen -*-
-module(nitrogen_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    init/1
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% Start the Process Registry...
    application:start(nprocreg),

    %% Start up Webmachine...
    application:load(webmachine),
    {ok, BindAddress} = application:get_env(webmachine, bind_address),
    {ok, Port} = application:get_env(webmachine, port),

    io:format("Starting Webmachine Server on ~s:~p~n", [BindAddress, Port]),

    Options = [
        {ip, BindAddress},
        {port, Port},
        {dispatch, mtws_dispatch:rules()}
    ],
    webmachine_mochiweb:start(Options),

    {ok, { {one_for_one, 5, 10}, []} }.

