-module(mtws_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  Children =
    [
     {mtws_sanitizer, {mtws_sanitizer, start_link, []},
      Restart, Shutdown, Type, [mtws_sanitizer]}
    ],

  {ok, {SupFlags, Children}}.
