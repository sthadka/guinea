-module(guinea_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

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
    % Create ets table for payload lookups
    guinea_kv:new(),
    {ok, { {one_for_one, 5, 10}, [webserver()]} }.

webserver() ->
    MiddlewareConfig =
        [{mods, [
                 % Uncomment to enable elli stats (accesible at
                    % server/elli/stats )
                 {elli_stats, [{docroot, filename:join(
                                    code:priv_dir(elli_stats), "docroot")}]},
                 {guinea_http, []}]}],

    {webserver,
     {elli, start_link, [[{port, 5000},
                          {callback, elli_middleware},
                          {callback_args, MiddlewareConfig},
                          {name, {local, elli}}]]},
     permanent, 2000, worker, [elli]}.
