-module(game_frame_sup).

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
    %%配置文件的gen_server
    ConfigPath = fetch_config_path(),
    ConfigWorker = {game_config, {game_config, start_link, [ConfigPath]},
    permanent, 5000, worker, [game_config]},

    {ok, { {one_for_one, 5, 10}, [ConfigWorker,?CHILD(lager_controller, worker),?CHILD(game_db_sup,supervisor)]} }.

fetch_config_path() ->
    case init:get_argument(config_path) of
        {ok, [[Path]]} ->
            Path;
        _ ->
            {ok, [[ConfigFile]]} = init:get_argument(config),
            filename:dirname(ConfigFile)
    end.

