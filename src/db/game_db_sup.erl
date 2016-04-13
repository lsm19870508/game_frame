%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 三月 2016 15:58
%%%-------------------------------------------------------------------
-module(game_db_sup).
-author("Administrator").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  %%eredis_pool的启动参数配置
  {ok,Args} = application:get_env(game_frame,redis_pools),
  GlobalOrLocal = proplists:get_value(global_or_local,Args),
  Pools = proplists:get_value(pools,Args),
  RedisPoolWorker = {eredis_pool_sup, {eredis_pool_sup, start_link, [Pools,GlobalOrLocal]}, permanent, 5000, supervisor, [eredis_pool_sup]},

  % 启动mysql连接池
  case catch mysql:init() of
    {ok, Result} ->
      io:format("=================start mysql pools ok~n");
    Error ->
      {ok, Args} = application:get_env(game_frame, mysql_pools),
      io:format("=================start_mysql_pool_fail,reason:~p, mysqlconfig:~p~n", [Error, Args]),
      init:stop()
  end,

  %%redis连接池，写入队列，缓存处理器(mysql=>redis)
  {ok, {SupFlags, [RedisPoolWorker,?CHILD(game_db_queue,worker),?CHILD(game_db_cache,worker),?CHILD(game_db_writer,worker)]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================