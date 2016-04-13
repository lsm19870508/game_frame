%%%-------------------------------------------------------------------
%%% @author 李世铭
%%% @copyright (C) April 1st,2016, <COMPANY>
%%% @doc
%%% 负责redis->mysql同步的写线程
%%% @end
%%% Created : 01. 四月 2016 15:02
%%%-------------------------------------------------------------------
-module(game_db_writer).
-author("Administrator").

-behaviour(gen_fsm).
-include("db_config.hrl").
-include("error_log.hrl").
-include("config_keys.hrl").

%% API
-export([start_link/0]).

-export([write_sql/0]).

%% gen_fsm callbacks
-export([init/1,
  writing/2,
  writing/3,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4]).

-define(SERVER, ?MODULE).
-define(MAX_PACKET,4096).%%mysql5.6默认允许的最大的包上限
-define(TIMEOUT_SPAN, 1000).%%休眠间隔
-define(ZERO_SPAN,0).%%立即执行

-record(state, {try_times=0}).%%重试次数

%%%===================================================================
%%% API
%%%===================================================================
%%写一条sql语句
write_sql()->
  StartTime = time_utility:longunixtime(),
  io:format("Start Writing Time is ~p!~n",[StartTime]),
  gen_fsm:send_event(?MODULE,{write_a_sql}).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, StateName :: atom(), StateData :: #state{}} |
  {ok, StateName :: atom(), StateData :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  io:format("db_writer is ready!~n"),
  %%{ok, writing, #state{},?ZERO_SPAN}.
  {ok,writing,#state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
-spec(writing(Event :: term(), State :: #state{}) ->
  {next_state, NextStateName :: atom(), NextState :: #state{}} |
  {next_state, NextStateName :: atom(), NextState :: #state{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
writing(timeout,State)->
  do_write(State);
writing(_Event, State) ->
  do_write(State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(writing(Event :: term(), From :: {pid(), term()},
    State :: #state{}) ->
  {next_state, NextStateName :: atom(), NextState :: #state{}} |
  {next_state, NextStateName :: atom(), NextState :: #state{},
    timeout() | hibernate} |
  {reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
  {reply, Reply, NextStateName :: atom(), NextState :: #state{},
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewState :: #state{}} |
  {stop, Reason :: normal | term(), Reply :: term(),
    NewState :: #state{}}).
writing(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, writing, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), StateName :: atom(),
    StateData :: #state{}) ->
  {next_state, NextStateName :: atom(), NewStateData :: #state{}} |
  {next_state, NextStateName :: atom(), NewStateData :: #state{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewStateData :: #state{}}).
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
    StateName :: atom(), StateData :: term()) ->
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
  {stop, Reason :: term(), NewStateData :: term()}).
handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), StateName :: atom(),
    StateData :: term()) ->
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #state{}, Extra :: term()) ->
  {ok, NextStateName :: atom(), NewStateData :: #state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%进行实际的写操作
do_write(State)->
  case State#state.try_times>0 of
    true->
      %%说明上次的消息未写入成功，从中转区取消息
      Result = redis:get(?CURR_WRITING_MSG),
      case Result of
        {ok,SzMsg} ->
          Msg = db_utility:unpack_data(SzMsg);
        _->
          {ok,Msg} = game_db_queue:dequeue(?MYSQL_WRITE_LIST),
          ?LOG_ERROR("REDIS SYSTEM ERROR!!!Cannot load Msg from game_frame:mysql_writing_msg")
      end;
    _->
      {ok,Msg} = game_db_queue:dequeue(?MYSQL_WRITE_LIST)
  end,
  case Msg of
    %%队列已空
    undefined->
      CurrTime = time_utility:longunixtime(),
      io:format("end writing test time is:~w~n",[{CurrTime}]),
      {next_state,writing,#state{},?TIMEOUT_SPAN};
    _->
      do_write(Msg,State)
  end.

do_write(Msg,State)->
  %%先将取出来的消息存入中转区
  redis:set(?CURR_WRITING_MSG,db_utility:pack_data(Msg)),
  IsPrepare = Msg#db_queue_msg.prepare,
  case IsPrepare of
    true->
      %%如果预编译过
      SqlId = Msg#db_queue_msg.prepare_atom,
      SqlArgs = Msg#db_queue_msg.prepare_param,
      PoolId = Msg#db_queue_msg.poolid,
      Result = mysql:run_prepare(PoolId,SqlId,SqlArgs);
    _->
      %%如果没有
      PoolId = Msg#db_queue_msg.poolid,
      Sql = Msg#db_queue_msg.sql,
      Result = mysql:execute(PoolId,Sql)
  end,
  case Result of
    {ok,_}->
      %%写入成功后标记数据过期时间
      Redis_expir_time = game_config:lookup_keys([?CF_DB_QUEUE, <<"redis_expir_time">>]),
      redis:expire(Msg#db_queue_msg.redis_key, integer_to_list(util:floor(3600 * Redis_expir_time))),
      %%然后中转区标记为<<"successful">>，表示写成功
      redis:set(?CURR_WRITING_MSG,<<"successful">>),
      {next_state,writing,#state{},?ZERO_SPAN};
    _->
      RetryTimes = State#state.try_times,
      case RetryTimes>=?MAX_MYSQL_RETRY_TIME of
        true->
          %% 如果写代码次数超过上限
          %% 单独写一个log，方便查找log
          ?LOG_ERROR("Max MySQL retry times reached, Msg is: ~p",
            [[Msg]]),
          {next_state,writing,#state{},?ZERO_SPAN};
        _->
          {next_state,writing,#state{try_times=RetryTimes + 1},?ZERO_SPAN}
      end
  end.