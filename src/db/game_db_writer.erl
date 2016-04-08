%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 四月 2016 15:45
%%%-------------------------------------------------------------------
-module(game_db_writer).
-author("Administrator").

-behaviour(gen_fsm).

%% API
-export([start_link/0]).
-export([writeDelay/1]).

%% gen_fsm callbacks
-export([init/1,
  waiting/2,
  waiting/3,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4]).

-define(SERVER, ?MODULE).

-record(sql_writer, {time=0,sqlbuf= <<"">>,sql_num=0,redis_key_list=[]}).
-define(MAX_PACKET,4096).%%mysql5.6默认允许的最大的包上限
-define(TIME_SPAN, 1).%%数据库写入间隔
-define(TIMEOUT_SPAN, 1000).%%数据库写入间隔

%%%===================================================================
%%% API
%%%===================================================================

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
  {ok, StateName :: atom(), StateData :: #sql_writer{}} |
  {ok, StateName :: atom(), StateData :: #sql_writer{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, waiting, #sql_writer{}}.

writeDelay(Sql)->
  gen_fsm:send_event(?MODULE, {sql,Sql}),
  true.

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
-spec(waiting(Event :: term(), State :: #sql_writer{}) ->
  {next_state, NextStateName :: atom(), NextState :: #sql_writer{}} |
  {next_state, NextStateName :: atom(), NextState :: #sql_writer{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #sql_writer{}}).
%% ====================================================================
% @todo implement actual state
waiting(timeout, StateData) ->
  SqlBefore = StateData#sql_writer.sqlbuf,
  CurrTime = util:unixtime(),
  do_write(SqlBefore),
%% 	?DEBUG("mod_db timeout:~p~n",[{CurrTime}]),
%% 	?DEBUG("mod_db timeout sql is:~s~n",[SqlBefore]),
  {next_state, waiting, #sql_writer{time = CurrTime},?TIMEOUT_SPAN};

waiting({sql,Sql}, StateData) ->
  SqlBefore = StateData#sql_writer.sqlbuf,
  LastWriteTime = StateData#sql_writer.time,
  CurrTime = util:unixtime(),
  if
    (is_list(Sql))->
      Sql2 = list_to_binary(Sql);
    true->
      Sql2 = Sql
  end,
  FinalSql = util:trueab(byte_size(SqlBefore)==0,<<Sql2/binary>>,<<SqlBefore/binary,";",Sql2/binary>>),
  if
    (byte_size(FinalSql) > ?MAX_PACKET)->
      do_write(SqlBefore),
      {next_state, waiting, #sql_writer{time = CurrTime,sqlbuf=Sql2},?TIMEOUT_SPAN};
    true->
      if
        (CurrTime - LastWriteTime>?TIME_SPAN)->
          do_write(FinalSql),
          {next_state, waiting, #sql_writer{time = CurrTime},?TIMEOUT_SPAN};
        true->
          {next_state, waiting, #sql_writer{time = LastWriteTime,sqlbuf=FinalSql},?TIMEOUT_SPAN}
      end
  end.

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
-spec(waiting(Event :: term(), From :: {pid(), term()},
    State :: #sql_writer{}) ->
  {next_state, NextStateName :: atom(), NextState :: #sql_writer{}} |
  {next_state, NextStateName :: atom(), NextState :: #sql_writer{},
    timeout() | hibernate} |
  {reply, Reply, NextStateName :: atom(), NextState :: #sql_writer{}} |
  {reply, Reply, NextStateName :: atom(), NextState :: #sql_writer{},
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewState :: #sql_writer{}} |
  {stop, Reason :: normal | term(), Reply :: term(),
    NewState :: #sql_writer{}}).
waiting(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, state_name, State}.

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
    StateData :: #sql_writer{}) ->
  {next_state, NextStateName :: atom(), NewStateData :: #sql_writer{}} |
  {next_state, NextStateName :: atom(), NewStateData :: #sql_writer{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewStateData :: #sql_writer{}}).
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
    StateData :: #sql_writer{}, Extra :: term()) ->
  {ok, NextStateName :: atom(), NewStateData :: #sql_writer{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
