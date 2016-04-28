%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 四月 2016 11:13
%%%-------------------------------------------------------------------
-module(game_db_writer3).
-author("Administrator").
-include("db_config.hrl").
-include("error_log.hrl").
-include("config_keys.hrl").

%% API
-export([write_sql/0]).

write_sql()->
  StartTime = time_utility:longunixtime(),
  io:format("Start Writing Time is ~p!~n",[StartTime]),
  do_write(0),
  ok.

%%进行实际的写操作
do_write(TryTimes)->
  case TryTimes>0 of
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
      timer:sleep(1000);
      %%do_write(0);
    _->
      do_write(Msg,TryTimes)
  end.

do_write(Msg,TryTimes)->
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
      do_write(0);
    _->
      case TryTimes>=?MAX_MYSQL_RETRY_TIME of
        true->
          %% 如果写代码次数超过上限
          %% 单独写一个log，方便查找log
          ?LOG_ERROR("Max MySQL retry times reached, Msg is: ~p",
            [[Msg]]),
          do_write(0);
        _->
          do_write(Msg,TryTimes+1)
      end
  end.


