%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 五月 2016 17:44
%%%-------------------------------------------------------------------
-module(redis_test).
-author("Administrator").
-include("db_config.hrl").

%% API
-export([start/2,run/3,run_sql/2,recv/1]).


-record(state,{running=0,start_time,total_count_all,sql_count_each_process}).
%% 启动ProcessCount个进程，每个进程执行SqlCountEachProcess次sql 操作
start(TotalCount,SqlCountEachProcess)->
  CurrentTime=time_utility:longunixtime(),
  spawn_link(?MODULE,run,[TotalCount,SqlCountEachProcess,#state{start_time=CurrentTime,
    total_count_all=TotalCount,
    sql_count_each_process=SqlCountEachProcess}]).

recv(#state{running=0,start_time=StartTime, total_count_all=ProcessCount, sql_count_each_process=SqlCountEachProcess})->
  CurrentTime = time_utility:longunixtime(),
  Usedtime = CurrentTime-StartTime,
  io:format("process_count:~p sql count each process:~p used time:~p~n",[ProcessCount,SqlCountEachProcess,Usedtime]);
recv(#state{running=Running}=State)->
  receive
    done->
      recv(State#state{running=Running-1})
  end.

run(0,_SqlCountEachProcess,#state{}=State)->
  recv(State);
run(TotalCount,SqlCountEachProcess,#state{running=Running}=State) when (TotalCount>0) ->
  Parent =self(),
  spawn(fun()-> run_sql(SqlCountEachProcess,Parent)end),
  run(TotalCount-SqlCountEachProcess,SqlCountEachProcess,State#state{running=Running+1}).

run_sql(0,Parent)->
  Parent!done;
run_sql(SqlCountEachProcess,Parent) ->
  L = lists:seq(1,SqlCountEachProcess),
  [test_redis() || _<-L],
  run_sql(SqlCountEachProcess-SqlCountEachProcess ,Parent).

test_redis()->
  Id = util:rand(1,500000),
  SzId = conversion_utility:to_binary(Id),
  RedisIdKey = <<"tb_account:",SzId/binary>>,
  Values = ["id",Id,"accountname","test","password","test","is_register","test",
    "secret","test","secret_answer","test","phoneno","test","secrettime","test"],
  redis:hmset(RedisIdKey,Values).