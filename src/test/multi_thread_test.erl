%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 四月 2016 16:28
%%%-------------------------------------------------------------------
-module(multi_thread_test).
-author("Administrator").

%% API
-export([start/2,run/3,run_sql/2,recv/1]).


-record(state,{running=0,start_time,total_count_all,sql_count_each_process}).
%% 启动ProcessCount个进程，每个进程执行SqlCountEachProcess次sql 操作
start(TotalCount,SqlCountEachProcess)->
  emysql:execute(default,<<"delete from account">>),
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
  [test2() || _<-L],
  run_sql(SqlCountEachProcess-SqlCountEachProcess ,Parent).

test_prepare()->
  Rand = util:rand(1,10000),
  emysql:execute(default,account_replace,[Rand]).

test1()->
  Rand = util:rand(1,10000),
  Sql = io_lib:format(<<"REPLACE INTO account(id) values (~p)">>,[Rand]).

test2()->
  emysql:execute(default,<<"REPLACE INTO account(id) values (floor(RAND()*10000));">>).