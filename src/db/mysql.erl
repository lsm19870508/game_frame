%%%-------------------------------------------------------------------
%%% @author 李世铭
%%% @email 240782361@qq.com
%%% @copyright (C) Mar 29th,2016 <COMPANY>
%%% @doc
%%% mysql的具体操作，使用emysql库
%%% @end
%%% Created : 29. 三月 2016 19:07
%%%-------------------------------------------------------------------
-module(mysql).
-author("Administrator").
-include("error_log.hrl").

%% API
-export([init/0]).
-export([execute/1,execute/2]).
-export([run_prepare/2,run_prepare/3]).
-export([get_one/1,get_one/2]).
-export([get_row/1,get_row/2]).
-export([get_all/1,get_all/2]).
-export([insert/1,insert/2]).
-export([insertMult/1,insertMult/2]).
-export([update/1,update/2]).
-export([make_insert_sql/3]).
-export([make_replace_sql/3]).
-export([make_update_sql/5]).

%%初始化
-spec(init() -> {ok,Result::term()} | {error,Reason::term()}).
init()->
  %%启动mysql数据库
  Result = start_mysql_pools(),
  case Result of
    {ok,_}->
      %%进行某些语句的预编译
      prepare_queries(),
      Result;
    _->
      Result
  end.

%%启动mysql数据库
-spec(start_mysql_pools() -> {ok,Result::term()} | {error,Reason::term()}).
start_mysql_pools() ->
  io:format("====================ready to start mysql pools~n"),
  {ok, Args} = application:get_env(game_frame, mysql_pools),
  case proplists:get_value(enabled, Args, false) of
    true ->
      Pools = proplists:get_value(pools, Args),
      lists:foreach(
        fun({Name, Options}) ->
          emysql:add_pool(Name, Options)
        end, Pools),
      {ok, true};
    false ->
      {ok, false}
  end.

%%进行某些语句的预编译
prepare_queries()->
  ok = emysql:prepare(account_replace, <<"replace into account set id=?">>),
  ok.

%% ====================================================================
%% public functions
%% ====================================================================

%%mysql的标准请求接口，query
execute(Query)->
  execute(default,Query).

%% querysql
execute(POOL, QUERY)->
  case emysql:execute(POOL, QUERY) of
    {result_packet,_,_,RS,_} ->
      {ok, RS};
    {ok_packet,_,_,_,_,_,_} ->
      {ok, 0};
    {error_packet,_,_,_,DB_ERROR_MSG} ->
      ?LOG_ERROR("~n* DBERROR:~p *~n", [DB_ERROR_MSG]),
      {error, DB_ERROR_MSG};
    _D->
      {exception,_D}
  end.

%%运行预编译语句
run_prepare(SqlId,Args)->
  case emysql:execute(default, SqlId, Args) of
    {result_packet, _, RS, _} ->
      {ok, RS};
    {ok_packet, _, _, _, _, _, _} ->
      {ok, 0};
    {error_packet,_,_,_,DB_ERROR_MSG} ->
      ?LOG_ERROR("~n* DBERROR:~p *~n", [DB_ERROR_MSG]),
      {error, DB_ERROR_MSG};
    _D->
      {exception,_D}
  end.

%%运行预编译语句
run_prepare(POOL,SqlId,Args)->
  case emysql:execute(POOL, SqlId, Args) of
    {result_packet, _, RS, _} ->
      {ok, RS};
    {ok_packet, _, _, _, _, _, _} ->
      {ok, 0};
    {error_packet,_,_,_,DB_ERROR_MSG} ->
      ?LOG_ERROR("~n* DBERROR:~p *~n", [DB_ERROR_MSG]),
      {error, DB_ERROR_MSG};
    _D->
      {exception,_D}
  end.

%%取数据，只取一个
get_one(Query)->
  get_one(default,Query).

get_one(POOL, QUERY)->
  case emysql:execute(POOL, QUERY) of
    {result_packet,_,_,[[RS]],_} ->
      RS;
    {result_packet,_,_,[RS],_} ->
      ?LOG_ERROR("~n* DBERROR:~p *~n", [QUERY]),
      {error, QUERY};
    {ok_packet,_,_,_,_,_,_} ->
      {ok, 0};
    {error_packet,_,_,_,DB_ERROR_MSG} ->
      ?LOG_ERROR("~n* DBERROR:~p *~n", [DB_ERROR_MSG]),
      {error, DB_ERROR_MSG}
  end.

%%取数据，取一行
get_row(Query)->
  get_row(default,Query).

get_row(POOL, QUERY)->
  case emysql:execute(POOL, QUERY) of
    {result_packet,_,_,[RS],_} ->
      RS;
    {ok_packet,_,_,_,_,_,_} ->
      {ok, 0};
    {error_packet,_,_,_,DB_ERROR_MSG} ->
      ?LOG_ERROR("~n* DBERROR:~p *~n", [DB_ERROR_MSG]),
      {error, DB_ERROR_MSG}
  end.

%%取数据，全取，select * from XXX
get_all(Query)->
  get_all(default,Query).

get_all(POOL, QUERY)->
  case emysql:execute(POOL, QUERY) of
    {result_packet,_,_,RS,_} ->
      RS;
    {ok_packet,_,_,_,_,_,_} ->
      {ok, 0};
    {error_packet,_,_,_,DB_ERROR_MSG} ->
      ?LOG_ERROR("~n* DBERROR:~p *~n", [DB_ERROR_MSG]),
      {error, DB_ERROR_MSG}
  end.

%% insert
insert(Query)->
  insert(default,Query).

insert(POOL, QUERY) ->
  case emysql:execute(POOL, QUERY) of
    {ok_packet,_,_,NID,_,_,_} ->
      {ok, NID};
    {error_packet,_,_,_,DB_ERROR_MSG} ->
      ?LOG_ERROR("~n* DBERROR:~p *~n", [DB_ERROR_MSG]),
      {error, DB_ERROR_MSG};
    D ->
      ?LOG_ERROR("~n* DBERROR:~p *~n", [D]),
      {error, unknow_error}
  end.

%% insert 多条
insertMult(Query)->
  insert(default,Query).

insertMult(POOL, QUERY) ->
  RS = emysql:execute(POOL, QUERY),
  F = fun(ETM, L) ->
    case ETM of
      {ok_packet,_,_,NID,_,_,_} when is_list(L) ->
        [NID|L];
      {error_packet,_,_,_,DB_ERROR_MSG} ->
        {error, DB_ERROR_MSG}
    end
      end,
  lists:foldl(F, [], RS).

%%更新数据
update(Query)->
  update(default,Query).

%% update
update(POOL, QUERY) ->
  case emysql:execute(POOL, QUERY) of
    {ok_packet,_,_,_,_,_,_} ->
      {ok, 1};
    {error_packet,_,_,_,DB_ERROR_MSG} ->
      ?LOG_ERROR("~n* DBERROR:~p *~n", [DB_ERROR_MSG]),
      {error, DB_ERROR_MSG};
    D ->
      ?LOG_ERROR("~n* DBERROR:~p *~n", [D]),
      {error, unknow_error}
  end.

%%组合mysql insert语句
%% 使用方式make_insert_sql(test,["row","r"],["测试",123]) 相当 insert into `test` (row,r) values('测试','123')
%%Table:表名
%%Field：字段
%%Data:数据
make_insert_sql(Table, Field, Data) ->
  L = make_conn_sql(Field, Data, []),
  lists:concat(["insert into `", Table, "` set ", L]).

%%组合mysql replace语句
%% 使用方式make_insert_sql(test,["row","r"],["测试",123]) 相当 insert into `test` (row,r) values('测试','123')
%%Table:表名
%%Field：字段
%%Data:数据
make_replace_sql(Table, Field, Data) ->
  L = make_conn_sql(Field, Data, []),
  lists:concat(["replace into `", Table, "` set ", L]).

%%组合mysql update语句
%% 使用方式make_update_sql(test,["row","r"],["测试",123],"id",1) 相当 update `test` set row='测试', r = '123' where id = '1'
%%Table:表名
%%Field：字段
%%Data:数据
%%Key:键
%%Data:值
make_update_sql(Table, Field, Data, Key, Value) ->
  L = make_conn_sql(Field, Data, []),
  lists:concat(["update `", Table, "` set ",L," where ",Key,"= '",sql_format(Value),"'"]).


make_conn_sql([], _, L ) ->
  L ;
make_conn_sql(_, [], L ) ->
  L ;
make_conn_sql([F | T1], [D | T2], []) ->
  L  = [F," = '",sql_format(D),"'"],
  make_conn_sql(T1, T2, L);
make_conn_sql([F | T1], [D | T2], L) ->
  L1  = L ++ [",", F," = '",sql_format(D),"'"],
  make_conn_sql(T1, T2, L1).

sql_format(S) when is_integer(S)->
  integer_to_list(S);
sql_format(S) when is_float(S)->
  float_to_list(S);
sql_format(S) ->
  S.