%%%-------------------------------------------------------------------
%%% @author 李世铭
%%% @Email:  240782361@qq.com
%%% @copyright (C) Mar/24th 2016, <COMPANY>
%%% @doc
%%% 数据库操作的定义文件
%%% @end
%%% Created : 24. 三月 2016 16:14
%%%-------------------------------------------------------------------
-author("Administrator").
%%redis define
-define(REDIS_TIMEOUT, 5000).

-define(REDIS_POOL_TIMEOUT, 5000).

-define(REDIS_DEFAULT_POOL,{global,dbsrv}).

-define(ZINCRBY(Set,AddValue,Member), case AddValue of
                                        0 ->
                                          [];
                                        _->
                                          ["ZINCRBY",Set,AddValue,Member]
                                      end).

-define(INCRBY(Key,Value), case Value of
                             0 ->
                               [];
                             _->
                               ["INCRBY", Key, Value]
                           end).

-define(EXISTS(Key),["EXISTS",Key]).

-define(INCR(Key),["INCR",Key]).

-define(GET(Key),["GET",Key]).

-define(SET(Key,Value),["SET",Key,Value]).

-define(DEL(Key),["DEL",Key]).

-define(HGET(Key,Field),["HGET",Key,Field]).

-define(HSET(Key,Field,Value),["HSET",Key,Field,Value]).

-define(HMSET(Key,Values),["HMSET" |[Key | Values]]).

-define(HMGET(Key,Fields), ["HMGET" |[Key|Fields]]).

-define(HDEL(Key,Fields),["HDEL"|[Key | Fields]]).

-define(HGETALL(Key),["HGETALL",Key]).

-define(HINCRBY(Key,Field,Value), ["HINCRBY" ,Key,Field,Value]).

-define(LPUSH(Key,Values), ["LPUSH" | [Key| Values]]).

-define(RPOP(Key), ["RPOP" , Key]).

-define(LLEN(Key), ["LLEN" , Values]).

-define(LRANGE(Key,Start,End),["LRANGE",Key,Start,End]).

-define(ZADD(OrderSet,Values), ["ZADD" | [OrderSet | Values]]).

-define(ZREM(OrderSet,Keys),["ZREM"|[OrderSet|Keys]]).

-define(ZSCORE(OrderSet,Member),["ZSCORE",OrderSet,Member]).

-define(ZRANGE(OrderSet,Start,End),["ZRANGE",OrderSet,Start,End]).

%%数据库写入队列，所用的临时表list
-define(MYSQL_WRITE_LIST,<<"game_frame:mysql_write_queue">>).
-define(CURR_WRITING_MSG,<<"game_frame:mysql_writing_msg">>).
-define(MAX_MYSQL_RETRY_TIME,5).

%%数据库队列用的结构
-record(db_queue_msg,{
            prepare=false,%%是否预编译过
            prepare_atom=undefined,%%预编译标识
            prepare_param=0,%%预编译参数
            poolid = default,
            sql= <<"">> %%如果未预编译则填写语句
}
).