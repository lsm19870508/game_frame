%%%-------------------------------------------------------------------
%%% @author 李世铭
%%% @Email:  240782361@qq.com
%%% @copyright (C) Mar/24th 2016, <COMPANY>
%%% @doc
%%% 一个封装好的redis操作类
%%% @end
%%% Created : 24. 三月 2016 16:01
%%%-------------------------------------------------------------------
-module(redis).
-include("db_config.hrl").
-author("Administrator").

%% API
-export([incr/1,incr/2,incr/3]).
-export([set/2,set/3]).
-export([get/1,get/2]).
-export([del/1,del/2]).
-export([hget/2,hget/3]).
-export([hset/3,hset/4]).
-export([hdel/2,hdel/3]).
-export([hmget/2,hmget/3]).
-export([hmset/2,hmset/3]).
-export([hgetall/1,hgetall/2]).
-export([exists/1,exists/2]).
-export([hincrby/2,hincrby/3,hincrby/4]).
-export([redis_q/3,redis_qp/3]).
-export([lrange/3,lrange/4]).
-export([lpush/2,lpush/3]).
-export([rpop/1,rpop/2]).
-export([llen/1,llen/2]).
-export([zscore/2,zscore/3]).
-export([zadd/2,zadd/3]).
-export([zincr/3,zincr/4]).
-export([zrange/3,zrange/4]).
-export([persist/1,persist/2]).
-export([expire/2,expire/3]).

%%persist
%%http://www.yiibai.com/redis/keys_persist.html
persist(Key) ->
  persist(?REDIS_DEFAULT_POOL,Key).

persist(PoolName,Key) ->
  redis_q(PoolName,?PERSIST(Key),?REDIS_TIMEOUT).

%%expire
%%http://www.yiibai.com/redis/keys_expire.html
expire(Key, ExpireTime) ->
  expire(?REDIS_DEFAULT_POOL,Key,ExpireTime).

expire(PoolName,Key,ExpireTime)->
  redis_q(PoolName,?EXPIRE(Key,ExpireTime),?REDIS_TIMEOUT).

%%incr
incr(Key)->
  incr(?REDIS_DEFAULT_POOL,Key).

incr(PoolName,Key)->
  redis_q(PoolName,?INCR(Key),?REDIS_TIMEOUT).

incr(PoolName,Key,Value)->
  redis_q(PoolName,?INCRBY(Key,Value),?REDIS_TIMEOUT).

%%get,set
set(Key,Value)->
  set(?REDIS_DEFAULT_POOL,Key,Value).

set(PoolName,Key,Value)->
  redis_q(PoolName, ?SET(Key,Value), ?REDIS_TIMEOUT).

get(Key)->
  get(?REDIS_DEFAULT_POOL,Key).

get(PoolName,Key)->
  redis_q(PoolName, ?GET(Key), ?REDIS_TIMEOUT).

%%del
del(Key)->
  del(?REDIS_DEFAULT_POOL,Key).

del(PoolName,Key)->
  redis_q(PoolName, ?DEL(Key), ?REDIS_TIMEOUT).

%%hget,hset
hget(Key,Field)->
  hget(?REDIS_DEFAULT_POOL,Key,Field).

hget(PoolName,Key,Field)->
  redis_q(PoolName,?HGET(Key,Field),?REDIS_TIMEOUT).

hset(Key,Field,Value)->
  hset(?REDIS_DEFAULT_POOL,Key,Field,Value).

hset(PoolName,Key,Field,Value)->
  redis_q(PoolName,?HSET(Key,Field,Value),?REDIS_TIMEOUT).

%%hmget,hmset
hmget(Key,Fields)->
  hmget(?REDIS_DEFAULT_POOL,Key,Fields).

hmget(PoolName,Key,Fields)->
  redis_q(PoolName,?HMGET(Key,Fields),?REDIS_TIMEOUT).

%%之所以没有fields是因为eredis中的hmset中的field和value是连续在一起的
%%类似["HMSET",KEY,FIELD1,VALUE1,FIELD2,VALUE2...]
hmset(Key,Values)->
  hmset(?REDIS_DEFAULT_POOL,Key,Values).

hmset(PoolName,Key,Values)->
  redis_q(PoolName,?HMSET(Key,Values),?REDIS_TIMEOUT).

%%hdel
hdel(Key,Fields)->
  hdel(?REDIS_DEFAULT_POOL,Key,Fields).

hdel(PoolName,Key,Fields)->
  redis_q(PoolName,?HDEL(Key,Fields),?REDIS_TIMEOUT).

%%hgetall
hgetall(Key)->
  hgetall(?REDIS_DEFAULT_POOL,Key).

hgetall(PoolName,Key)->
  case redis_q(PoolName,?HGETALL(Key),?REDIS_TIMEOUT) of
    {ok, List} ->
      util:conver_list_to_kvlist(List);
    {error,_}=E ->
      undefined
  end.

%%exists
exists(Key)->
  exists(?REDIS_DEFAULT_POOL,Key).

exists(PoolName,Key)->
  redis_q(PoolName,?EXISTS(Key),?REDIS_TIMEOUT).

%%hincr
%%http://www.yiibai.com/redis/hashes_hincrby.html
hincrby(Key,Field)->
  hincrby(?REDIS_DEFAULT_POOL,Key,Field).

hincrby(Pool,Key,Field)->
  hincrby(Pool,Key,Field,1).

hincrby(Pool,Key,Field,Value)->
  case redis_q(Pool,?HINCRBY(Key,Field,Value),?REDIS_TIMEOUT) of
    {ok, Result}->binary_to_integer(Result);
    {error,_}->
      undefined
  end.

%%lrange
%%http://www.yiibai.com/redis/lists_lrange.html
lrange(Key,Start,End)->
  lrange(?REDIS_DEFAULT_POOL,Key,Start,End).

lrange(Pool,Key,Start,End)->
  redis_q(Pool,?LRANGE(Key,Start,End),?REDIS_TIMEOUT).

%%lpush
%%http://www.yiibai.com/redis/lists_lpush.html
lpush(Key,Values) when is_list(Values)->
  lpush(?REDIS_DEFAULT_POOL,Key,Values);

lpush(Key,Values)->
  redis_q(?REDIS_DEFAULT_POOL,["LPUSH",Key,Values],?REDIS_TIMEOUT).

lpush(Pool,Key,Values) when is_list(Values)->
  redis_q(Pool,?LPUSH(Key,Values),?REDIS_TIMEOUT);

lpush(Pool,Key,Values)->
  redis_q(Pool,["LPUSH",Key,Values],?REDIS_TIMEOUT).

%%rpop
%%http://www.yiibai.com/redis/lists_rpop.html
rpop(Key)->
  rpop(?REDIS_DEFAULT_POOL,Key).

rpop(Pool,Key)->
  redis_q(Pool,?RPOP(Key),?REDIS_TIMEOUT).

%%llen
%%http://www.yiibai.com/redis/lists_llen.html
llen(Key)->
  llen(?REDIS_DEFAULT_POOL).

llen(Pool,Key)->
  redis_q(Pool,?LLEN(Key),?REDIS_TIMEOUT).

%%zscore
%%http://www.yiibai.com/redis/sorted_sets_zscore.html
zscore(Key,Member)->
  zscore(?REDIS_DEFAULT_POOL,Key,Member).

zscore(PoolName,Key,Member)->
  redis_q(PoolName,?ZSCORE(Key,Member),?REDIS_TIMEOUT).

%%zadd
%%http://www.yiibai.com/redis/sorted_sets_zadd.html
%%参数中的values指的是[Score,Member....]这样的键值对
zadd(Key,Values)->
  zadd(?REDIS_DEFAULT_POOL,Key,Values).

zadd(PoolName,Key,Values)->
  redis_q(PoolName,?ZADD(Key,Values),?REDIS_TIMEOUT).

%%zincr
%%http://www.yiibai.com/redis/sorted_sets_zincrby.html
zincr(Key,AddValue,Member)->
  zincr(?REDIS_DEFAULT_POOL,Key,AddValue,Member).

zincr(PoolName,Key,AddValue,Member)->
  redis_q(PoolName,?ZINCRBY(Key,AddValue,Member),?REDIS_TIMEOUT).

%%zrange
%%http://www.yiibai.com/redis/sorted_sets_zrange.html
zrange(Key,Start,End)->
  zrange(?REDIS_DEFAULT_POOL,Key,Start,End).

zrange(PoolName,Key,Start,End)->
  redis_q(PoolName,?ZRANGE(Key,Start,End),?REDIS_TIMEOUT).

%%redis query
-spec redis_q(PoolName::atom(), Command::iolist(), TimeOut::integer()) ->
  {ok, binary()} | {error, Reason::binary()}.
redis_q(PoolName,QueryList,TimeOut)->
  case eredis_pool:q(PoolName, QueryList, TimeOut) of
    {ok, Value} -> {ok, Value};
    %%日后加入log
    {error, _}=E -> E
  end.

%%redis pipeline
-spec redis_qp(PoolName::atom(), Pipeline::[iolist()], TimeOut::integer()) ->
  {ok, binary()} | {error, Reason::binary()}.
redis_qp(PoolName,Pipeline,TimeOut)->
  eredis_pool:qp(PoolName, Pipeline, TimeOut).
