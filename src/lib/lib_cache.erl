%%%-------------------------------------------------------------------
%%% @author 李世铭
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% mysql==>redis,缓存的初始化部分
%%% @end
%%% Created : 30. 三月 2016 15:12
%%%-------------------------------------------------------------------
-module(lib_cache).
-include("db_table_define.hrl").
-author("Lsm").

%% API
-export([init_sciense_cache/0,init_user_cache/0,get_sciense_cache/2]).
-export([init_account_cache/0]).

%%
init_sciense_cache()->
  %%List = mysql_model:get_all("select * from scisence"),
  ok.

init_user_cache()->
  ok.

get_sciense_cache(Type,Level)->
  ok.

init_account_cache()->
  Sql = "select * from account",
  List = mysql:get_all(Sql),
  F = fun([Id,AccountName,Password,IsRegister,Secret,SecretAnswer,Phoneno,SecretTime],MaxAccountId)->
        RedisNameKey = <<?REDIS_TB_ACCOUNTNAME_ACCOUNT/binary,":",AccountName/binary>>,
        SzId = util:to_binary(Id),
        RedisIdKey = <<?REDIS_TB_ACCOUNT/binary,":",SzId/binary>>,
        Values = ["id",Id,"accountname",AccountName,"password",Password,"is_register",IsRegister,
          "secret",Secret,"secret_answer",SecretAnswer,"phoneno",Phoneno,"secrettime",SecretTime],
        redis:hmset(RedisNameKey,Values),
        redis:hmset(RedisIdKey,Values),
        case (Id>MaxAccountId) of
          true->
            Id;
          _->
            MaxAccountId
        end
    end,
  MaxAccountId = lists:foldl(F,0,List),
  redis:set(?REDIS_TB_ACCOUNT_INCR,MaxAccountId).