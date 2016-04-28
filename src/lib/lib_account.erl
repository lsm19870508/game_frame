%%%-------------------------------------------------------------------
%%% @author 李世铭
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 账号相关处理
%%% @end
%%% Created : 20. 四月 2016 16:40
%%%-------------------------------------------------------------------
-module(lib_account).
-include("db_table_define.hrl").
-include("platform.hrl").
-author("Administrator").

%% API
-export([is_token_valid/2]).
-export([do_register/2]).

%%token是否有效
is_token_valid(AccountId,Token)->
  RedisToken = redis:hget(?REDIS_TB_ACCOUNT_TOKEN,AccountId),
  (Token == RedisToken).

%%进行注册
do_register(AccountName,Password) when is_list(AccountName)->
  SzAccount = conversion_utility:to_binary(AccountName),
  do_register(SzAccount,Password);

do_register(AccountName,Password)->
  put(ret,0),
  %%判断是否重名
  RedisKey = <<?REDIS_TB_ACCOUNTNAME_ACCOUNT/binary,":",AccountName/binary>>,
  {ok,Account} = redis:hget(RedisKey,<<"id">>),
  case Account of
    undefined->
      ok;
    _->
      put(ret,1)
  end,
  %%判断是否符合命名规范
  Regex = "^[a-zA-Z0-9_@\\.]{3,20}$",
  CanMatch = re:run(AccountName,Regex),
  if
    (CanMatch == nomatch)->
      put(ret,2);
    true->
      ok
  end,
  %%如果符合，则插入新帐号，并且将新帐号的id,token返回给客户端
  Ret = get(ret),
  if
    (Ret == 0)->
      {ok,SzAccountId} = redis:incr(?REDIS_TB_ACCOUNT_INCR),
      AccountId = util:to_integer(SzAccountId),
      CurrTime = time_utility:unixtime(),
      Token = calc_token(AccountId,CurrTime),
      redis:hmset(RedisKey,["id",AccountId,"accountname",AccountName,"password",Password,
        "is_register","1","token",Token,"secret",<<"0">>,"secret_answer",<<"0">>]),
      {0,SzAccountId,CurrTime,Token};
    true->
      {Ret,0,0,<<"12345678901234567890123456789012">>}
  end.

%%token计算方法
calc_token(AccountId,Time)->
  Encode = integer_to_list(AccountId*Time) ++ ?TICKET,
  list_to_binary(util:md5(Encode)).
calc_token(AccountId,Time,ClubID)->
  Encode = integer_to_list(AccountId)++integer_to_list(Time)++integer_to_list(ClubID) ++ ?TICKET,
  list_to_binary(util:md5(Encode)).
