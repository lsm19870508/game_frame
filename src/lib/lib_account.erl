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
-include("db_config.hrl").
-include("error_log.hrl").
-author("Administrator").

%% API
-export([is_token_valid/2]).
-export([do_register/2]).
-export([do_login/2]).
-export([do_fast_login/1]).
-export([update_acc_pass/3]).
-export([prepare_quries/0]).

%%初始化prepare某些sql
prepare_quries()->
  ok = emysql:prepare(pay_report_insert,<<"insert into pay_report (pay_id,province,phone,age,sex,alipay,wechat,bank_name,bank_number,ip,time) values(?,?,?,?,?,?,?,?,?,?,?)">>),
  ok.

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
      Values = ["id",AccountId,"accountname",AccountName,"password",Password, "is_register","1","token",Token,"secret",<<"0">>,
        "secret_answer",<<"0">>,"phoneno",<<"0">>,"secrettime",<<"0">>],
      RedisIdKey = <<?REDIS_TB_ACCOUNT/binary,":",SzAccountId/binary>>,
      redis:redis_qp(?REDIS_DEFAULT_POOL,[?HMSET(RedisKey,Values),?HMSET(RedisIdKey,Values)],?REDIS_TIMEOUT),
      {0,SzAccountId,CurrTime,Token};
    true->
      {Ret,0,0,<<"12345678901234567890123456789012">>}
  end.

%%进行登录
do_login(AccountName,Password) when is_list(AccountName)->
  SzAccount = conversion_utility:to_binary(AccountName),
  do_login(SzAccount,Password);

do_login(AccountName,Password)->
  put(ret,0),
  %%组合rediskey
  RedisKey = <<?REDIS_TB_ACCOUNTNAME_ACCOUNT/binary,":",AccountName/binary>>,
  {ok,[AccountId,Pass]} = redis:hmget(RedisKey,[<<"id">>,<<"password">>]),
  %%是否存在账号
  if
    (AccountId==undefined) -> put(ret,1);
    true -> ok
  end,
  %%密码是否正确
  if
    (Password /= Pass) -> put(ret,2);
    true -> ok
  end,
  RetCode = get(ret),
  case RetCode of
    0->
      CurrTime = time_utility:unixtime(),
      IntAccountId = conversion_utility:to_integer(AccountId),
      Token = calc_token(IntAccountId,CurrTime),
      {0,AccountId,CurrTime,Token};
    1->
      {1,0,0,<<"12345678901234567890123456789012">>};
    2->
      {2,0,0,<<"12345678901234567890123456789012">>};
    _->
      ?LOG_ERROR("Logic error!!!class [lib_account] [do_login]!! "),
      {RetCode,0,0,<<"12345678901234567890123456789012">>}
  end.

%%快速登录，使用设备码同时做为用户名和密码
do_fast_login(Imei)->
  put(ret,0),
  %%组合rediskey
  RedisKey = <<?REDIS_TB_ACCOUNTNAME_ACCOUNT/binary,":",Imei/binary>>,
  {ok,[AccountId,_Password]} = redis:hmget(RedisKey,[<<"id">>,<<"password">>]),
  case AccountId of
    undefined->
      {ok,SzAccountId} = redis:incr(?REDIS_TB_ACCOUNT_INCR),
      AccountId1 = util:to_integer(SzAccountId),
      CurrTime = time_utility:unixtime(),
      Token = calc_token(AccountId1,CurrTime),
      Values = ["id",AccountId1,"accountname",Imei,"password",Imei, "is_register","0","token",Token,"secret",<<"0">>,
        "secret_answer",<<"0">>,"phoneno",<<"0">>,"secrettime",<<"0">>],
      RedisIdKey = <<?REDIS_TB_ACCOUNT/binary,":",SzAccountId/binary>>,
      redis:redis_qp(?REDIS_DEFAULT_POOL,[?HMSET(RedisKey,Values),?HMSET(RedisIdKey,Values)],?REDIS_TIMEOUT),
      {0,SzAccountId,CurrTime,Token};
    _->
      CurrTime = time_utility:unixtime(),
      Token = calc_token(AccountId,CurrTime),
      {0,AccountId,CurrTime,Token}
  end.

%%更新用户名密码
update_acc_pass(AccountId,AccountName,Password)->
  put(ret,0),
  RedisIdKey = <<?REDIS_TB_ACCOUNT/binary,":",AccountId/binary>>,
  RedisKey = <<?REDIS_TB_ACCOUNTNAME_ACCOUNT/binary,":",AccountName/binary>>,
  %%判断用户名是否已经使用过，是否重名
  {ok,[OldAccountName,IsRegister,Secret,SecretAnswer,Phoneno,SecretTime]} = redis:hmget(RedisIdKey,[<<"accountname">>,
    <<"is_register">>, <<"secret">>,<<"secret_answer">>,<<"phoneno">>,<<"secrettime">>]),
  if
    IsRegister -> put(ret,1);
    true -> ok
  end,
  %%判断用户是否已经补全过
  if
    (IsRegister == 1) -> put(ret,2);
    true -> ok
  end,
  %%判断用户名是否符合命名规范
  Regex = "^[a-zA-Z0-9_@\\.]{3,20}$",
  CanMatch = re:run(AccountName,Regex),
  if
    (CanMatch == nomatch)->
      put(ret,3);
    true->
      ok
  end,
  RetCode = get(ret),
  if
    RetCode==0 ->
      Values = ["id",AccountId,"accountname",AccountName,"password",Password,"is_register","1"],
      Values1 = ["id",AccountId,"accountname",AccountName,"password",Password,"is_register","1",
        "secret",Secret,"secret_answer",SecretAnswer,"phoneno",Phoneno,"secrettime",SecretTime],
      %%删除旧的name key即记录
      RedisOldKey = <<?REDIS_TB_ACCOUNTNAME_ACCOUNT/binary,":",OldAccountName/binary>>,
      DeleteQuery = ?DEL(RedisOldKey),
      %%插入新的key
      UpdateQuery = ?HMSET(RedisIdKey,Values),
      UpdateNameQuery = ?HMSET(RedisKey,Values1),
      redis:redis_qp(?REDIS_DEFAULT_POOL,[DeleteQuery,UpdateQuery,UpdateNameQuery],?REDIS_TIMEOUT);
    true ->
      ok
  end,
  RetCode.

%%token计算方法
calc_token(AccountId,Time)->
  Encode = integer_to_list(AccountId*Time) ++ ?TICKET,
  list_to_binary(util:md5(Encode)).
calc_token(AccountId,Time,ClubID)->
  Encode = integer_to_list(AccountId)++integer_to_list(Time)++integer_to_list(ClubID) ++ ?TICKET,
  list_to_binary(util:md5(Encode)).
