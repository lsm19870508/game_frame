%%%-------------------------------------------------------------------
%%% @author 李世铭
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 账号相关的接口信息
%%% @end
%%% Created : 20. 四月 2016 16:41
%%%-------------------------------------------------------------------
-module(pp_account).
-include("status_code.hrl").
-include("record.hrl").
-author("Lsm").

%% API
-export([handle/3]).

%%token校验
handle(60000,Common,Data)->
  ok;

%%注册
handle(60001,Common,Data)->
  AccountName = maps:get(<<"accountname">>,Data),
  Password = maps:get(<<"password">>,Data),
  {CanLogin,AccountId,UnixTime,Token} = lib_account:do_register(AccountName,Password),
  case CanLogin of
    0->
      {ok,#{<<"accountid">>=>AccountId,<<"unixtime">>=>UnixTime,<<"token">>=>Token}};
    _->
      ErrorCode = ?MAKE_ERROR_CODE(60001,CanLogin),
      {error,ErrorCode}
  end;


%%登录
handle(60002,Common,Data)->
  AccountName = maps:get(<<"accountname">>,Data),
  Password = maps:get(<<"password">>,Data),
  {CanLogin,AccountId,UnixTime,Token} = lib_account:do_login(AccountName,Password),
  case CanLogin of
    0->
      {ok,#{<<"accountid">>=>AccountId,<<"unixtime">>=>UnixTime,<<"token">>=>Token}};
    _->
      ErrorCode = ?MAKE_ERROR_CODE(60002,CanLogin),
      {error,ErrorCode}
  end;

%%快速登录
handle(60003,Common,Data)->
  Imei = maps:get(<<"imei">>,Data),
  {CanLogin,AccountId,UnixTime,Token} = lib_account:do_fast_login(Imei),
  case CanLogin of
    0->
      {ok,#{<<"accountid">>=>AccountId,<<"unixtime">>=>UnixTime,<<"token">>=>Token}};
    _->
      ErrorCode = ?MAKE_ERROR_CODE(60003,CanLogin),
      {error,ErrorCode}
  end;

%%补全账号
handle(60004,Common,Data)->
  AccountId = maps:get(<<"account_id">>, Common),
  AccountName = maps:get(<<"accountname">>,Data),
  Password = maps:get(<<"password">>,Data),
  RetCode = lib_account:update_acc_pass(AccountId,AccountName,Password),
  case RetCode of
    0->
      {ok,#{<<"result">>=>0}};
    _->
      ErrorCode = ?MAKE_ERROR_CODE(60004,RetCode),
      {error,ErrorCode}
  end;

%%保存打款数据
handle(60009,Common,Data)->
  PostTime = maps:get(<<"timestamp">>,Common),
  ClientIp = maps:get(<<"client_ip">>,Common),
  PayId = maps:get(<<"realname">>,Data),
  Phone = maps:get(<<"phone">>,Data),
  Province = maps:get(<<"province">>,Data),
  Age = maps:get(<<"age">>,Data),
  Sex = maps:get(<<"sex">>,Data),
  Wechat = maps:get(<<"wechat">>,Data),
  Alipay = maps:get(<<"alipay">>,Data),
  BankName = maps:get(<<"bank_name">>,Data),
  BankNumber = maps:get(<<"bank_number">>,Data),
  SrqKey = <<PostTime,ClientIp,Phone>>,
  Rs = ets:lookup(ets_srq,SrqKey),
  case Rs==[] of
    true->
      mysql:run_prepare(pay_report_insert,[PayId,Province,Phone,Age,Sex,Alipay,Wechat,BankName,BankNumber,ClientIp,PostTime]),
      ets:insert(ets_srq,#ets_srq_check{id=SrqKey,response = true}),
      {ok,#{<<"result">>=>0}};
    _->
      ErrorCode = ?MAKE_ERROR_CODE(60009,1),
      {error,ErrorCode}
  end;

handle(_Code,_Common,_Data)->
  request_dispatcher:routing_fail(_Code).
