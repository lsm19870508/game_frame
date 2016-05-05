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
      {ok,#{"accountid"=>AccountId,"unixtime"=>UnixTime,"token"=>Token}};
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
      {ok,#{"accountid"=>AccountId,"unixtime"=>UnixTime,"token"=>Token}};
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
      {ok,#{"accountid"=>AccountId,"unixtime"=>UnixTime,"token"=>Token}};
    _->
      ErrorCode = ?MAKE_ERROR_CODE(60002,CanLogin),
      {error,ErrorCode}
  end;


handle(_Code,_Common,_Data)->
  request_dispatcher:routing_fail(_Code).
