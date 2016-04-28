-module(request_dispatcher).

-export([dispatch/1]).
-export([routing_fail/1]).

-include("status_code.hrl").
-include("field.hrl").
-include("error_log.hrl").
-include("config_keys.hrl").

-define(COMMON_REQUIRED_FIELDS, [
  {?CODE, ?STATUS_SRQ_MISSING},
  {?DATA, ?STATUS_SRQ_MISSING},
  {?SRQ, ?STATUS_SRQ_MISSING},
  {?DB_PLAT_ID, ?STATUS_PLAT_ID_MISSING},
  {?VERSION, ?STATUS_VERSION_MISSING},
  {?TIMESTAMP, ?STATUS_TIMESTAMP_MISSING},
  {?DB_ACCOUNT_ID, ?STATUS_ACCOUNTID_MISSING},
  {?DB_ACCOUNT_TOKEN, ?STATUS_ACCTOKEN_MISSING},
  {?CLIENTIP, ?STATUS_CLEINTIP_MISSING},
  {?CHANNEL, ?STATUS_CHANNEL_MISSION}
]).

dispatch(Map) ->
  case slice_common(Map, ?COMMON_REQUIRED_FIELDS, []) of
    {ok, CommonData} ->
      Code = maps:get(?CODE, CommonData),
      Data = maps:get(?DATA, CommonData),
      dispatch_with_data(Code, CommonData, Data, 1);
    {error, Status} ->
      respond_with_response(Status, #{}, 0)
  end.

dispatch_with_data(Code, Common, Data, RetryCount) ->
  StartTime = erlang:localtime(),
  try
    %%dispatch_with_data_step_1(Code, Common, Data, RetryCount)
    dispatch_with_data(Code,Common,Data)
  catch
    Type:Error ->
      Stacktrace = erlang:get_stacktrace(),
      ?LOG_ERROR("getTime:~p, returnTime:~p, Code:~p, throw_type:~p, reason:~p, stacktrace:~p, Data:~p, Common:~p",
      [StartTime, erlang:localtime(), Code, Type, Error, Stacktrace, Data, Common]),
      respond_with_response(?STATUS_SYSTEM_ERROR, #{}, Code)
  end.

dispatch_with_data_step_1(10001, Common, Data, RetryCount) ->
  OpenID = maps:get(?DB_OPEN_ID, Data),
  PlatID = maps:get(?DB_PLAT_ID, Common),
  % 获取账户的数据
  UserData = user_model:get_user_data(OpenID, PlatID),
  % 先进行登录操作
  dispatch_with_data_step_2(10001, Common, Data, UserData, RetryCount);

dispatch_with_data_step_1(Code, Common, Data, RetryCount) ->
  AccountId = maps:get(?DB_ACCOUNT_ID, Common),
  UserData = user_model:get_user_data(AccountId),
  case  is_map(UserData) of
    true ->
      OpenID = maps:get(?DB_OPEN_ID, UserData),
      % 停服检测
      case lib_login_restriction:check_login(OpenID) of
        false ->
          respond_with_response(?STATUS_SERVER_STOP, #{}, 10001);
        true ->
          % 弱网络机制
          Srq = maps:get(?SRQ, Common),
          case lib_weak:check_srq(AccountId,Code,Srq) of
            undefined ->
              Response= dispatch_with_data_step_2(Code, Common, Data, UserData, RetryCount),
              lib_weak:save_srq(AccountId,Code,Srq, Response),
              Response;
            R -> % srq重复了
              R
          end
      end;
    false ->
      ?LOG_ERROR("userdata is error,Code:~p,Common:~p,Data:~p, userdata:~p", [Code, Common, Data, UserData])
  end.


dispatch_with_data_step_2(Code, Common, Data, UserData, RetryCount) ->
  case dispatch_to_processor(Code, Common, Data, UserData) of
    {ok, Result} ->
%%      case baba3_db:commite() of
%%        ok ->
          respond_with_response(?STATUS_OK, Result, Code);
%%        R -> % 保存失败
%%          % 暂停一段时间再重试，降低cas的概率
%%          Sleep = random_utility:random(100, 500),
%%          timer:sleep(Sleep),
%%          userdata_save_fail(Code, Common, Data, R, RetryCount)
%%      end;
    {error, Status} ->
      ?LOG_ERROR("code:~p, common:~p, data:~p, status:~p", [Code, Common, Data, Status]),
      respond_with_response(?STATUS_SYSTEM_ERROR, #{}, Code)
  end.

%%账号信息走这里
dispatch_with_data(Code, Common, Data) when (Code div 1000 == 60)->
  pp_account:handle(Code,Common,Data);

%%其他信息走这里
dispatch_with_data(Code, Common, Data)->
  AccountId = maps:get(?DB_ACCOUNT_ID, Common),
  AccountToken = maps:get(?DB_ACCOUNT_TOKEN, Common),
  % 停服检测
  case lib_login_restriction:check_login(AccountId) of
    false ->
      respond_with_response(?STATUS_SERVER_STOP, #{}, 10001);
    true ->
      % 弱网络机制
      Srq = maps:get(?SRQ, Common),
      case lib_weak:check_srq(AccountId,Code,Srq) of
        undefined ->
          %%验证token是否有效
          case lib_account:is_token_valid(AccountId,AccountToken) of
            false->
              #{<<"code">> => 0, <<"status">> => ?STATUS_ACCOUNT_TOKEN_EXPIRE};
            true->
              case routing(Code, Common, Data) of
                {ok, Result} ->
                  respond_with_response(?STATUS_OK, Result, Code);
                {error, Status} ->
                  ?LOG_ERROR("code:~p, common:~p, data:~p, status:~p", [Code, Common, Data, Status]),
                  respond_with_response(Status, #{}, Code)
              end
          end;
        R -> % srq重复了
          R
      end
  end.

%% 路由
%%cmd:命令号
%%Common:总包
%%Data:数据包
routing(Code, Common, Data)->
  Protrol = Code div 1000,
  case Protrol of
    10->
      ok;
    60->
      pp_account:handle(Code, Common, Data);%%账号信息处理
    %%错误处理
    _ ->
      routing_fail(Code)
  end.

%%记录并返回无法解析的路由
routing_fail(Code)->
  #{<<"code">> => 0, <<"status">> => ?STATUS_PARA_ILLEGAL},
  ?LOG_ERROR("Routing error,Routing Code is:~w~n",[{Code}]).

%% 数据保存失败,重试机制
userdata_save_fail(Code, Common, Data, ErrorInfo, RetryCount) when RetryCount > 3 ->
  ?LOG_ERROR("save error:code:~p, Common:~p, Data:~p, Count:~p, reason:~p",
    [Code, Common, Data, RetryCount, ErrorInfo]),
  respond_with_response(?STATUS_SAVE_ERROR, #{}, Code);
userdata_save_fail(Code, Common, Data, _ErrorInfo, RetryCount) ->
  dispatch_with_data(Code, Common, Data, RetryCount + 1).

%% 检测必须参数是否齐全
slice_common(_, [], Result) ->
  {ok, maps:from_list(Result)};
slice_common(Data, [{Key, ErrorCode} | Tail], Result) ->
  Val = maps:get(Key, Data, undefined),
  case Val of
    undefined ->
      {error, ErrorCode};
    Val2 ->
      slice_common(Data, Tail, [{Key, Val2} | Result])
  end.

respond_with_response(Status = ?STATUS_OK, Result, Code)->
  #{?CODE => Code, ?STATUS => Status,?RESULT => Result};
%% 其他错误
respond_with_response(Status, _, Code) ->
  #{?CODE => Code, ?STATUS => Status,?RESULT => #{}}.


%% 请求分发接口，请求处理部分接口定义请参见status_processor, 注意
%% 在增加新的接口处理函数时，需要在这里添加相应的分发代码
%%% 账户相关协议
dispatch_to_processor(Code, Common, Data, UserData) when (Code div 1000 == 10) ->
  user_processor:process(Code,Common, Data, UserData);

dispatch_to_processor(_, _, _, _) ->
  {error, ?STATUS_UNKNOWN_CODE}.