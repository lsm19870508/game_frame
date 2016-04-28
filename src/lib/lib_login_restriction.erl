-module(lib_login_restriction).

-include("config_keys.hrl").

%% API
-export([check_login/1]).

%% 检测该用户是否可以登陆(true/false)
check_login(OpenID) ->
  % 白名单检测
  case not baba3_config:lookup_keys([?CF_APP, <<"login_restriction">>]) of
    true ->
      true;
    false ->
      LoginWhiteList = get_login_white(),
      lists:member(OpenID, LoginWhiteList)
  end.

% 获取白名单
get_login_white() ->
  {ok, Binary} = file:read_file(fetch_config_path() ++ "/login_white.txt"),
  binary:split(Binary, [<<"\n">>, <<"\r">>], [global]).

fetch_config_path() ->
  case init:get_argument(config_path) of
    {ok, [[Path]]} ->
      Path;
    _ ->
      {ok, [[ConfigFile]]} = init:get_argument(config),
      filename:dirname(ConfigFile)
  end.