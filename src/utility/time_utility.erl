%%%-------------------------------------------------------------------
%%% @author 李世铭
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 关于时间的部分处理函数
%%% @end
%%% Created : 05. 四月 2016 14:03
%%%-------------------------------------------------------------------
-module(time_utility).
-author("Lsm").

%% API
-export([unixtime/0,longunixtime/0]).
-export([zerounixtime/0,zerounixtime/1]).
-export([yzerounixtime/0]).

%% 取得当前的unix时间戳
unixtime() ->
  {M, S, _} = erlang:now(),
  M * 1000000 + S.

longunixtime() ->
  {M, S, Ms} = erlang:now(),
  M * 1000000000 + S*1000 + Ms div 1000.

%%获取当天0点的unixtime
zerounixtime()->
  T = erlang:now(),
  {M,S,_} = T,
  {{_,_,_},{HH,MM,SS}} = calendar:now_to_local_time(T),
  Time = HH * 3600 + MM * 60 +SS,
  Time1 = M * 1000000 + S,
  Time2 = Time1 - Time.

zerounixtime(TimeStap)->
  M = TimeStap div 1000000,
  S = TimeStap rem 1000000,
  T = {M,S,0},
  {{_,_,_},{HH,MM,SS}} = calendar:now_to_local_time(T),
  Time = HH * 3600 + MM * 60 +SS,
  Time2 = TimeStap - Time.

%%获取昨天0点的unixtime
yzerounixtime()->
  T = erlang:now(),
  {M,S,_} = T,
  {{_,_,_},{HH,MM,SS}} = calendar:now_to_local_time(T),
  Time = HH * 3600 + MM * 60 +SS,
  Time1 = M * 1000000 + S,
  Time2 = Time1 - Time - 86400.