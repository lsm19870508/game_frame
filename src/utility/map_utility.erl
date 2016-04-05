%%% @author czf688@@kunpo.cc
%%% 将maps封装一层，方便日后做一些统一处理

-module(map_utility).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_values/2, get_values/3, get_get/3, get_get/4, gets/2, sets/3, gets/3,
  puts/3, get_count/2, add/3]).
-export([convert_list_to_map/1]).

add(Key, AddVal, Map) ->
  OldVal = maps:get(Key, Map),
  maps:put(Key, OldVal + AddVal, Map).

%% 从Map中取出一个值，然后对该值做values操作
get_values(Key, Map) ->
  Map2 = maps:get(Key,Map),
  maps:values(Map2).

%% 从Map中取出一个值，然后对该值做values操作
get_values(Key, Map, Default) ->
  Map2 = maps:get(Key,Map, Default),
  maps:values(Map2).

%% 从Map中取出一个子Map，然后对子Map做get操作
get_get(Key1, Key2, Map) ->
  Map2 = maps:get(Key1,Map),
  maps:get(Key2, Map2).

%% 从Map中取出一个子Map，然后对子Map做get操作
get_get(Key1, Key2, Map, Default) ->
  Map2 = maps:get(Key1,Map),
  maps:get(Key2, Map2, Default).

%% 根据KeyList值深层取值
gets(List, Map) ->
  Fun =
    fun(Key, InMap) ->
      maps:get(Key, InMap)
    end,
  lists:foldl(Fun, Map, List).

%% 根据KeyList值深层取值
gets([Key], Map, Default) ->
  maps:get(Key, Map, Default);
gets([Key|List], Map, Default) ->
  Value = maps:get(Key, Map, Default),
  case Value =:= Default of
    true ->
      Default;
    false ->
      gets(List, Value, Default)
  end.

%% 根据KeyList深层赋值(暂时没想到怎么用尾递归实现这个功能)
sets([Key|[]], Value, Map) ->
  maps:update(Key, Value, Map);
sets([Key|List], Value, Map) ->
  SubMap = maps:get(Key, Map),
  NewSubMap = sets(List, Value, SubMap),
  maps:update(Key, NewSubMap, Map).

%% 根据KeyList深层赋值(暂时没想到怎么用尾递归实现这个功能)
puts([Key|[]], Value, Map) ->
  maps:put(Key, Value, Map);
puts([Key|List], Value, Map) ->
  SubMap = maps:get(Key, Map, #{}),
  NewSubMap = puts(List, Value, SubMap),
  maps:put(Key, NewSubMap, Map).

%% 获取满足条件的元素数量
get_count(Fun, Map) ->
  maps:fold(
    fun(Key, Vel, InCount) ->
      case Fun(Key, Vel) of
        true ->
          InCount + 1;
        false ->
          InCount
      end
    end, 0, Map).

%%将list  [k1,v1,k2,v2...]转换成map {k1=>v1,key2=>v2...}
%%Author:李世铭
%%April 5th,2016
-spec(convert_list_to_map(List::list()) -> map()).
convert_list_to_map(List) when is_list(List)->
  F = fun(X,{IsPass,Key,Result})->
    if
      not(IsPass)-> {true,X,Result};
      true -> {false,{},Result#{Key=>X}}
    end
      end,
  {_,_,Return} = lists:foldl(F,{false,{},#{}},List),
  Return.

%% ====================================================================
%% Internal functions
%% ====================================================================


