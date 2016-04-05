%%%-------------------------------------------------------------------
%%% @author 李世铭
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 一些自己实现的list函数
%%% @end
%%% Created : 05. 四月 2016 10:30
%%%-------------------------------------------------------------------
-module(list_utility).
-author("Lsm").

%% API
-export([get_rand_value_from_list/2]).

%%get ValueNum of Random Value From List,Returns Random Value List and Left Value List
-spec(get_rand_value_from_list(List::list(),ValueNum::integer())->{list(),list()}).
get_rand_value_from_list(List,ValueNum)->
  L = lists:seq(1,ValueNum),
  F = fun(X,{RandList,LeftList})->
        Len = length(LeftList),
        Random = util:rand(1,Len),
        Value = lists:nth(Random,LeftList),
        {[Value | RandList],lists:delete(Value,LeftList)}
    end,
  lists:foldl(F,{[],List},L).

