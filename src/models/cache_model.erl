%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 三月 2016 15:12
%%%-------------------------------------------------------------------
-module(cache_model).
-author("Administrator").

%% API
-export([init_sciense_cache/0,init_user_cache/0,get_sciense_cache/2]).

%%
init_sciense_cache()->
  %%List = mysql_model:get_all("select * from scisence"),
  ok.

init_user_cache()->
  ok.

get_sciense_cache(Type,Level)->
  ok.