%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 四月 2016 19:01
%%%-------------------------------------------------------------------
-module(lib_weak).
-author("Administrator").

%% API
%% API
-export([check_srq/3,save_srq/4]).

-define(TYPE_NAME, <<"SRQ">>).

check_srq(_, 10001,_) ->
  undefined;
check_srq(AccountId,Code,Srq) ->
  case get_srq_by_accountid(?TYPE_NAME, AccountId) of
    undefined ->
      undefined;
    Map ->
      OldCode = maps:get(<<"code">>, Map, 0),
      OldSrq = maps:get(<<"srq">>, Map, 0),
      case OldCode =:= Code andalso OldSrq =:=Srq of
        true ->
          maps:get(<<"result">>, Map);
        false ->
          undefined
      end
  end.

save_srq(_AccountId,10001,_Srq,_Result) ->
  ok;
save_srq(AccountId,Code,Srq,Result) ->
  KVList = [<<"code">>,Code,<<"srq">>,Srq,<<"result">>,Result],
  save_srq_by_accountid(?TYPE_NAME, AccountId, KVList).

model_key(Type, Id) ->
  [Type, Id].

get_srq_by_accountid(Type, Id)->
  Key = model_key(Type, Id),
  case redis:hgetall(Key) of
    {ok, []} ->
      Map = #{
        <<"code">> => 0,
        <<"srq">> => 0,
        <<"result">> => #{}
      },
      Map;
    {ok, List} ->
      conversion_utility:convert_list_to_map(List)
  end.

save_srq_by_accountid(Type, Id, KVList)->
  Key = model_key(Type, Id),
  redis:hmset(Key,KVList).

