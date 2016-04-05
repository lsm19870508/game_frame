%%%-------------------------------------------------------------------
%%% @author 李世铭
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 测试数据库各种写方法的效率
%%% @end
%%% Created : 05. 四月 2016 16:48
%%%-------------------------------------------------------------------
-module(db_test).
-author("Administrator").

%% API
-export([test_db_multi_write/1,test_prepare_write/1,test_directly_write/1]).
-export([test_directly_select/1,test_prepare_select/1]).

test_db_multi_write(N)->
  CurrTime = time_utility:longunixtime(),

  L = lists:seq(1,N),
  F = fun(X,Res)->
        Rand = util:rand(1,1000000),
        Sql = mysql:make_insert_sql(account,["id"],[Rand]),
        SzSql = list_to_binary(Sql),
        case Res==<<"">> of
          true->
            SzSql;
          _->
            <<Res/binary,";",SzSql/binary>>
        end
      end,
  FinalSql = lists:foldl(F,<<>>,L),
  Result  = emysql:execute(default,FinalSql),
  EndTime = time_utility:longunixtime(),
  io:format("Cost time is:~w~n",[{EndTime - CurrTime}]).

test_prepare_write(N)->
  CurrTime = time_utility:longunixtime(),
  L = lists:seq(1,N),
  F = fun(X)->
    Rand = util:rand(1,10000),
    emysql:execute(default,account_replace,[Rand])
      end,
  [F(X) || X<-L],
  EndTime = time_utility:longunixtime(),
  io:format("Cost time is:~w~n",[{EndTime - CurrTime}]).

test_directly_write(N)->
  CurrTime = time_utility:longunixtime(),
  L = lists:seq(1,N),
  F = fun(X)->
    Rand = util:rand(1,10000),
    Sql = mysql:make_replace_sql(account,["id"],[Rand]),
    emysql:execute(default,Sql)
  end,
  [F(X) || X<-L],
  EndTime = time_utility:longunixtime(),
  io:format("Cost time is:~w~n",[{EndTime - CurrTime}]).

test_directly_select(N)->
  CurrTime = time_utility:longunixtime(),
  L = lists:seq(1,N),
  F = fun(X)->
    Sql = <<"select * from account where id=1">>,
    emysql:execute(default,Sql)
    end,
  [F(X) || X<-L],
  EndTime = time_utility:longunixtime(),
  io:format("Cost time is:~w~n",[{EndTime - CurrTime}]).

test_prepare_select(N)->
  emysql:prepare(account_select,"select * from account where id=?"),
  CurrTime = time_utility:longunixtime(),
  L = lists:seq(1,N),
  F = fun(X)->
    emysql:execute(default,account_select,[1])
      end,
  [F(X) || X<-L],
  EndTime = time_utility:longunixtime(),
  io:format("Cost time is:~w~n",[{EndTime - CurrTime}]).