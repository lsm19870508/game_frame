%%%-------------------------------------------------------------------
%%% @author 李世铭
%%% @email 240782361@qq.com
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 存储一些常用的工具函数
%%% @end
%%% Created : 25. 三月 2016 16:24
%%%-------------------------------------------------------------------
-module(util).
-author("Administrator").

%% API
-export([pid_to_binary/1,binary_to_pid/1]).
-export([conver_list_to_kvlist/1]).
-export([term_to_string/1,string_to_term/1]).
-export([term_to_bitstring/1,bitstring_to_term/1]).
-export([set_bytes/2,get_bytes/2]).
-export([to_atom/1,to_binary/1,to_integer/1,to_float/1,to_bool/1,to_list/1,to_tuple/1]).
-export([get_type/2]).
-export([is_string/1,is_string/2]).
-export([ceil/1,floor/1]).
-export([md5/1]).
-export([list_to_hex/1]).
-export([list_to_atom2/1]).
-export([get_msg_queue/0]).
-export([get_memory/0,get_memory/1]).
-export([get_heap/0,get_heap/1]).
-export([get_processes/0]).

%%将pid转换为binary
-spec(pid_to_binary(Pid::pid()) -> binary()).
pid_to_binary(Pid) when is_pid(Pid)->
  list_to_binary(pid_to_list(Pid)).

%%将binary转换为pid
-spec(binary_to_pid(BitString::binary()) -> pid()).
binary_to_pid(BitString) when is_binary(BitString)->
  list_to_pid(binary_to_list(BitString)).

%%将list  [k1,v1,k2,v2...]转换成kvlist [{k1,v1},{k2,v2},....]
-spec(conver_list_to_kvlist(List::list()) -> list()).
conver_list_to_kvlist(List) when is_list(List)->
  F = fun(X,{IsPass,Key,Result})->
        if
          not(IsPass)-> {true,X,Result};
          true -> {false,{},[{Key,X}] ++ Result}
        end
      end,
  {_,_,Return} = lists:foldl(F,{false,{},[]},List),
  Return.

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
  binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
  case erl_scan:string(String++".") of
    {ok, Tokens, _} ->
      case erl_parse:parse_term(Tokens) of
        {ok, Term} -> Term;
        _Err -> undefined
      end;
    _Error ->
      undefined
  end.

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
  erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
  string_to_term(binary_to_list(BitString)).

%%设置比特位
set_bytes(Temp,Order)->
  Temp bor (1 bsl (Order-1)).

%%取比特位
get_bytes(Temp,Order)->
  (Temp bsr (Order-1)) band 1.

%% @doc convert other type to atom
to_atom(Msg) when is_atom(Msg) ->
  Msg;
to_atom(Msg) when is_binary(Msg) ->
  tool:list_to_atom2(binary_to_list(Msg));
to_atom(Msg) when is_list(Msg) ->
  tool:list_to_atom2(Msg);
to_atom(_) ->
  throw(other_value).  %%list_to_atom("").

%% @doc convert other type to list
to_list(Msg) when is_list(Msg) ->
  Msg;
to_list(Msg) when is_atom(Msg) ->
  atom_to_list(Msg);
to_list(Msg) when is_binary(Msg) ->
  binary_to_list(Msg);
to_list(Msg) when is_integer(Msg) ->
  integer_to_list(Msg);
to_list(Msg) when is_float(Msg) ->
  f2s(Msg);
to_list(_) ->
  throw(other_value).

%% @doc convert other type to binary
to_binary(Msg) when is_binary(Msg) ->
  Msg;
to_binary(Msg) when is_atom(Msg) ->
  list_to_binary(atom_to_list(Msg));
%%atom_to_binary(Msg, utf8);
to_binary(Msg) when is_list(Msg) ->
  list_to_binary(Msg);
to_binary(Msg) when is_integer(Msg) ->
  list_to_binary(integer_to_list(Msg));
to_binary(Msg) when is_float(Msg) ->
  list_to_binary(f2s(Msg));
to_binary(_Msg) ->
  throw(other_value).

%% @doc convert other type to float
to_float(Msg)->
  Msg2 = to_list(Msg),
  list_to_float(Msg2).

%% @doc convert other type to integer
-spec to_integer(Msg :: any()) -> integer().
to_integer(Msg) when is_integer(Msg) ->
  Msg;
to_integer(Msg) when is_binary(Msg) ->
  Msg2 = binary_to_list(Msg),
  list_to_integer(Msg2);
to_integer(Msg) when is_list(Msg) ->
  list_to_integer(Msg);
to_integer(Msg) when is_float(Msg) ->
  round(Msg);
to_integer(_Msg) ->
  throw(other_value).

to_bool(D) when is_integer(D) ->
  D =/= 0;
to_bool(D) when is_list(D) ->
  length(D) =/= 0;
to_bool(D) when is_binary(D) ->
  to_bool(binary_to_list(D));
to_bool(D) when is_boolean(D) ->
  D;
to_bool(_D) ->
  throw(other_value).

%% @doc convert other type to tuple
to_tuple(T) when is_tuple(T) -> T;
to_tuple(T) -> {T}.

%% @doc get data type {0=integer,1=list,2=atom,3=binary}
get_type(DataValue,DataType)->
  case DataType of
    0 ->
      DataValue2 = binary_to_list(DataValue),
      list_to_integer(DataValue2);
    1 ->
      binary_to_list(DataValue);
    2 ->
      DataValue2 = binary_to_list(DataValue),
      list_to_atom(DataValue2);
    3 ->
      DataValue
  end.

%% @spec is_string(List)-> yes|no|unicode
is_string([]) -> yes;
is_string(List) -> is_string(List, non_unicode).

is_string([C|Rest], non_unicode) when C >= 0, C =< 255 -> is_string(Rest, non_unicode);
is_string([C|Rest], _) when C =< 65000 -> is_string(Rest, unicode);
is_string([], non_unicode) -> yes;
is_string([], unicode) -> unicode;
is_string(_, _) -> no.

%% @doc 取整 大于X的最小整数
ceil(X) ->
  T = trunc(X),
  if
    X - T == 0 ->
      T;
    true ->
      if
        X > 0 ->
          T + 1;
        true ->
          T
      end
  end.


%% @doc 取整 小于X的最大整数
floor(X) ->
  T = trunc(X),
  if
    X - T == 0 ->
      T;
    true ->
      if
        X > 0 ->
          T;
        true ->
          T-1
      end
  end.

md5(S) ->
  Md5_bin =  erlang:md5(S),
  Md5_list = binary_to_list(Md5_bin),
  lists:flatten(list_to_hex(Md5_list)).

list_to_hex(L) ->
  lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].
hex(N) when N < 10 ->
  $0+N;
hex(N) when N >= 10, N < 16 ->
  $a + (N-10).

list_to_atom2(List) when is_list(List) ->
  case catch(list_to_existing_atom(List)) of
    {'EXIT', _} -> erlang:list_to_atom(List);
    Atom when is_atom(Atom) -> Atom
  end.

get_msg_queue() ->
  io:fwrite("process count:~p~n~p value is not 0 count:~p~nLists:~p~n",
    get_process_info_and_zero_value(message_queue_len) ).

get_memory() ->
  io:fwrite("process count:~p~n~p value is large than ~p count:~p~nLists:~p~n",
    get_process_info_and_large_than_value(memory, 1048576) ).

get_memory(Value) ->
  io:fwrite("process count:~p~n~p value is large than ~p count:~p~nLists:~p~n",
    get_process_info_and_large_than_value(memory, Value) ).

get_heap() ->
  io:fwrite("process count:~p~n~p value is large than ~p count:~p~nLists:~p~n",
    get_process_info_and_large_than_value(heap_size, 1048576) ).

get_heap(Value) ->
  io:fwrite("process count:~p~n~p value is large than ~p count:~p~nLists:~p~n",
    get_process_info_and_large_than_value(heap_size, Value) ).

get_processes() ->
  io:fwrite("process count:~p~n~p value is large than ~p count:~p~nLists:~p~n",
    get_process_info_and_large_than_value(memory, 0) ).