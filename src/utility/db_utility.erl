%%%-------------------------------------------------------------------
%%% @author 李世铭
%%% @copyright (C) April 1st,2016, <COMPANY>
%%% @doc
%%% 数据库读写一些常用方法
%%% @end
%%% Created : 01. 四月 2016 15:04
%%%-------------------------------------------------------------------
-module(db_utility).
-author("Administrator").

%% API
-export([pack_data/1,unpack_data/1]).

%%数据包压包
pack_data(Msg)->
  erlang:list_to_bitstring(io_lib:format("~w", [Msg])).

%%数据包解包
unpack_data(Msg)->
  util:string_to_term(binary_to_list(Msg)).
