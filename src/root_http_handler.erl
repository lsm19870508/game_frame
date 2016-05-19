%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 三月 2016 14:15
%%%-------------------------------------------------------------------
-module(root_http_handler).
-author("Administrator").
-include("error_log.hrl").
-include("field.hrl").
-include("status_code.hrl").

%% API
-export([init/2]).

init(Req,Opts) ->
  Method = cowboy_req:method(Req),
  HasBody = cowboy_req:has_body(Req),
  Req2 = process_request(Method,HasBody,Req),
  {ok,Req2,Opts}.

process_request(<<"POST">>, false, Req) ->
  respond_invalid_request(Req);
process_request(<<"POST">>, true, Req) ->
  process_post(Req);
process_request(<<"GET">>, _, Req) ->
  process_get(Req);
process_request(_, _, Req) ->
  respond_invalid_request(Req).

%% HTTP GET请求
process_get(Req) ->
  respond_invalid_request(Req).

%% HTTP Body Fetching, right now the limit is 1MB(defined in cowboy)
process_post(Req) ->
  Test = cowboy_req:body(Req),
  case cowboy_req:body(Req) of
    {ok, Body, Req2} ->
      % 解密
      case catch lib_crypto:decrypt(Req, Body) of
        {error, _Reason} ->
          respond_invalid_request(Req);
        {ok, Body2} ->
          process_body(Body2, Req2)
      end;
    {more, _, _} ->
      %% By default, cowboy will read 1MB data at one time,
      %% if our request data exist this range, probably we
      %% have a problem with our API
      respond_invalid_request(Req)
  end.

%% JSON parsing
process_body(Body, Req) ->
  case catch jsx:decode(Body, [return_maps]) of
    {Error, Reason} ->
      ?LOG_ERROR("request:~p,error:~p,reason:~p", [Body, Error, Reason]),
      respond_invalid_request(Req);
    Data ->
      process_data(Data, Req)
  end.

%% This function contains the actual logic!
process_data(Data, Req) when is_map(Data) ->
  {{IP1, IP2, IP3, IP4}, _} = cowboy_req:get(peer, Req),
  IP = lists:concat([IP1, ".",IP2, ".",IP3, ".",IP4]),
  RetJson = request_dispatcher:dispatch(Data#{?CLIENTIP => IP}),
  respond_json(200, RetJson, Req);
process_data(_Data, Req) ->
  respond_invalid_request(Req).


respond_json(Code, Json, Req) ->
  Body = jsx:encode(Json),
  % 加密
  {ok, Body2, Head} = lib_crypto:encrypt(Body),
  cowboy_req:reply(Code,[Head, {<<"Content-Type">>, <<"application/json; charset=utf-8">>},{<<"Access-Control-Allow-Origin">>,<<"http://121.43.97.11">>}], Body2, Req).

respond_invalid_request(Req) ->
  Status = ?STATUS_INVALID_REQUEST,
  respond_json(200, #{<<"code">> => 0, <<"status">> => Status}, Req).