%%%-------------------------------------------------------------------
%%% @author wlq
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 五月 2015 上午10:42
%%%-------------------------------------------------------------------
-module(lager_controller).
-behaviour(gen_server).

%% API
-export([start_link/0, start/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([reload_lager_handler/2, set_log_level/1, get_log_level/0, get_console_level/0, set_console_level/1]).
-define(SERVER, ?MODULE).
-define(LEVEL_ERROR, error).
-define(LEVEL_INFO, info).
-include("error_log.hrl").
-record(state, {log_level = error, log_file}).

%%%===================================================================
%%% API
%%%===================================================================
%% 设置file log的等级
set_log_level(Level) ->
  case lists:member(Level, [debug, info, notice, warning, error, critical, alert, emergency]) of
    true ->
      Pid = whereis(?SERVER),
      Pid ! {set_log_level, Level};
    false ->
      ok
  end.

%% 设置控制台 log等级
set_console_level(Level) ->
  case lists:member(Level, [debug, info, notice, warning, error, critical, alert, emergency]) of
    true ->
      Pid = whereis(?SERVER),
      Pid ! {set_console_level, Level};
    false ->
      ok
  end.

%% 查看file log等级
get_log_level() ->
  Pid = whereis(?SERVER),
  Pid ! {get_log_level}.

%% 查看控制台 log等级
get_console_level() ->
  Pid = whereis(?SERVER),
  Pid ! {get_console_level}.


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start() ->
  supervisor:start_child(baba3_server_sup,
               {?MODULE, {?MODULE, start_link, []},
               permanent, 5000, worker, [?MODULE]}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  FileName = make_file_name(),
  {ok, Level} = application:get_env(lager, log_level),
  reload_lager_handler(FileName, Level),
  {_, {_, M, S}} = erlang:localtime(),
  NextHourSeconds = 3600 - (M * 60 + S),
  erlang:send_after(NextHourSeconds * 1000, self(), {reload_lager_handler}),
  {ok, #state{log_file = {lager_file_backend, FileName}, log_level = Level}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
  try
    {ok, NewState} = do_handle_info(Info, State),
    {noreply, NewState}
  catch
    Error:Reason ->
      ?LOG_ERROR("~p,~p", [Error, Reason]),
      {noreply, State}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 查看file log等级
do_handle_info({get_log_level}, State) ->
  Level = lager:get_loglevel(State#state.log_file),
  io:format("log level is:~p~n", [Level]),
  {ok, State};

%% 查看控制台 log等级
do_handle_info({get_console_level}, State) ->
  Level = lager:get_loglevel(lager_console_backend),
  io:format("console level is:~p~n", [Level]),
  {ok, State};

%% 设置文件 log的level
do_handle_info({set_log_level, Level}, State) ->
  lager:set_loglevel(State#state.log_file, Level),
  {ok, State#state{log_level = Level}};

%% 设置控制台 log的level
do_handle_info({set_console_level, Level}, State) ->
  lager:set_loglevel(lager_console_backend, Level),
  {ok, State#state{log_level = Level}};

%% 重新加载handler
do_handle_info({reload_lager_handler}, State) ->
  FileName = make_file_name(),
  reload_lager_handler(FileName, State#state.log_level),
  erlang:send_after(3600 * 1000, self(), {reload_lager_handler}),
  {ok, State#state{log_file = {lager_file_backend, FileName}}};

do_handle_info(_Info, State) ->
  {ok, State}.

%% 重新加载lager日志handler
reload_lager_handler(File, Level) ->
  % 获取当前的所有handler
  Handlers = gen_event:which_handlers(lager_event),
  % 过滤evel级别的hander
  CurLevelHandlers = [H||H <- Handlers, lager:get_loglevel(H) == Level],
  % 添加新的handlers
  {ok, Size} = application:get_env(lager, log_size),
  [supervisor:start_child(lager_handler_watcher_sup, [lager_event, Module, Config]) ||
    {Module, Config} <- [{{lager_file_backend,File}, [{file,File}, {level,Level}, {date, []}, {size, Size}]}]],
  % 删除就得handlers
  [gen_event:delete_handler(lager_event, CurHandler, [])||CurHandler <- CurLevelHandlers],
  ok.

make_file_name() ->
  {{Y, M, D}, {H, _Min, _Sec}} = erlang:localtime(),
  MStr = format_file(M),
  DStr = format_file(D),
  HStr = format_file(H),
  lists:concat(["bbqn3_", Y, MStr, DStr, "_", HStr, "00.log"]).

format_file(D) ->
  case D < 10 of
    true ->
      "0" ++ conversion_utility:to_list(D);
    false ->
      conversion_utility:to_list(D)
  end.
