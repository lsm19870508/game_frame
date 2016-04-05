%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 三月 2016 15:12
%%%-------------------------------------------------------------------
-module(game_config).
-author("Administrator").

-behaviour(gen_server).
-include("error_log.hrl").

%% API
-export([start_link/0,start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {path, data = #{}}).
-define(ETS_CONFIG, ets_config).
-record(game_config, {key, data}).

%%%===================================================================
%%% API
%%%===================================================================
-export([lookup_keys/1, lookup_keys/2, reload/0, reload/1]).

lookup_keys(Keys) when is_list(Keys) ->
  case lookup_keys(Keys, null) of
    null ->
      ?LOG_ERROR("config not found Keys:~p~n", [Keys]);
    Value ->
      Value
  end.

lookup_keys(Keys, Default) when is_list(Keys) ->
  [FileName|Keys2] = Keys,
  case ets:lookup(?ETS_CONFIG, FileName) of
    [] ->
      Default;
    [Data] ->
      map_utility:gets(Keys2, Data#game_config.data, Default)
  end.

start_link(Path) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Path], []).

reload() ->
  gen_server:call(?MODULE, {reload}).

reload(Path) ->
  gen_server:call(?MODULE, {reload, Path}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
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
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Path]) ->
  io:format("==============ready to load config file from path~p~n",[Path]),
  % 基本配置表
  ets:new(?ETS_CONFIG, [named_table, set, public, {keypos, #game_config.key},{read_concurrency, true}]),
  % 加载到ets中
  load_config_files(Path),
  {ok, #state{path = Path}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({reload}, From, State) ->
  handle_call({reload, State#state.path}, From, State);
handle_call({reload, Path}, _From, State) ->
  load_config_files(Path),
  {reply, {ok, Path}, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
load_config_files(Path) ->
  ets:delete_all_objects(?ETS_CONFIG),
  FileList = filelib:wildcard("*.json", Path),
  Map = lists:foldl(
    fun(FileName, InMap) ->
      Basename = list_to_binary(filename:basename(FileName, ".json")),
      Fullfile = filename:join(Path, FileName),
      case file:read_file(Fullfile) of
        {ok, Binary} ->
          case jsx:is_json(Binary) of
            true ->
              Data = jsx:decode(Binary,[return_maps]),
              maps:put(Basename, Data, InMap);
            false ->
              ?LOG_ERROR("config content error:" ++ Fullfile),
              throw({"config content error:" ++ Fullfile})
          end;
        _ ->
          ?LOG_ERROR("config load error:" ++ Fullfile),
          throw({"config load error:" ++ Fullfile})
      end
    end, #{}, FileList),
  Keys = maps:keys(Map),
  [ets:insert(?ETS_CONFIG, #game_config{key = K, data = maps:get(K, Map)})||K <- Keys].