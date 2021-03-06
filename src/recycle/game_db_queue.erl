%%%-------------------------------------------------------------------
%%% @author 李世铭
%%% @copyright (C) Mar,31th 2016, <COMPANY>
%%% @doc
%%% 维护一个队列，redis=>mysql同步数据用的队列
%%% 包含以下功能：
%%% 1、入队
%%% 2、出队
%%% 3、获取队列余量
%%% 使用genserver来保证队列单线程，
%%% 使用redis的队列来保证数据不会丢失太多。
%%% @end
%%% Created : 31. 三月 2016 17:18
%%%-------------------------------------------------------------------
-module(game_db_queue).
-author("Administrator").

-behaviour(gen_server).
-include("db_config.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([enqueue/2,dequeue/1,dequeue/2,get_queue_len/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%入队操作
enqueue(DbMsg,List) ->
  gen_server:call(?MODULE, {enqueue,DbMsg,List}).

%%出队操作
dequeue(List)->
  gen_server:call(?MODULE,{dequeue,List}).

%%出队操作N
dequeue(N,List)->
  gen_server:call(?MODULE,{dequeue,N,List}).

%%获取队列长度
get_queue_len(List)->
  gen_server:call(?MODULE,{get_queue_len,List}).

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
init([]) ->
  {ok, #state{}}.

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

handle_call({enqueue,DbMsg,List},_From,State)->
  SzDbMsg = db_utility:pack_data(DbMsg),
  Result = redis:lpush(List,SzDbMsg),
  {reply,Result,State};

handle_call({dequeue,List},_From,State)->
  SzDbMsg = redis:rpop(List),
  Result = db_utility:unpack_data(SzDbMsg),
  {reply,Result,State};

handle_call({dequeue,N,List},_From,State) when is_integer(N)->
  %%首先，根据取出来的第一条信息来决定接下来写入用的连接id
  L = lists:seq(1,N),
  F = fun(_,Res)->
        SzDbMsg = redis:rpop(List),
        {ok,Result} = db_utility:unpack_data(SzDbMsg),
        case Result of
          undefined->
            Res;
          _->
            [Result | Res]
        end
      end,
  %%这里结果是逆序，外层一定要逆序解码
  Result = lists:foldl(F,[],L),
  {reply,{ok,Result},State};

handle_call({get_queue_len,List},_From,State)->
  Result = redis:llen(List),
  {reply,Result,State};

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
