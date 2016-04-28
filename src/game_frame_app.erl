-module(game_frame_app).

-behaviour(application).

-include("error_log.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%%启动依赖程序
init_applications()->
    ok = application:start(crypto),
    ok = application:start(cowlib),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(eredis),
    ok = application:start(poolboy),
    %%ok = application:start(eredis_pool),
    ok = application:start(emysql),
    ok = application:start(syntax_tools),
    ok = application:start(compiler),
    ok = application:start(goldrush),
    ok = application:start(lager),
    ok = application:start(jsx),
    ok.

start(_StartType, _StartArgs) ->
    %%启动依赖程序
    init_applications(),

    Dispatch = cowboy_router:compile([
        {'_', [{"/[...]", root_http_handler, []}]}
    ]),
    {ok, Port} = application:get_env(game_frame, cowboy_port),
    {ok, _} = cowboy:start_http(game_frame_http_listener, 100, [{port, Port}],
        [{env, [{dispatch, Dispatch}]},
            {onresponse, fun error_hook/4}
        ]
    ),
    game_frame_sup:start_link().

stop(_State) ->
    ok.

error_hook(404, Headers, <<>>, Req) ->
    Path = cowboy_req:path(Req),
    %%monitor_collector:incr(Path, {error, {<<"404">>, <<"Not found">>}}),
    Body = ["404 Not Found: \"", Path,
        "\" is not the path you are looking for.\n"],
    Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
        {<<"content-length">>, integer_to_list(iolist_size(Body))}),
    ?LOG_INFO("cowbody info req:code:~p, ip:~p, path:~p, qs:~p, headers:~p, body:~p",
        [404, cowboy_req:get(peer, Req), cowboy_req:path(Req), cowboy_req:qs(Req), cowboy_req:headers(Req), cowboy_req:has_body(Req)]),
    cowboy_req:reply(404, Headers2, Body, Req);
error_hook(Code, Headers, <<>>, Req) when is_integer(Code), Code >= 400 ->
    io:format("error_hook is ~p~n",[{Code,Req}]),
    Path = cowboy_req:path(Req),
    Body = ["HTTP Error ", integer_to_list(Code), $\n],

    %%monitor_collector:incr(Path, {error, {conversion_utility:to_binary(Code), <<"ERROR">>}}),

    Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
        {<<"content-length">>, integer_to_list(iolist_size(Body))}),
    ?LOG_INFO("cowbody info req:code:~p, ip:~p, path:~p, qs:~p, headers:~p, body:~p",
        [Code, cowboy_req:get(peer, Req), cowboy_req:path(Req), cowboy_req:qs(Req), cowboy_req:headers(Req), cowboy_req:has_body(Req)]),
    cowboy_req:reply(Code, Headers2, Body, Req);
error_hook(Code, _Headers, _Body, Req) ->
    ?LOG_INFO("cowbody info req:code:~p, ip:~p, path:~p, qs:~p, headers:~p, body:~p",
        [Code, cowboy_req:get(peer, Req), cowboy_req:path(Req), cowboy_req:qs(Req), cowboy_req:headers(Req), cowboy_req:has_body(Req)]),
    Req.
