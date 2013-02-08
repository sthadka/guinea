-module(guinea_http).
-export([handle/2, handle_event/3]).

-export([fixed_char/1, gauss_delay/0]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

-define(MEAN, 500).
-define(SD, 100).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

%% Simple response
%% Test GET
handle('GET',[<<"simple">>], _Req) ->
    {200, [], <<"ok">>};

%% Gauss
%% GET with response times over normal distribution
handle('GET',[<<"gauss">>], _Req) ->
    gauss_delay(),
    {200, [], <<"ok">>};

%% Payload
%% Download a large payload
handle('GET',[<<"payload">>, Size], _Req) ->
    {200, [], get_binary(Size)};

%% Long running connection
%% Connection that is open for a long time (via SSE)
%% Pings every interval for count times
handle('GET', [<<"chunk">>, Count, Interval], Req) ->
    ChunkRef = case elli_request:chunk_ref(Req) of
                   {error, not_supported} ->
                       throw({505, [{<<"Access-Control-Allow-Origin">>, <<"*">>}],
                              <<"HTTP Version Not Supported">>});
                   Ref -> Ref
               end,
    Headers = [{<<"Content-Type">>, <<"text/event-stream">>},
               {<<"Cache-Control">>,
                <<"no-cache">>},
               {<<"Connection">>,
                <<"Keep-Alive">>},
               {<<"Access-Control-Allow-Origin">>, <<"*">>}],

    spawn(fun () -> send_ping(ChunkRef, b2i(Count), b2i(Interval)) end),
    {chunk, Headers, <<>>};

%% POST data
%% Receive (large) data from client
handle('POST', [<<"post">>], Req) ->
    {200, [], i2b(byte_size(elli_request:body(Req)))};

%% API GET (tracking)
%% Simple get with gaussian delay
handle('GET',[<<"api_get">>], _Req) ->
    gauss_delay(),
    {200, [], <<"ok">>};

%% Similate an online API provider
%% For pushing large data from client to server
handle('POST', [<<"api_push">>], Req) ->
    gauss_delay(),
    {200, [], i2b(byte_size(elli_request:body(Req)))};

%% Similate an online API provider (database)
%% For pull large data from server to client
handle('POST', [<<"api_pull">>, Size], _Req) ->
    gauss_delay(),
    {200, [], get_binary(i2b(Size))};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(request_error, _Data, Args) ->
    error_logger:warning_msg("Request Error: ~p ~n", [Args]);
handle_event(_Event, _Data, _Args) ->
    ok.


% -------------------------------------------------------------------
% Helper functions
% -------------------------------------------------------------------

% Generate binary blob of given size
fixed_char(Size) ->
    IntSize = list_to_integer(binary_to_list(Size)),
    list_to_binary(lists:map(fun (_) ->
                                     random:uniform(95)+31 end,
                             lists:seq(1, IntSize))).

% Send regular ping to client (SSE)
send_ping(Ref, 0, _Interval) ->
    elli_request:close_chunk(Ref);
send_ping(Ref, Count, Interval) ->
    timer:sleep(Interval),
    elli_request:send_chunk(Ref, message_to_event(<<"PING">>)),
    send_ping(Ref, Count - 1, Interval).

message_to_event(Message) ->
    <<"data: ", Message/binary, "\nretry: 300000\n\n">>.

b2i(Binary) ->
    list_to_integer(binary_to_list(Binary)).

i2b(Integer) ->
    list_to_binary(integer_to_list(Integer)).

% Cache binary blob locally
get_binary(Size) ->
    case guinea_kv:get(Size) of
        undefined ->
            Data = fixed_char(Size),
            guinea_kv:set(Size, Data),
            Data;
        Data -> Data
    end.

gauss_delay() ->
    timer:sleep(guinea_lib:gauss_int(?MEAN, ?SD)).

