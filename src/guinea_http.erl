% TODO: Format return data in JSON where necessary

-module(guinea_http).
-export([handle/2, handle_event/3,
        auth_fun/3]).

-export([fixed_char/1, gauss_delay/0]).

-export([bproplist2b/1]).

-include("guinea.hrl").
-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

-define(MEAN, 500).
-define(SD, 100).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

%% Home page
handle('GET',[], _Req) ->
    {200, [], <<"ok">>};

%% Origin IP
handle('GET',[<<"ip">>], Req) ->
    {200, [], elli_request:peer(Req)};

%% User agent
handle('GET',[<<"user-agent">>], Req) ->
    {200, [], elli_request:get_header(<<"User-Agent">>, Req)};

%% Headers
handle('GET',[<<"headers">>], Req) ->
    {200,
     [{<<"Access-Control-Allow-Origin">>, <<"*">>},
      {<<"Content-Type">>, <<"text/plain">>}],
     bproplist2b(Req#req.headers)};

%% GET
handle('GET',[<<"get">>], _Req) ->
    {200, [], <<"ok">>};

%% POST
handle('POST',[<<"post">>], Req) ->
    {200, [], elli_request:body(Req)};

%% PUT
handle('PUT',[<<"put">> | _Path], Req) ->
    Path = elli_request:raw_path(Req),
    Body = elli_request:body(Req),
    {200, [], <<Path/binary, "\n", Body/binary>>};

%% DELETE
handle('DELETE',[<<"delete">>], Req) ->
    {200, [], elli_request:body(Req)};

%% Gzip
handle('GET',[<<"gzip">>], Req) ->
    {200,
     [{<<"Access-Control-Allow-Origin">>, <<"*">>},
      {<<"Content-Type">>, <<"text/plain">>},
      {<<"Content-Encoding">>, <<"gzip">>}],
     zlib:gzip(bproplist2b(Req#req.headers))};

%% Status code
handle('GET',[<<"status">>, Code], _Req) ->
    {?b2i(Code), [], <<>>};

%% Response headers
handle('GET',[<<"response-headers">>], Req) ->
    io:format("~p~n", [elli_request:get_args(Req)]),
    {200,
     elli_request:get_args(Req),
     bproplist2b(elli_request:get_args(Req))};

%% Redirect n times
handle('GET',[<<"redirect">>, <<"1">>], Req) ->
    Host = elli_request:get_header(<<"Host">>, Req),
    {302, [{<<"Location">>, <<"http://", Host/binary, "/get">>}], <<>>};
handle('GET',[<<"redirect">>, N], Req) when N > 0 ->
    Host = elli_request:get_header(<<"Host">>, Req),
    NewN = ?i2b(?b2i(N) - 1),
    {302,
     [{<<"Location">>, <<"http://", Host/binary, "/redirect/", NewN/binary>>}],
     <<>>};

%% Redirect n times
handle('GET',[<<"relative-redirect">>, <<"1">>], _Req) ->
    {302,
     [{<<"Location">>, <<"/get">>}],
     <<>>};
handle('GET',[<<"relative-redirect">>, N], _Req) when N > 0 ->
    NewN = ?i2b(?b2i(N) - 1),
    {302, [{<<"Location">>, <<"/relative-redirect/", NewN/binary>>}], <<>>};

%% Get cookie data
handle('GET',[<<"cookies">>], Req) ->
    case elli_cookie:parse(Req) of
        no_cookies ->
            {200, [], <<"no_cookies">>};
        Cookies ->
            {200, [], bproplist2b(Cookies)}
    end;

%% Set cookie data
handle('GET', [<<"cookies">>, <<"set">>], Req) ->
    Headers = elli_cookie:new(elli_request:get_args(Req)),
    {302, [{<<"Location">>, <<"/cookies">>} | Headers], <<>>};

%% Delete given cookies
handle('GET', [<<"cookies">>, <<"delete">>], Req) ->
    [{Keys, true}] = elli_request:get_args(Req),
    Ks = binary:split(Keys, <<",">>, [trim, global]),
    % TODO: move this to elli_cookie
    % TODO: Try using binary comprehensions
    Headers = lists:map(fun (K) ->
                                elli_cookie:delete(K)
                        end, Ks),
    {302, [{<<"Location">>, <<"/cookies">>} | Headers], <<>>};

%% Basic Auth
handle('GET', [<<"basic-auth">>, User, _Password], _Req) ->
    % User is authenticated via auth_user at this point
    {200, [], <<"Authenticated: ", User/binary>>};

%% Hidden basic Auth
handle('GET', [<<"hidden-basic-auth">>, User, _Password], _Req) ->
    % User is authenticated via auth_user at this point
    {200, [], <<"Authentlcated: ", User/binary>>};

%% TODO: digest auth

%% Stream chunked data
handle('GET', [<<"stream">>, Count], Req) ->
    N = min(?b2i(Count), 100),
    ChunkRef = case elli_request:chunk_ref(Req) of
                   {error, not_supported} ->
                       throw({505, [{<<"Access-Control-Allow-Origin">>, <<"*">>}],
                              <<"HTTP Version Not Supported">>});
                   Ref -> Ref
               end,
    Headers = [{<<"Content-Type">>, <<"text/plain">>},
               {<<"Cache-Control">>, <<"no-cache">>},
               {<<"Connection">>, <<"Keep-Alive">>},
               {<<"Access-Control-Allow-Origin">>, <<"*">>}],

    % Start streaming after 500ms
    spawn(fun() ->
                  timer:sleep(500),
                  [elli_request:send_chunk(ChunkRef, <<(?i2b(I))/binary,"\n">>)
                   || I <- lists:seq(1, N)],
                  elli_request:close_chunk(ChunkRef)
          end),

    {chunk, Headers, <<>>};

%% Delay
handle('GET', [<<"delay">>, Duration], _Req) ->
    timer:sleep(?b2i(Duration) * 1000),
    {200, [], <<"ok">>};

%% HTML page
handle('GET', [<<"html">>], _Req) ->
    file:read_file(filename:join(["./priv/docroot", "index.html"]));

%% robots.txt
handle('GET', [<<"robots.txt">>], _Req) ->
    file:read_file(filename:join(["./priv/docroot", "robots.txt"]));

%% Path denied by robots.txt
handle('GET', [<<"deny">>], _Req) ->
    file:read_file(filename:join(["./priv/docroot", "deny.txt"]));

% Additional functions

%% Gauss
%% GET with response times over normal distribution
handle('GET',[<<"gauss">>], _Req) ->
    gauss_delay(),
    {200, [], <<"ok">>};

%% Gauss with user specified delay
%% GET with response times over normal distribution
handle('GET',[<<"gauss">>, Mean, SD], _Req) ->
    gauss_delay(Mean, SD),
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
               {<<"Cache-Control">>, <<"no-cache">>},
               {<<"Connection">>, <<"Keep-Alive">>},
               {<<"Access-Control-Allow-Origin">>, <<"*">>}],

    spawn(fun () -> send_ping(ChunkRef, ?b2i(Count), ?b2i(Interval)) end),
    {chunk, Headers, <<>>};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(request_error, _Data, Args) ->
    error_logger:warning_msg("Request Error: ~p ~n", [Args]);
handle_event(_Event, _Data, _Args) ->
    ok.

auth_fun(Req, User, Password) ->
    case elli_request:path(Req) of
        [<<"basic-auth">>, U, P] ->
            password_check({U, P}, {User, Password});
        [<<"hidden-basic-auth">>, U, P] ->
            hidden_password_check({U, P}, {User, Password});
        _ -> ok
    end.



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
    gauss_delay(?MEAN, ?SD).

gauss_delay(Mean, SD) ->
    timer:sleep(guinea_lib:gauss_int(Mean, SD)).

bproplist2b(List) ->
    bproplist2b(List, <<>>).

bproplist2b([], Acc) ->
    Acc;
bproplist2b([{K, V} | List], Acc) ->
    bproplist2b(List,
                <<Acc/binary, "{", K/binary, ", ", V/binary, "}", "\n">>).

password_check({U, P}, {U, P}) -> ok;
password_check(_, {undefined, undefined}) -> unauthorized;
password_check(_, _) -> forbidden.

hidden_password_check({U, P}, {U, P}) -> ok;
hidden_password_check(_, _) -> hidden.
