-module(gleam@http@cowboy).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([start/2]).
-export_type([cowboy_request/0]).

-type cowboy_request() :: any().

-spec set_headers(
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    cowboy_request()
) -> cowboy_request().
set_headers(Headers, Request) ->
    gleam_cowboy_native:set_headers(Headers, Request).

-spec get_method(cowboy_request()) -> gleam@http:method().
get_method(Request) ->
    _pipe = Request,
    _pipe@1 = cowboy_req:method(_pipe),
    _pipe@2 = gleam@http:method_from_dynamic(_pipe@1),
    gleam@result:unwrap(_pipe@2, get).

-spec get_headers(cowboy_request()) -> list({binary(), binary()}).
get_headers(Request) ->
    _pipe = Request,
    _pipe@1 = cowboy_req:headers(_pipe),
    maps:to_list(_pipe@1).

-spec get_scheme(cowboy_request()) -> gleam@http:scheme().
get_scheme(Request) ->
    _pipe = Request,
    _pipe@1 = cowboy_req:scheme(_pipe),
    _pipe@2 = gleam@http:scheme_from_string(_pipe@1),
    gleam@result:unwrap(_pipe@2, http).

-spec get_query(cowboy_request()) -> gleam@option:option(binary()).
get_query(Request) ->
    case cowboy_req:qs(Request) of
        <<""/utf8>> ->
            none;

        Query ->
            {some, Query}
    end.

-spec proplist_get_all(list({IGQ, IGR}), IGQ) -> list(IGR).
proplist_get_all(Input, Key) ->
    gleam@list:filter_map(Input, fun(Item) -> case Item of
                {K, V} when K =:= Key ->
                    {ok, V};

                _ ->
                    {error, nil}
            end end).

-spec cowboy_format_headers(list({binary(), binary()})) -> gleam@dict:dict(binary(), gleam@dynamic:dynamic_()).
cowboy_format_headers(Headers) ->
    Set_cookie_headers = proplist_get_all(Headers, <<"set-cookie"/utf8>>),
    _pipe = Headers,
    _pipe@1 = gleam@list:map(
        _pipe,
        fun(_capture) ->
            gleam@pair:map_second(_capture, fun gleam@dynamic:from/1)
        end
    ),
    _pipe@2 = maps:from_list(_pipe@1),
    gleam@dict:insert(
        _pipe@2,
        <<"set-cookie"/utf8>>,
        gleam@dynamic:from(Set_cookie_headers)
    ).

-spec service_to_handler(
    fun((gleam@http@request:request(bitstring())) -> gleam@http@response:response(gleam@bytes_builder:bytes_builder()))
) -> fun((cowboy_request()) -> cowboy_request()).
service_to_handler(Service) ->
    fun(Request) ->
        {Body, Request@1} = gleam_cowboy_native:read_entire_body(Request),
        Response = Service(
            {request,
                get_method(Request@1),
                get_headers(Request@1),
                Body,
                get_scheme(Request@1),
                cowboy_req:host(Request@1),
                {some, cowboy_req:port(Request@1)},
                cowboy_req:path(Request@1),
                get_query(Request@1)}
        ),
        Status = erlang:element(2, Response),
        Headers = cowboy_format_headers(erlang:element(3, Response)),
        Request@2 = set_headers(Headers, Request@1),
        Body@1 = erlang:element(4, Response),
        cowboy_req:reply(Status, gleam@dict:new(), Body@1, Request@2)
    end.

-spec start(
    fun((gleam@http@request:request(bitstring())) -> gleam@http@response:response(gleam@bytes_builder:bytes_builder())),
    integer()
) -> {ok, gleam@erlang@process:pid_()} | {error, gleam@dynamic:dynamic_()}.
start(Service, Number) ->
    _pipe = Service,
    _pipe@1 = service_to_handler(_pipe),
    gleam_cowboy_native:start_link(_pipe@1, Number).
