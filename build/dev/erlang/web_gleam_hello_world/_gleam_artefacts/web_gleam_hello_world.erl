-module(web_gleam_hello_world).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([my_service/1, main/0]).

-spec my_service(gleam@http@request:request(any())) -> gleam@http@response:response(gleam@bytes_builder:bytes_builder()).
my_service(Request) ->
    Body = gleam_stdlib:wrap_list(<<"Hello, world!"/utf8>>),
    _pipe = gleam@http@response:new(200),
    _pipe@1 = gleam@http@response:prepend_header(
        _pipe,
        <<"made-with"/utf8>>,
        <<"Gleam"/utf8>>
    ),
    gleam@http@response:set_body(_pipe@1, Body).

-spec main() -> nil.
main() ->
    gleam@http@cowboy:start(fun my_service/1, 8888),
    gleam_erlang_ffi:sleep_forever().
