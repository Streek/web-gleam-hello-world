-module(web_gleam_hello_world).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-spec main() -> nil.
main() ->
    App = lustre:element(lustre@element:text(<<"Hello, world!"/utf8>>)),
    _assert_subject = lustre:start(App, <<"#app"/utf8>>, nil),
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"web_gleam_hello_world"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 6})
    end,
    nil.
