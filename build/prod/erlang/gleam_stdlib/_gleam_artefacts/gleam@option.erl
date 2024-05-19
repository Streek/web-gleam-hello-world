-module(gleam@option).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([all/1, is_some/1, is_none/1, to_result/2, from_result/1, unwrap/2, lazy_unwrap/2, map/2, flatten/1, then/2, 'or'/2, lazy_or/2, values/1]).
-export_type([option/1]).

-type option(GA) :: {some, GA} | none.

-spec do_all(list(option(GB)), list(GB)) -> option(list(GB)).
do_all(List, Acc) ->
    case List of
        [] ->
            {some, Acc};

        [X | Rest] ->
            Accumulate = fun(Acc@1, Item) -> case {Acc@1, Item} of
                    {{some, Values}, {some, Value}} ->
                        {some, [Value | Values]};

                    {_, _} ->
                        none
                end end,
            Accumulate(do_all(Rest, Acc), X)
    end.

-spec all(list(option(GH))) -> option(list(GH)).
all(List) ->
    do_all(List, []).

-spec is_some(option(any())) -> boolean().
is_some(Option) ->
    Option /= none.

-spec is_none(option(any())) -> boolean().
is_none(Option) ->
    Option =:= none.

-spec to_result(option(GQ), GT) -> {ok, GQ} | {error, GT}.
to_result(Option, E) ->
    case Option of
        {some, A} ->
            {ok, A};

        _ ->
            {error, E}
    end.

-spec from_result({ok, GW} | {error, any()}) -> option(GW).
from_result(Result) ->
    case Result of
        {ok, A} ->
            {some, A};

        _ ->
            none
    end.

-spec unwrap(option(HB), HB) -> HB.
unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default
    end.

-spec lazy_unwrap(option(HD), fun(() -> HD)) -> HD.
lazy_unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default()
    end.

-spec map(option(HF), fun((HF) -> HH)) -> option(HH).
map(Option, Fun) ->
    case Option of
        {some, X} ->
            {some, Fun(X)};

        none ->
            none
    end.

-spec flatten(option(option(HJ))) -> option(HJ).
flatten(Option) ->
    case Option of
        {some, X} ->
            X;

        none ->
            none
    end.

-spec then(option(HN), fun((HN) -> option(HP))) -> option(HP).
then(Option, Fun) ->
    case Option of
        {some, X} ->
            Fun(X);

        none ->
            none
    end.

-spec 'or'(option(HS), option(HS)) -> option(HS).
'or'(First, Second) ->
    case First of
        {some, _} ->
            First;

        none ->
            Second
    end.

-spec lazy_or(option(HW), fun(() -> option(HW))) -> option(HW).
lazy_or(First, Second) ->
    case First of
        {some, _} ->
            First;

        none ->
            Second()
    end.

-spec do_values(list(option(IA)), list(IA)) -> list(IA).
do_values(List, Acc) ->
    case List of
        [] ->
            Acc;

        [X | Xs] ->
            Accumulate = fun(Acc@1, Item) -> case Item of
                    {some, Value} ->
                        [Value | Acc@1];

                    none ->
                        Acc@1
                end end,
            Accumulate(do_values(Xs, Acc), X)
    end.

-spec values(list(option(IF))) -> list(IF).
values(Options) ->
    do_values(Options, []).
