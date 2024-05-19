-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, partition/1, replace/2, replace_error/2, values/1, try_recover/2]).

-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-spec map({ok, BJS} | {error, BJT}, fun((BJS) -> BJW)) -> {ok, BJW} |
    {error, BJT}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, BJZ} | {error, BKA}, fun((BKA) -> BKD)) -> {ok, BJZ} |
    {error, BKD}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, BKG} | {error, BKH}} | {error, BKH}) -> {ok, BKG} |
    {error, BKH}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec 'try'({ok, BKO} | {error, BKP}, fun((BKO) -> {ok, BKS} | {error, BKP})) -> {ok,
        BKS} |
    {error, BKP}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec then({ok, BKX} | {error, BKY}, fun((BKX) -> {ok, BLB} | {error, BKY})) -> {ok,
        BLB} |
    {error, BKY}.
then(Result, Fun) ->
    'try'(Result, Fun).

-spec unwrap({ok, BLG} | {error, any()}, BLG) -> BLG.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-spec lazy_unwrap({ok, BLK} | {error, any()}, fun(() -> BLK)) -> BLK.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, BLP}, BLP) -> BLP.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, BLS} | {error, BLS}) -> BLS.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, BLV} | {error, any()}) -> {ok, BLV} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, BMB} | {error, BMC}, {ok, BMB} | {error, BMC}) -> {ok, BMB} |
    {error, BMC}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-spec lazy_or({ok, BMJ} | {error, BMK}, fun(() -> {ok, BMJ} | {error, BMK})) -> {ok,
        BMJ} |
    {error, BMK}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-spec all(list({ok, BMR} | {error, BMS})) -> {ok, list(BMR)} | {error, BMS}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec do_partition(list({ok, BNG} | {error, BNH}), list(BNG), list(BNH)) -> {list(BNG),
    list(BNH)}.
do_partition(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            do_partition(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            do_partition(Rest@1, Oks, [E | Errors])
    end.

-spec partition(list({ok, BMZ} | {error, BNA})) -> {list(BMZ), list(BNA)}.
partition(Results) ->
    do_partition(Results, [], []).

-spec replace({ok, any()} | {error, BNP}, BNS) -> {ok, BNS} | {error, BNP}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, BNV} | {error, any()}, BNZ) -> {ok, BNV} | {error, BNZ}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-spec values(list({ok, BOC} | {error, any()})) -> list(BOC).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-spec try_recover(
    {ok, BOI} | {error, BOJ},
    fun((BOJ) -> {ok, BOI} | {error, BOM})
) -> {ok, BOI} | {error, BOM}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.
