-module(gleam@dict).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([size/1, to_list/1, new/0, get/2, has_key/2, insert/3, from_list/1, keys/1, values/1, take/2, merge/2, delete/2, drop/2, update/3, fold/3, map_values/2, filter/2]).
-export_type([dict/2]).

-type dict(KR, KS) :: any() | {gleam_phantom, KR, KS}.

-spec size(dict(any(), any())) -> integer().
size(Dict) ->
    maps:size(Dict).

-spec to_list(dict(KX, KY)) -> list({KX, KY}).
to_list(Dict) ->
    maps:to_list(Dict).

-spec new() -> dict(any(), any()).
new() ->
    maps:new().

-spec get(dict(ME, MF), ME) -> {ok, MF} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-spec has_key(dict(LO, any()), LO) -> boolean().
has_key(Dict, Key) ->
    maps:is_key(Key, Dict).

-spec insert(dict(MQ, MR), MQ, MR) -> dict(MQ, MR).
insert(Dict, Key, Value) ->
    maps:put(Key, Value, Dict).

-spec fold_list_of_pair(list({LH, LI}), dict(LH, LI)) -> dict(LH, LI).
fold_list_of_pair(List, Initial) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold_list_of_pair(
                Rest,
                insert(Initial, erlang:element(1, X), erlang:element(2, X))
            )
    end.

-spec from_list(list({LC, LD})) -> dict(LC, LD).
from_list(List) ->
    maps:from_list(List).

-spec reverse_and_concat(list(TR), list(TR)) -> list(TR).
reverse_and_concat(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            reverse_and_concat(Rest, [Item | Accumulator])
    end.

-spec do_keys_acc(list({OD, any()}), list(OD)) -> list(OD).
do_keys_acc(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [X | Xs] ->
            do_keys_acc(Xs, [erlang:element(1, X) | Acc])
    end.

-spec keys(dict(NQ, any())) -> list(NQ).
keys(Dict) ->
    maps:keys(Dict).

-spec do_values_acc(list({any(), OT}), list(OT)) -> list(OT).
do_values_acc(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [X | Xs] ->
            do_values_acc(Xs, [erlang:element(2, X) | Acc])
    end.

-spec values(dict(any(), OJ)) -> list(OJ).
values(Dict) ->
    maps:values(Dict).

-spec insert_taken(dict(PX, PY), list(PX), dict(PX, PY)) -> dict(PX, PY).
insert_taken(Dict, Desired_keys, Acc) ->
    Insert = fun(Taken, Key) -> case get(Dict, Key) of
            {ok, Value} ->
                insert(Taken, Key, Value);

            _ ->
                Taken
        end end,
    case Desired_keys of
        [] ->
            Acc;

        [X | Xs] ->
            insert_taken(Dict, Xs, Insert(Acc, X))
    end.

-spec take(dict(PJ, PK), list(PJ)) -> dict(PJ, PK).
take(Dict, Desired_keys) ->
    maps:with(Desired_keys, Dict).

-spec insert_pair(dict(QW, QX), {QW, QX}) -> dict(QW, QX).
insert_pair(Dict, Pair) ->
    insert(Dict, erlang:element(1, Pair), erlang:element(2, Pair)).

-spec fold_inserts(list({RC, RD}), dict(RC, RD)) -> dict(RC, RD).
fold_inserts(New_entries, Dict) ->
    case New_entries of
        [] ->
            Dict;

        [X | Xs] ->
            fold_inserts(Xs, insert_pair(Dict, X))
    end.

-spec merge(dict(QG, QH), dict(QG, QH)) -> dict(QG, QH).
merge(Dict, New_entries) ->
    maps:merge(Dict, New_entries).

-spec delete(dict(RJ, RK), RJ) -> dict(RJ, RK).
delete(Dict, Key) ->
    maps:remove(Key, Dict).

-spec drop(dict(RV, RW), list(RV)) -> dict(RV, RW).
drop(Dict, Disallowed_keys) ->
    case Disallowed_keys of
        [] ->
            Dict;

        [X | Xs] ->
            drop(delete(Dict, X), Xs)
    end.

-spec update(dict(SC, SD), SC, fun((gleam@option:option(SD)) -> SD)) -> dict(SC, SD).
update(Dict, Key, Fun) ->
    _pipe = Dict,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Dict, Key, _pipe@3).

-spec do_fold(list({SJ, SK}), SM, fun((SM, SJ, SK) -> SM)) -> SM.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Rest] ->
            do_fold(Rest, Fun(Initial, K, V), Fun)
    end.

-spec fold(dict(SN, SO), SR, fun((SR, SN, SO) -> SR)) -> SR.
fold(Dict, Initial, Fun) ->
    _pipe = Dict,
    _pipe@1 = maps:to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).

-spec map_values(dict(NC, ND), fun((NC, ND) -> NG)) -> dict(NC, NG).
map_values(Dict, Fun) ->
    maps:map(Fun, Dict).

-spec filter(dict(OX, OY), fun((OX, OY) -> boolean())) -> dict(OX, OY).
filter(Dict, Predicate) ->
    maps:filter(Predicate, Dict).
