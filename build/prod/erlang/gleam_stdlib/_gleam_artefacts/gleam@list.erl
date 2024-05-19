-module(gleam@list).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([length/1, reverse/1, is_empty/1, contains/2, first/1, rest/1, filter/2, filter_map/2, map/2, map2/3, index_map/2, try_map/2, drop/2, take/2, new/0, append/2, prepend/2, concat/1, flatten/1, flat_map/2, fold/3, group/2, map_fold/3, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, at/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, key_filter/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, try_each/2, partition/2, permutations/1, window/2, window_by_2/1, drop_while/2, take_while/2, chunk/2, sized_chunk/2, reduce/2, scan/3, last/1, combinations/2, combination_pairs/1, transpose/1, interleave/1, shuffle/1]).
-export_type([continue_or_stop/1]).

-type continue_or_stop(YE) :: {continue, YE} | {stop, YE}.

-spec count_length(list(any()), integer()) -> integer().
count_length(List, Count) ->
    case List of
        [_ | List@1] ->
            count_length(List@1, Count + 1);

        _ ->
            Count
    end.

-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-spec do_reverse(list(AQH), list(AQH)) -> list(AQH).
do_reverse(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            do_reverse(Rest, [Item | Accumulator])
    end.

-spec reverse(list(YJ)) -> list(YJ).
reverse(Xs) ->
    lists:reverse(Xs).

-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-spec contains(list(YR), YR) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [First | _] when First =:= Elem ->
            true;

        [_ | Rest] ->
            contains(Rest, Elem)
    end.

-spec first(list(YT)) -> {ok, YT} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _] ->
            {ok, X}
    end.

-spec rest(list(YX)) -> {ok, list(YX)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_ | Xs] ->
            {ok, Xs}
    end.

-spec update_group(fun((AAC) -> AAD)) -> fun((gleam@dict:dict(AAD, list(AAC)), AAC) -> gleam@dict:dict(AAD, list(AAC))).
update_group(F) ->
    fun(Groups, Elem) -> case gleam@dict:get(Groups, F(Elem)) of
            {ok, Existing} ->
                gleam@dict:insert(Groups, F(Elem), [Elem | Existing]);

            {error, _} ->
                gleam@dict:insert(Groups, F(Elem), [Elem])
        end end.

-spec do_filter(list(AAQ), fun((AAQ) -> boolean()), list(AAQ)) -> list(AAQ).
do_filter(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                true ->
                    [X | Acc];

                false ->
                    Acc
            end,
            do_filter(Xs, Fun, New_acc)
    end.

-spec filter(list(AAU), fun((AAU) -> boolean())) -> list(AAU).
filter(List, Predicate) ->
    do_filter(List, Predicate, []).

-spec do_filter_map(
    list(AAX),
    fun((AAX) -> {ok, AAZ} | {error, any()}),
    list(AAZ)
) -> list(AAZ).
do_filter_map(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                {ok, X@1} ->
                    [X@1 | Acc];

                {error, _} ->
                    Acc
            end,
            do_filter_map(Xs, Fun, New_acc)
    end.

-spec filter_map(list(ABF), fun((ABF) -> {ok, ABH} | {error, any()})) -> list(ABH).
filter_map(List, Fun) ->
    do_filter_map(List, Fun, []).

-spec do_map(list(ABM), fun((ABM) -> ABO), list(ABO)) -> list(ABO).
do_map(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            do_map(Xs, Fun, [Fun(X) | Acc])
    end.

-spec map(list(ABR), fun((ABR) -> ABT)) -> list(ABT).
map(List, Fun) ->
    do_map(List, Fun, []).

-spec do_map2(list(ACB), list(ACD), fun((ACB, ACD) -> ACF), list(ACF)) -> list(ACF).
do_map2(List1, List2, Fun, Acc) ->
    case {List1, List2} of
        {[], _} ->
            lists:reverse(Acc);

        {_, []} ->
            lists:reverse(Acc);

        {[A | As_], [B | Bs]} ->
            do_map2(As_, Bs, Fun, [Fun(A, B) | Acc])
    end.

-spec map2(list(ABV), list(ABX), fun((ABV, ABX) -> ABZ)) -> list(ABZ).
map2(List1, List2, Fun) ->
    do_map2(List1, List2, Fun, []).

-spec do_index_map(
    list(ACN),
    fun((ACN, integer()) -> ACP),
    integer(),
    list(ACP)
) -> list(ACP).
do_index_map(List, Fun, Index, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            Acc@1 = [Fun(X, Index) | Acc],
            do_index_map(Xs, Fun, Index + 1, Acc@1)
    end.

-spec index_map(list(ACS), fun((ACS, integer()) -> ACU)) -> list(ACU).
index_map(List, Fun) ->
    do_index_map(List, Fun, 0, []).

-spec do_try_map(list(ACW), fun((ACW) -> {ok, ACY} | {error, ACZ}), list(ACY)) -> {ok,
        list(ACY)} |
    {error, ACZ}.
do_try_map(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, lists:reverse(Acc)};

        [X | Xs] ->
            case Fun(X) of
                {ok, Y} ->
                    do_try_map(Xs, Fun, [Y | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec try_map(list(ADG), fun((ADG) -> {ok, ADI} | {error, ADJ})) -> {ok,
        list(ADI)} |
    {error, ADJ}.
try_map(List, Fun) ->
    do_try_map(List, Fun, []).

-spec drop(list(ADP), integer()) -> list(ADP).
drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_ | Xs] ->
                    drop(Xs, N - 1)
            end
    end.

-spec do_take(list(ADS), integer(), list(ADS)) -> list(ADS).
do_take(List, N, Acc) ->
    case N =< 0 of
        true ->
            lists:reverse(Acc);

        false ->
            case List of
                [] ->
                    lists:reverse(Acc);

                [X | Xs] ->
                    do_take(Xs, N - 1, [X | Acc])
            end
    end.

-spec take(list(ADW), integer()) -> list(ADW).
take(List, N) ->
    do_take(List, N, []).

-spec new() -> list(any()).
new() ->
    [].

-spec do_append(list(AEF), list(AEF)) -> list(AEF).
do_append(First, Second) ->
    case First of
        [] ->
            Second;

        [Item | Rest] ->
            do_append(Rest, [Item | Second])
    end.

-spec append(list(AEB), list(AEB)) -> list(AEB).
append(First, Second) ->
    lists:append(First, Second).

-spec prepend(list(AEJ), AEJ) -> list(AEJ).
prepend(List, Item) ->
    [Item | List].

-spec reverse_and_prepend(list(AEM), list(AEM)) -> list(AEM).
reverse_and_prepend(Prefix, Suffix) ->
    case Prefix of
        [] ->
            Suffix;

        [First | Rest] ->
            reverse_and_prepend(Rest, [First | Suffix])
    end.

-spec do_concat(list(list(AEQ)), list(AEQ)) -> list(AEQ).
do_concat(Lists, Acc) ->
    case Lists of
        [] ->
            lists:reverse(Acc);

        [List | Further_lists] ->
            do_concat(Further_lists, reverse_and_prepend(List, Acc))
    end.

-spec concat(list(list(AEV))) -> list(AEV).
concat(Lists) ->
    do_concat(Lists, []).

-spec flatten(list(list(AEZ))) -> list(AEZ).
flatten(Lists) ->
    do_concat(Lists, []).

-spec flat_map(list(AFD), fun((AFD) -> list(AFF))) -> list(AFF).
flat_map(List, Fun) ->
    _pipe = map(List, Fun),
    concat(_pipe).

-spec fold(list(AFI), AFK, fun((AFK, AFI) -> AFK)) -> AFK.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(Initial, X), Fun)
    end.

-spec group(list(AAK), fun((AAK) -> AAM)) -> gleam@dict:dict(AAM, list(AAK)).
group(List, Key) ->
    fold(List, gleam@dict:new(), update_group(Key)).

-spec map_fold(list(ACI), ACK, fun((ACK, ACI) -> {ACK, ACL})) -> {ACK,
    list(ACL)}.
map_fold(List, Acc, Fun) ->
    _pipe = fold(
        List,
        {Acc, []},
        fun(Acc@1, Item) ->
            {Current_acc, Items} = Acc@1,
            {Next_acc, Next_item} = Fun(Current_acc, Item),
            {Next_acc, [Next_item | Items]}
        end
    ),
    gleam@pair:map_second(_pipe, fun lists:reverse/1).

-spec fold_right(list(AFL), AFN, fun((AFN, AFL) -> AFN)) -> AFN.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), X)
    end.

-spec do_index_fold(
    list(AFO),
    AFQ,
    fun((AFQ, AFO, integer()) -> AFQ),
    integer()
) -> AFQ.
do_index_fold(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            do_index_fold(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-spec index_fold(list(AFR), AFT, fun((AFT, AFR, integer()) -> AFT)) -> AFT.
index_fold(Over, Initial, Fun) ->
    do_index_fold(Over, Initial, Fun, 0).

-spec try_fold(list(AFU), AFW, fun((AFW, AFU) -> {ok, AFW} | {error, AFX})) -> {ok,
        AFW} |
    {error, AFX}.
try_fold(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            {ok, Accumulator};

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {ok, Result} ->
                    try_fold(Rest, Result, Fun);

                {error, _} = Error ->
                    Error
            end
    end.

-spec fold_until(list(AGC), AGE, fun((AGE, AGC) -> continue_or_stop(AGE))) -> AGE.
fold_until(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            Accumulator;

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {continue, Next_accumulator} ->
                    fold_until(Rest, Next_accumulator, Fun);

                {stop, B} ->
                    B
            end
    end.

-spec find(list(AGG), fun((AGG) -> boolean())) -> {ok, AGG} | {error, nil}.
find(Haystack, Is_desired) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Is_desired(X) of
                true ->
                    {ok, X};

                _ ->
                    find(Rest, Is_desired)
            end
    end.

-spec find_map(list(AGK), fun((AGK) -> {ok, AGM} | {error, any()})) -> {ok, AGM} |
    {error, nil}.
find_map(Haystack, Fun) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Fun(X) of
                {ok, X@1} ->
                    {ok, X@1};

                _ ->
                    find_map(Rest, Fun)
            end
    end.

-spec all(list(AGS), fun((AGS) -> boolean())) -> boolean().
all(List, Predicate) ->
    case List of
        [] ->
            true;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    all(Rest, Predicate);

                false ->
                    false
            end
    end.

-spec any(list(AGU), fun((AGU) -> boolean())) -> boolean().
any(List, Predicate) ->
    case List of
        [] ->
            false;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    true;

                false ->
                    any(Rest, Predicate)
            end
    end.

-spec do_zip(list(AGW), list(AGY), list({AGW, AGY})) -> list({AGW, AGY}).
do_zip(Xs, Ys, Acc) ->
    case {Xs, Ys} of
        {[X | Xs@1], [Y | Ys@1]} ->
            do_zip(Xs@1, Ys@1, [{X, Y} | Acc]);

        {_, _} ->
            lists:reverse(Acc)
    end.

-spec zip(list(AHC), list(AHE)) -> list({AHC, AHE}).
zip(List, Other) ->
    do_zip(List, Other, []).

-spec strict_zip(list(AHH), list(AHJ)) -> {ok, list({AHH, AHJ})} | {error, nil}.
strict_zip(List, Other) ->
    case erlang:length(List) =:= erlang:length(Other) of
        true ->
            {ok, zip(List, Other)};

        false ->
            {error, nil}
    end.

-spec do_unzip(list({AXG, AXH}), list(AXG), list(AXH)) -> {list(AXG), list(AXH)}.
do_unzip(Input, Xs, Ys) ->
    case Input of
        [] ->
            {lists:reverse(Xs), lists:reverse(Ys)};

        [{X, Y} | Rest] ->
            do_unzip(Rest, [X | Xs], [Y | Ys])
    end.

-spec unzip(list({AHS, AHT})) -> {list(AHS), list(AHT)}.
unzip(Input) ->
    do_unzip(Input, [], []).

-spec do_intersperse(list(AHX), AHX, list(AHX)) -> list(AHX).
do_intersperse(List, Separator, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Rest] ->
            do_intersperse(Rest, Separator, [X, Separator | Acc])
    end.

-spec intersperse(list(AIB), AIB) -> list(AIB).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_] ->
            List;

        [X | Rest] ->
            do_intersperse(Rest, Elem, [X])
    end.

-spec at(list(AIE), integer()) -> {ok, AIE} | {error, nil}.
at(List, Index) ->
    case Index >= 0 of
        true ->
            _pipe = List,
            _pipe@1 = drop(_pipe, Index),
            first(_pipe@1);

        false ->
            {error, nil}
    end.

-spec unique(list(AII)) -> list(AII).
unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

-spec merge_up(
    integer(),
    integer(),
    list(AIL),
    list(AIL),
    list(AIL),
    fun((AIL, AIL) -> gleam@order:order())
) -> list(AIL).
merge_up(Na, Nb, A, B, Acc, Compare) ->
    case {Na, Nb, A, B} of
        {0, 0, _, _} ->
            Acc;

        {_, 0, [Ax | Ar], _} ->
            merge_up(Na - 1, Nb, Ar, B, [Ax | Acc], Compare);

        {0, _, _, [Bx | Br]} ->
            merge_up(Na, Nb - 1, A, Br, [Bx | Acc], Compare);

        {_, _, [Ax@1 | Ar@1], [Bx@1 | Br@1]} ->
            case Compare(Ax@1, Bx@1) of
                gt ->
                    merge_up(Na, Nb - 1, A, Br@1, [Bx@1 | Acc], Compare);

                _ ->
                    merge_up(Na - 1, Nb, Ar@1, B, [Ax@1 | Acc], Compare)
            end;

        {_, _, _, _} ->
            Acc
    end.

-spec merge_down(
    integer(),
    integer(),
    list(AIQ),
    list(AIQ),
    list(AIQ),
    fun((AIQ, AIQ) -> gleam@order:order())
) -> list(AIQ).
merge_down(Na, Nb, A, B, Acc, Compare) ->
    case {Na, Nb, A, B} of
        {0, 0, _, _} ->
            Acc;

        {_, 0, [Ax | Ar], _} ->
            merge_down(Na - 1, Nb, Ar, B, [Ax | Acc], Compare);

        {0, _, _, [Bx | Br]} ->
            merge_down(Na, Nb - 1, A, Br, [Bx | Acc], Compare);

        {_, _, [Ax@1 | Ar@1], [Bx@1 | Br@1]} ->
            case Compare(Bx@1, Ax@1) of
                lt ->
                    merge_down(Na - 1, Nb, Ar@1, B, [Ax@1 | Acc], Compare);

                _ ->
                    merge_down(Na, Nb - 1, A, Br@1, [Bx@1 | Acc], Compare)
            end;

        {_, _, _, _} ->
            Acc
    end.

-spec merge_sort(
    list(AIV),
    integer(),
    fun((AIV, AIV) -> gleam@order:order()),
    boolean()
) -> list(AIV).
merge_sort(L, Ln, Compare, Down) ->
    N = Ln div 2,
    A = L,
    B = drop(L, N),
    case Ln < 3 of
        true ->
            case Down of
                true ->
                    merge_down(N, Ln - N, A, B, [], Compare);

                false ->
                    merge_up(N, Ln - N, A, B, [], Compare)
            end;

        false ->
            case Down of
                true ->
                    merge_down(
                        N,
                        Ln - N,
                        merge_sort(A, N, Compare, false),
                        merge_sort(B, Ln - N, Compare, false),
                        [],
                        Compare
                    );

                false ->
                    merge_up(
                        N,
                        Ln - N,
                        merge_sort(A, N, Compare, true),
                        merge_sort(B, Ln - N, Compare, true),
                        [],
                        Compare
                    )
            end
    end.

-spec sort(list(AIY), fun((AIY, AIY) -> gleam@order:order())) -> list(AIY).
sort(List, Compare) ->
    merge_sort(List, erlang:length(List), Compare, true).

-spec tail_recursive_range(integer(), integer(), list(integer())) -> list(integer()).
tail_recursive_range(Start, Stop, Acc) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            [Stop | Acc];

        gt ->
            tail_recursive_range(Start, Stop + 1, [Stop | Acc]);

        lt ->
            tail_recursive_range(Start, Stop - 1, [Stop | Acc])
    end.

-spec range(integer(), integer()) -> list(integer()).
range(Start, Stop) ->
    tail_recursive_range(Start, Stop, []).

-spec do_repeat(AJE, integer(), list(AJE)) -> list(AJE).
do_repeat(A, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            do_repeat(A, Times - 1, [A | Acc])
    end.

-spec repeat(AJH, integer()) -> list(AJH).
repeat(A, Times) ->
    do_repeat(A, Times, []).

-spec do_split(list(AJJ), integer(), list(AJJ)) -> {list(AJJ), list(AJJ)}.
do_split(List, N, Taken) ->
    case N =< 0 of
        true ->
            {lists:reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {lists:reverse(Taken), []};

                [X | Xs] ->
                    do_split(Xs, N - 1, [X | Taken])
            end
    end.

-spec split(list(AJO), integer()) -> {list(AJO), list(AJO)}.
split(List, Index) ->
    do_split(List, Index, []).

-spec do_split_while(list(AJS), fun((AJS) -> boolean()), list(AJS)) -> {list(AJS),
    list(AJS)}.
do_split_while(List, F, Acc) ->
    case List of
        [] ->
            {lists:reverse(Acc), []};

        [X | Xs] ->
            case F(X) of
                false ->
                    {lists:reverse(Acc), List};

                _ ->
                    do_split_while(Xs, F, [X | Acc])
            end
    end.

-spec split_while(list(AJX), fun((AJX) -> boolean())) -> {list(AJX), list(AJX)}.
split_while(List, Predicate) ->
    do_split_while(List, Predicate, []).

-spec key_find(list({AKB, AKC}), AKB) -> {ok, AKC} | {error, nil}.
key_find(Keyword_list, Desired_key) ->
    find_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-spec key_filter(list({AKG, AKH}), AKG) -> list(AKH).
key_filter(Keyword_list, Desired_key) ->
    filter_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-spec do_pop(list(BAZ), fun((BAZ) -> boolean()), list(BAZ)) -> {ok,
        {BAZ, list(BAZ)}} |
    {error, nil}.
do_pop(Haystack, Predicate, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    {ok, {X, lists:append(lists:reverse(Checked), Rest)}};

                false ->
                    do_pop(Rest, Predicate, [X | Checked])
            end
    end.

-spec pop(list(AKO), fun((AKO) -> boolean())) -> {ok, {AKO, list(AKO)}} |
    {error, nil}.
pop(Haystack, Is_desired) ->
    do_pop(Haystack, Is_desired, []).

-spec do_pop_map(list(BBN), fun((BBN) -> {ok, BCA} | {error, any()}), list(BBN)) -> {ok,
        {BCA, list(BBN)}} |
    {error, nil}.
do_pop_map(Haystack, Mapper, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Mapper(X) of
                {ok, Y} ->
                    {ok, {Y, lists:append(lists:reverse(Checked), Rest)}};

                {error, _} ->
                    do_pop_map(Rest, Mapper, [X | Checked])
            end
    end.

-spec pop_map(list(AKX), fun((AKX) -> {ok, AKZ} | {error, any()})) -> {ok,
        {AKZ, list(AKX)}} |
    {error, nil}.
pop_map(Haystack, Is_desired) ->
    do_pop_map(Haystack, Is_desired, []).

-spec key_pop(list({ALG, ALH}), ALG) -> {ok, {ALH, list({ALG, ALH})}} |
    {error, nil}.
key_pop(Haystack, Key) ->
    pop_map(
        Haystack,
        fun(Entry) ->
            {K, V} = Entry,
            case K of
                K@1 when K@1 =:= Key ->
                    {ok, V};

                _ ->
                    {error, nil}
            end
        end
    ).

-spec key_set(list({ALM, ALN}), ALM, ALN) -> list({ALM, ALN}).
key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

-spec each(list(ALQ), fun((ALQ) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [X | Xs] ->
            F(X),
            each(Xs, F)
    end.

-spec try_each(list(ALT), fun((ALT) -> {ok, any()} | {error, ALW})) -> {ok, nil} |
    {error, ALW}.
try_each(List, Fun) ->
    case List of
        [] ->
            {ok, nil};

        [X | Xs] ->
            case Fun(X) of
                {ok, _} ->
                    try_each(Xs, Fun);

                {error, E} ->
                    {error, E}
            end
    end.

-spec do_partition(list(BDH), fun((BDH) -> boolean()), list(BDH), list(BDH)) -> {list(BDH),
    list(BDH)}.
do_partition(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {lists:reverse(Trues), lists:reverse(Falses)};

        [X | Xs] ->
            case Categorise(X) of
                true ->
                    do_partition(Xs, Categorise, [X | Trues], Falses);

                false ->
                    do_partition(Xs, Categorise, Trues, [X | Falses])
            end
    end.

-spec partition(list(AMG), fun((AMG) -> boolean())) -> {list(AMG), list(AMG)}.
partition(List, Categorise) ->
    do_partition(List, Categorise, [], []).

-spec permutations(list(AMK)) -> list(list(AMK)).
permutations(L) ->
    case L of
        [] ->
            [[]];

        _ ->
            _pipe = L,
            _pipe@5 = index_map(_pipe, fun(I, I_idx) -> _pipe@1 = L,
                    _pipe@2 = index_fold(
                        _pipe@1,
                        [],
                        fun(Acc, J, J_idx) -> case I_idx =:= J_idx of
                                true ->
                                    Acc;

                                false ->
                                    [J | Acc]
                            end end
                    ),
                    _pipe@3 = lists:reverse(_pipe@2),
                    _pipe@4 = permutations(_pipe@3),
                    map(_pipe@4, fun(Permutation) -> [I | Permutation] end) end),
            concat(_pipe@5)
    end.

-spec do_window(list(list(AMO)), list(AMO), integer()) -> list(list(AMO)).
do_window(Acc, L, N) ->
    Window = take(L, N),
    case erlang:length(Window) =:= N of
        true ->
            do_window([Window | Acc], drop(L, 1), N);

        false ->
            Acc
    end.

-spec window(list(AMU), integer()) -> list(list(AMU)).
window(L, N) ->
    _pipe = do_window([], L, N),
    lists:reverse(_pipe).

-spec window_by_2(list(AMY)) -> list({AMY, AMY}).
window_by_2(L) ->
    zip(L, drop(L, 1)).

-spec drop_while(list(ANB), fun((ANB) -> boolean())) -> list(ANB).
drop_while(List, Predicate) ->
    case List of
        [] ->
            [];

        [X | Xs] ->
            case Predicate(X) of
                true ->
                    drop_while(Xs, Predicate);

                false ->
                    [X | Xs]
            end
    end.

-spec do_take_while(list(ANE), fun((ANE) -> boolean()), list(ANE)) -> list(ANE).
do_take_while(List, Predicate, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    do_take_while(Rest, Predicate, [First | Acc]);

                false ->
                    lists:reverse(Acc)
            end
    end.

-spec take_while(list(ANI), fun((ANI) -> boolean())) -> list(ANI).
take_while(List, Predicate) ->
    do_take_while(List, Predicate, []).

-spec do_chunk(list(ANL), fun((ANL) -> ANN), ANN, list(ANL), list(list(ANL))) -> list(list(ANL)).
do_chunk(List, F, Previous_key, Current_chunk, Acc) ->
    case List of
        [First | Rest] ->
            Key = F(First),
            case Key =:= Previous_key of
                false ->
                    New_acc = [lists:reverse(Current_chunk) | Acc],
                    do_chunk(Rest, F, Key, [First], New_acc);

                _ ->
                    do_chunk(Rest, F, Key, [First | Current_chunk], Acc)
            end;

        _ ->
            lists:reverse([lists:reverse(Current_chunk) | Acc])
    end.

-spec chunk(list(ANT), fun((ANT) -> any())) -> list(list(ANT)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            do_chunk(Rest, F, F(First), [First], [])
    end.

-spec do_sized_chunk(
    list(ANY),
    integer(),
    integer(),
    list(ANY),
    list(list(ANY))
) -> list(list(ANY)).
do_sized_chunk(List, Count, Left, Current_chunk, Acc) ->
    case List of
        [] ->
            case Current_chunk of
                [] ->
                    lists:reverse(Acc);

                Remaining ->
                    lists:reverse([lists:reverse(Remaining) | Acc])
            end;

        [First | Rest] ->
            Chunk = [First | Current_chunk],
            case Left > 1 of
                false ->
                    do_sized_chunk(
                        Rest,
                        Count,
                        Count,
                        [],
                        [lists:reverse(Chunk) | Acc]
                    );

                true ->
                    do_sized_chunk(Rest, Count, Left - 1, Chunk, Acc)
            end
    end.

-spec sized_chunk(list(AOF), integer()) -> list(list(AOF)).
sized_chunk(List, Count) ->
    do_sized_chunk(List, Count, Count, [], []).

-spec reduce(list(AOJ), fun((AOJ, AOJ) -> AOJ)) -> {ok, AOJ} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [First | Rest] ->
            {ok, fold(Rest, First, Fun)}
    end.

-spec do_scan(list(AON), AOP, list(AOP), fun((AOP, AON) -> AOP)) -> list(AOP).
do_scan(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            lists:reverse(Accumulated);

        [X | Xs] ->
            Next = Fun(Accumulator, X),
            do_scan(Xs, Next, [Next | Accumulated], Fun)
    end.

-spec scan(list(AOS), AOU, fun((AOU, AOS) -> AOU)) -> list(AOU).
scan(List, Initial, Fun) ->
    do_scan(List, Initial, [], Fun).

-spec last(list(AOW)) -> {ok, AOW} | {error, nil}.
last(List) ->
    _pipe = List,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-spec combinations(list(APA), integer()) -> list(list(APA)).
combinations(Items, N) ->
    case N of
        0 ->
            [[]];

        _ ->
            case Items of
                [] ->
                    [];

                [X | Xs] ->
                    First_combinations = begin
                        _pipe = map(
                            combinations(Xs, N - 1),
                            fun(Com) -> [X | Com] end
                        ),
                        lists:reverse(_pipe)
                    end,
                    fold(
                        First_combinations,
                        combinations(Xs, N),
                        fun(Acc, C) -> [C | Acc] end
                    )
            end
    end.

-spec do_combination_pairs(list(APE)) -> list(list({APE, APE})).
do_combination_pairs(Items) ->
    case Items of
        [] ->
            [];

        [X | Xs] ->
            First_combinations = map(Xs, fun(Other) -> {X, Other} end),
            [First_combinations | do_combination_pairs(Xs)]
    end.

-spec combination_pairs(list(API)) -> list({API, API}).
combination_pairs(Items) ->
    _pipe = do_combination_pairs(Items),
    concat(_pipe).

-spec transpose(list(list(APP))) -> list(list(APP)).
transpose(List_of_list) ->
    Take_first = fun(List) -> case List of
            [] ->
                [];

            [F] ->
                [F];

            [F@1 | _] ->
                [F@1]
        end end,
    case List_of_list of
        [] ->
            [];

        [[] | Xss] ->
            transpose(Xss);

        Rows ->
            Firsts = begin
                _pipe = Rows,
                _pipe@1 = map(_pipe, Take_first),
                concat(_pipe@1)
            end,
            Rest = transpose(map(Rows, fun(_capture) -> drop(_capture, 1) end)),
            [Firsts | Rest]
    end.

-spec interleave(list(list(APL))) -> list(APL).
interleave(List) ->
    _pipe = transpose(List),
    concat(_pipe).

-spec do_shuffle_pair_unwrap(list({float(), APU}), list(APU)) -> list(APU).
do_shuffle_pair_unwrap(List, Acc) ->
    case List of
        [] ->
            Acc;

        [Elem_pair | Enumerable] ->
            do_shuffle_pair_unwrap(
                Enumerable,
                [erlang:element(2, Elem_pair) | Acc]
            )
    end.

-spec do_shuffle_by_pair_indexes(list({float(), APY})) -> list({float(), APY}).
do_shuffle_by_pair_indexes(List_of_pairs) ->
    sort(
        List_of_pairs,
        fun(A_pair, B_pair) ->
            gleam@float:compare(
                erlang:element(1, A_pair),
                erlang:element(1, B_pair)
            )
        end
    ).

-spec shuffle(list(AQB)) -> list(AQB).
shuffle(List) ->
    _pipe = List,
    _pipe@1 = fold(_pipe, [], fun(Acc, A) -> [{rand:uniform(), A} | Acc] end),
    _pipe@2 = do_shuffle_by_pair_indexes(_pipe@1),
    do_shuffle_pair_unwrap(_pipe@2, []).
