-module(gleam@set).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, size/1, contains/2, delete/2, to_list/1, fold/3, filter/2, drop/2, take/2, intersection/2, difference/2, insert/2, from_list/1, union/2]).
-export_type([set/1]).

-opaque set(EYG) :: {set, gleam@dict:dict(EYG, list(nil))}.

-spec new() -> set(any()).
new() ->
    {set, gleam@dict:new()}.

-spec size(set(any())) -> integer().
size(Set) ->
    maps:size(erlang:element(2, Set)).

-spec contains(set(EYP), EYP) -> boolean().
contains(Set, Member) ->
    _pipe = erlang:element(2, Set),
    _pipe@1 = gleam@dict:get(_pipe, Member),
    gleam@result:is_ok(_pipe@1).

-spec delete(set(EYR), EYR) -> set(EYR).
delete(Set, Member) ->
    {set, gleam@dict:delete(erlang:element(2, Set), Member)}.

-spec to_list(set(EYU)) -> list(EYU).
to_list(Set) ->
    gleam@dict:keys(erlang:element(2, Set)).

-spec fold(set(EZA), EZC, fun((EZC, EZA) -> EZC)) -> EZC.
fold(Set, Initial, Reducer) ->
    gleam@dict:fold(
        erlang:element(2, Set),
        Initial,
        fun(A, K, _) -> Reducer(A, K) end
    ).

-spec filter(set(EZD), fun((EZD) -> boolean())) -> set(EZD).
filter(Set, Predicate) ->
    {set,
        gleam@dict:filter(erlang:element(2, Set), fun(M, _) -> Predicate(M) end)}.

-spec drop(set(EZG), list(EZG)) -> set(EZG).
drop(Set, Disallowed) ->
    gleam@list:fold(Disallowed, Set, fun delete/2).

-spec take(set(EZK), list(EZK)) -> set(EZK).
take(Set, Desired) ->
    {set, gleam@dict:take(erlang:element(2, Set), Desired)}.

-spec order(set(EZO), set(EZO)) -> {set(EZO), set(EZO)}.
order(First, Second) ->
    case maps:size(erlang:element(2, First)) > maps:size(
        erlang:element(2, Second)
    ) of
        true ->
            {First, Second};

        false ->
            {Second, First}
    end.

-spec intersection(set(EZX), set(EZX)) -> set(EZX).
intersection(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    take(Larger, to_list(Smaller)).

-spec difference(set(FAB), set(FAB)) -> set(FAB).
difference(First, Second) ->
    drop(First, to_list(Second)).

-spec insert(set(EYM), EYM) -> set(EYM).
insert(Set, Member) ->
    {set, gleam@dict:insert(erlang:element(2, Set), Member, [])}.

-spec from_list(list(EYX)) -> set(EYX).
from_list(Members) ->
    Dict = gleam@list:fold(
        Members,
        gleam@dict:new(),
        fun(M, K) -> gleam@dict:insert(M, K, []) end
    ),
    {set, Dict}.

-spec union(set(EZT), set(EZT)) -> set(EZT).
union(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    fold(Smaller, Larger, fun insert/2).
