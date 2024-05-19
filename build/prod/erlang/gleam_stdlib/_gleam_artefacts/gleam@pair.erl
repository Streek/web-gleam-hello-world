-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2, new/2]).

-spec first({XP, any()}) -> XP.
first(Pair) ->
    {A, _} = Pair,
    A.

-spec second({any(), XS}) -> XS.
second(Pair) ->
    {_, A} = Pair,
    A.

-spec swap({XT, XU}) -> {XU, XT}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-spec map_first({XV, XW}, fun((XV) -> XX)) -> {XX, XW}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-spec map_second({XY, XZ}, fun((XZ) -> YA)) -> {XY, YA}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.

-spec new(YB, YC) -> {YB, YC}.
new(First, Second) ->
    {First, Second}.
