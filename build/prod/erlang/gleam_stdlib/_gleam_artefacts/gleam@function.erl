-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-spec compose(fun((EPS) -> EPT), fun((EPT) -> EPU)) -> fun((EPS) -> EPU).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((EPV, EPW) -> EPX)) -> fun((EPV) -> fun((EPW) -> EPX)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((EPZ, EQA, EQB) -> EQC)) -> fun((EPZ) -> fun((EQA) -> fun((EQB) -> EQC))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((EQE, EQF, EQG, EQH) -> EQI)) -> fun((EQE) -> fun((EQF) -> fun((EQG) -> fun((EQH) -> EQI)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((EQK, EQL, EQM, EQN, EQO) -> EQP)) -> fun((EQK) -> fun((EQL) -> fun((EQM) -> fun((EQN) -> fun((EQO) -> EQP))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((EQR, EQS, EQT, EQU, EQV, EQW) -> EQX)) -> fun((EQR) -> fun((EQS) -> fun((EQT) -> fun((EQU) -> fun((EQV) -> fun((EQW) -> EQX)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((EQZ, ERA) -> ERB)) -> fun((ERA, EQZ) -> ERB).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(ERC) -> ERC.
identity(X) ->
    X.

-spec constant(ERD) -> fun((any()) -> ERD).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(ERF, fun((ERF) -> any())) -> ERF.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-spec apply1(fun((ERH) -> ERI), ERH) -> ERI.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-spec apply2(fun((ERJ, ERK) -> ERL), ERJ, ERK) -> ERL.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-spec apply3(fun((ERM, ERN, ERO) -> ERP), ERM, ERN, ERO) -> ERP.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
