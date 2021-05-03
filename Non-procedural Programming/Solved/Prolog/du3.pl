getValid(Exp1, Exp2, [O | Ops], Valid) :-
  NewValid =.. [O, Exp1, Exp2], getValid(Exp1, Exp2, Ops, InnerValid), append([NewValid], InnerValid, Valid).
getValid(_, _, [], []).

gen([SingleNumber], _, SingleNumber).
gen(NumberList, OpsList, Expression):-
    append([X|Xs], [Y|Ys], NumberList),
    gen([X|Xs], OpsList, Exp1),
    gen([Y|Ys], OpsList, Exp2),
    (
	getValid(Exp1, Exp2, OpsList, Valid),
        member(Expression,Valid)
    ).

safe_is(Expression, R) :- 
    (
        member(Expression,[Exp1+Exp2,Exp1-Exp2,Exp1*Exp2]);
        (
            member(Expression,[_/Exp2]),
            Val2 is Exp2,
            Val2 =\= 0
        )
    ),
    R is Expression.

gen_result([SingleNumber], SingleNumber, SingleNumber).
gen_result(NumberList, Target, Expression):-
    gen(NumberList, [+, -, *, /], Expression),
    safe_is(Expression, Target).
