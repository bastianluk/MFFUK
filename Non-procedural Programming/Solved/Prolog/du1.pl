% numeral(X) :- checks whether X is a valid numeral.
numeral(0).
numeral(s(X)) :- numeral(X).

% query examples:
% ?- numeral(s(0)).
% true.
% ?- numeral(s(s(0))).
% true.

% lessThan(X, Y) :- the numeral X is less than Y.
lessThan(s(X), s(Y)) :- lessThan(X, Y), X \= Y.
lessThan(0, X) :- numeral(X), X \= 0.


% query examples:
% ?- lessThan(s(s(s(0))), s(s(0))).
%   false.
% ?- lessThan(0, s(s(0))).
%   true.
% ?- lessThan(s(0), X).
% X = s(s(0)) ;
% X = s(s(s(0))) ;
% X = s(s(s(s(0)))) ;
% X = s(s(s(s(s(0))))) .
% ?- lessThan(X, s(s(s(0)))).
% X = s(s(0)) ;
% X = s(0) ;
% X = 0 ;



% subtract(X, Y, Z) :- X - Y is equal to numeral Z.
subtract(X , 0, X) :- numeral(X).
subtract(s(X), s(Y), Z) :- subtract(X, Y, Z).

% ?- subtract(s(0), 0, s(0)).
% true.
% ?- subtract(s(s(0)), s(s(0)), 0).
% true.
% ?- subtract(s(s(0)), s(s(0)), Z).
% Z = 0 .
% ?- subtract(s(0), s(s(0)), X).
% false.
% ?- subtract(s(s(0)), Y, Z).
% Y = s(s(0)),
% Z = 0 ;
% Y = Z, Z = s(0) ;
% Y = 0,
% Z = s(s(0)).



% divide(X, Y, Result, Reminder) :- divide X by Y, the result numeral is in the argument Result and the reminder in the argument Reminder.
divide(0, _, 0, 0).
divide(X, Y, 0, X) :- lessThan(X, Y).
divide(X, X, s(0), 0).
divide(X, Y, s(R), Rem) :- lessThan(Y, X), subtract(X, Y, Temp), divide(Temp, Y, R, Rem).

% ?- divide(s(s(0)), s(s(s(s(s(s(0)))))), 0, Z).
% Z = s(s(0)) .

% ?- divide(s(s(s(s(0)))), s(s(0)), V, Z).
% V = s(s(0)),
% Z = 0 .

% ?- divide(s(s(s(s(0)))), s(s(s(0))), V, Z).
% V = Z, Z = s(0) .

% ?- divide(s(s(s(s(0)))), s(0), V, Z).
% V = s(s(s(s(0)))),
% Z = 0 .
