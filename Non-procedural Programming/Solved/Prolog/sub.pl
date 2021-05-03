% Pøíklady:
% ?- substitute([1,2,3,4],3,5,X).
% X = [1, 2, 5, 4].

% ?- substitute([1,2,3,4],3,B,[1,2,5,4]).
% B = 5 ;
% false.

% ?- substitute(X,3,5,[1,2,5,4]).
% X = [1, 2, 3, 4] ;
% X = [1, 2, 5, 4] ;

% ?- substitute([1,2,3,4],3,4,[1,2,5,4]).
% false.

% Za bonusové body zkuste predikát implementovat tak, aby fungovaly i tyto dotazy (budou se vám hodit znalosti z dalšího cvièení)

% ?- substitute([1,2,3,4],A,B,[1,2,5,4]).
% A = 3,
% B = 5 ;
% false.

% ?- substitute([1,2,3,4],A,B,Y).
% A = 1,
% Y = [B, 2, 3, 4] ;
% A = 2,
% Y = [1, B, 3, 4] ;
% A = 3,
% Y = [1, 2, B, 4] ;
% A = 4,
% Y = [1, 2, 3, B] ;
% false.
