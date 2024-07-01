:- begin_tests(course).

xs --> [].
xs --> [x], xs.

test(pharse_empty) :-
  assertion(phrase(xs, [])).

test(pharse_xx) :-
  assertion(phrase(xs, [x,x])).

test(pharse_xxx) :-
  assertion(phrase(xs, [x,x,x])).

test(pharse_xbx) :-
  assertion(not(phrase(xs, [x,b,x]))).

test(pharse_xXx_X_must_be_x) :-
  phrase(xs, [x,X,x]),
  assertion(X == x).

:- end_tests(course).
:- run_tests.

as --> [].
as --> [a], as.

% ?- phrase(as, As).
% As = [] ;
% As = [a] ;
% As = [a, a] ;
% As = [a, a, a] ;
% As = [a, a, a, a] .

ms --> [].
ms --> [m], ns.

ns --> [].
ns --> [n], ms.

mns --> [].
mns --> [m], mns1.
mns1 --> [n], mns.

% ?- phrase(ms, Ls).
% Ls = [] ;
% Ls = [m] ;
% Ls = [m, n] ;
% Ls = [m, n, m] ;
% Ls = [m, n, m, n] ;
% Ls = [m, n, m, n, m] .

% ?- phrase(mns, Ls).
% Ls = [] ;
% Ls = [m, n] ;
% Ls = [m, n, m, n] ;
% Ls = [m, n, m, n, m, n] .
