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

in_tree_nodes(nil) --> [].
in_tree_nodes(node(Name, Left, Right)) -->
  in_tree_nodes(Left),
  [Name],
  in_tree_nodes(Right).

test(parse_in_tree_nodes_abcd) :-
  assertion(phrase(
    in_tree_nodes(node(
      a,
      node(b, nil,
        node(c, nil, nil)),
      node(d, nil, nil))),
    [b, c, a, d])).

pre_tree_nodes(nil) --> [].
pre_tree_nodes(node(Name, Left, Right)) -->
  [Name],
  pre_tree_nodes(Left),
  pre_tree_nodes(Right).

test(parse_pre_tree_nodes_abcd) :-
  assertion(phrase(
    pre_tree_nodes(node(
      a,
      node(b, nil,
        node(c, nil, nil)),
      node(d, nil, nil))),
    [a, b, c, d])).

post_tree_nodes(nil) --> [].
post_tree_nodes(node(Name, Left, Right)) -->
  post_tree_nodes(Left),
  post_tree_nodes(Right),
  [Name].

test(parse_post_tree_nodes_abcd) :-
  assertion(phrase(
    post_tree_nodes(node(
      a,
      node(b, nil,
        node(c, nil, nil)),
      node(d, nil, nil))),
    [c, b, d, a])).

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
