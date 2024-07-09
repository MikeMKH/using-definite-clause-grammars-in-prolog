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
  
nt1, [b] --> [a].
nt2      --> [b].

test(pharse_compose_nt1_nt2_given_a) :-
  assertion(phrase((nt1, nt2), [a])).

test(pharse_nt1_given_a_returns_b) :-
  phrase(nt1, [a], R),
  assertion(R == [b]).

look_ahead(T), [T] --> [T].

test(look_ahead_returns_given) :-
  phrase(look_ahead(T), [x], R),
  assertion(T == x),
  assertion(R == [x]).

num_leaves(nil), [N1] --> [N0], { N1 is N0 + 1 }.
num_leaves(node(_,Left,Right)) -->
  num_leaves(Left),
  num_leaves(Right).

test(num_leaves_counts_number_of_leaves) :-
  phrase(num_leaves(
    node(a,
      node(b,nil,nil),
      node(c,nil,
        node(d,nil,nil)))),
    [0], [N]),
  assertion(N =:= 5).

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

tree_nodes(nil) --> [].
tree_nodes(node(Name, Left, Right)) -->
  tree_nodes(Left),
  [Name],
  tree_nodes(Right).

% ?- phrase(tree_nodes(Tree), [a,b,c,d]).
% Tree = node(a, nil, node(b, nil, node(c, nil, node(d, nil, nil)))) ;
% ERROR: Stack limit (1.0Gb) exceeded
% ERROR:   Stack sizes: local: 0.7Gb, global: 0.2Gb, trail: 0Kb
% ERROR:   Stack depth: 6,145,592, last-call: 0%, Choice points: 7
% ERROR:   Possible non-terminating recursion:
% ERROR:     [6,145,592] user:tree_nodes(_61472678, [], _61472682)
% ERROR:     [6,145,591] user:tree_nodes(<compound node/3>, [], _61472704)

tree_nodes(nil, Ls, Ls) --> [].
tree_nodes(node(Name, Left, Right), [_|Ls0], Ls) -->
  tree_nodes(Left, Ls0, Ls1),
  [Name],
  tree_nodes(Right, Ls1, Ls).

% ?- Ns = [a,b,c,d], phrase(tree_nodes(Tree, Ns, _), Ns).
% Ns = [a, b, c, d],
% Tree = node(a, nil, node(b, nil, node(c, nil, node(d, nil, nil)))) ;
% Ns = [a, b, c, d],
% Tree = node(a, nil, node(b, nil, node(d, node(c, nil, nil), nil))) ;
% Ns = [a, b, c, d],
% Tree = node(a, nil, node(c, node(b, nil, nil), node(d, nil, nil))) ;
% Ns = [a, b, c, d],
% Tree = node(a, nil, node(d, node(b, nil, node(c, nil, nil)), nil)) ;
% Ns = [a, b, c, d],
% Tree = node(a, nil, node(d, node(c, node(b, nil, nil), nil), nil)) ;
% Ns = [a, b, c, d],
% Tree = node(b, node(a, nil, nil), node(c, nil, node(d, nil, nil))) .

% ?- Ns = [a,b,c,d], phrase(tree_nodes(Tree, _, Ns), Ns).
% Ns = [a, b, c, d],
% Tree = node(a, nil, node(b, nil, node(c, nil, node(d, nil, nil)))) ;
% ERROR: Stack limit (1.0Gb) exceeded
% ERROR:   Stack sizes: local: 0.5Gb, global: 0.2Gb, trail: 0Kb
% ERROR:   Stack depth: 3,355,311, last-call: 0%, Choice points: 7
% ERROR:   In:
% ERROR:     [3,355,311] user:tree_nodes(_60394386, _60394388, _60394390, [], _60394394)
% ERROR:     [3,355,310] user:tree_nodes(<compound node/3>, [length:1|_60394432], _60394416, [], _60394420)
% ERROR:     [3,355,309] user:tree_nodes(<compound node/3>, [length:2|_60394470], _60394454, [], _60394458)
% ERROR:     [3,355,308] user:tree_nodes(<compound node/3>, [length:3|_60394508], _60394492, [], _60394496)
% ERROR:     [3,355,307] user:tree_nodes(<compound node/3>, [length:4|_60394546], _60394530, [], _60394534)
% ERROR: 
% ERROR: Use the --stack_limit=size[KMG] command line option or
% ERROR: ?- set_prolog_flag(stack_limit, 2_147_483_648). to double the limit.

% [trace]  ?- Ns = [a,b,c], phrase(tree_nodes(Tree, Ns, _), Ns).
%    Call: (11) _7666=[a, b, c] ? creep
%    Exit: (11) [a, b, c]=[a, b, c] ? creep
% ^  Call: (11) phrase(tree_nodes(_7672, [a, b, c], _7676), [a, b, c]) ? creep
%    Call: (14) tree_nodes(_7672, [a, b, c], _7676, [a, b, c], []) ? creep
%    Call: (15) [a, b, c]=[] ? creep
%    Fail: (15) [a, b, c]=[] ? creep
%    Redo: (14) tree_nodes(_7672, [a, b, c], _7676, [a, b, c], []) ? creep
%    Call: (15) tree_nodes(_14168, [b, c], _14172, [a, b, c], _14174) ? creep
%    Call: (16) [a, b, c]=_14174 ? creep
%    Exit: (16) [a, b, c]=[a, b, c] ? creep
%    Exit: (15) tree_nodes(nil, [b, c], [b, c], [a, b, c], [a, b, c]) ? creep
%    Call: (15) [a, b, c]=[_14166|_17444] ? creep
%    Exit: (15) [a, b, c]=[a, b, c] ? creep
%    Call: (15) tree_nodes(_14170, [b, c], _7676, [b, c], []) ? creep
%    Call: (16) [b, c]=[] ? creep
%    Fail: (16) [b, c]=[] ? creep
%    Redo: (15) tree_nodes(_14170, [b, c], _7676, [b, c], []) ? creep
%    Call: (16) tree_nodes(_22334, [c], _22338, [b, c], _22340) ? creep
%    Call: (17) [b, c]=_22340 ? creep
%    Exit: (17) [b, c]=[b, c] ? creep
%    Exit: (16) tree_nodes(nil, [c], [c], [b, c], [b, c]) ? creep
%    Call: (16) [b, c]=[_22332|_25610] ? creep
%    Exit: (16) [b, c]=[b, c] ? creep
%    Call: (16) tree_nodes(_22336, [c], _7676, [c], []) ? creep
%    Call: (17) [c]=[] ? creep
%    Fail: (17) [c]=[] ? creep
%    Redo: (16) tree_nodes(_22336, [c], _7676, [c], []) ? creep
%    Call: (17) tree_nodes(_30500, [], _30504, [c], _30506) ? creep
%    Call: (18) [c]=_30506 ? creep
%    Exit: (18) [c]=[c] ? creep
%    Exit: (17) tree_nodes(nil, [], [], [c], [c]) ? creep
%    Call: (17) [c]=[_166|_1540] ? creep
%    Exit: (17) [c]=[c] ? creep
%    Call: (17) tree_nodes(_170, [], _22, [], []) ? creep
%    Call: (18) []=[] ? creep
%    Exit: (18) []=[] ? creep
%    Exit: (17) tree_nodes(nil, [], [], [], []) ? creep
%    Exit: (16) tree_nodes(node(c, nil, nil), [c], [], [c], []) ? creep
%    Exit: (15) tree_nodes(node(b, nil, node(c, nil, nil)), [b, c], [], [b, c], []) ? creep
%    Exit: (14) tree_nodes(node(a, nil, node(b, nil, node(c, nil, nil))), [a, b, c], [], [a, b, c], []) ? creep
% ^  Exit: (11) phrase(user:tree_nodes(node(a, nil, node(b, nil, node(c, nil, nil))), [a, b, c], []), [a, b, c]) ? creep
% Ns = [a, b, c],
% Tree = node(a, nil, node(b, nil, node(c, nil, nil))) .