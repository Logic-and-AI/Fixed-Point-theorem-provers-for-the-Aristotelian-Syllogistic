
#program base.


%----------------------------------------------------------------
% Database section:
%----------------------------------------------------------------

node(a).
node(b).
node(c).
node(d).
node(e).
node(f).
node(g).
node(h).
node(i).
node(j).
node(k).
node(l).
node(m).
node(n).
node(p).
node(q).
node(r).
node(t).
node(s).
node(u).
node(v).
node(w).



edge(b, a, aa).
edge(c, b, aa).
edge(c, d, ii).
edge(c, f, ii).
edge(d, e, aa).
edge(e, f, aa).
edge(f, g, ee).
edge(h, g, aa).
edge(h, j, aa).
edge(j, k, aa).
edge(k, i, ii).
edge(a, l, aa).
edge(l, m, aa).
edge(m, n, oo).
edge(k, p, aa).
edge(p, q, ee).
edge(q, r, aa).
edge(r, t, aa).
edge(i, h, aa).
edge(u, s, aa).
edge(u, v, aa).
edge(v, w, ee).
edge(m, u, aa).



%currently: 23 nodes, 23 edges (syllogistic sentences)




%-------------syllogistic types-------------------------------
s_state(aa).
s_state(ee).
s_state(ii).
s_state(oo).

%-------------syllogisms--------------------------------------

syllogism(barbara).
syllogism(celarent).
syllogism(darii).
syllogism(ferio).
syllogism(cesare).
syllogism(camestres).
syllogism(festino).
syllogism(baroco).
syllogism(datisi).
syllogism(disamis).
syllogism(ferison).
syllogism(bocardo).
syllogism(felapton).
syllogism(darapti).

syllogism(null).

%-------------------------------------------------------------



% Upper time limits
#const horizon=20.
step(0..horizon).



%--------------------------------------------------------------
%--------------------------------------------------------------
% Reasoner Section:
%--------------------------------------------------------------
%--------------------------------------------------------------



block(e(X,Y, Z)) :- edge(X, Y, Z).



%%%% middle term conditions: %%%%%%

common(e(X, Y, Z), e(P, Q, R)) :-
    node(X), node(Y), node(P), node(Q),
    s_state(Z), s_state(R),
    X = P, Y != Q.

common(e(X, Y, Z), e(P, Q, R)) :-
    node(X), node(Y), node(P), node(Q),
    s_state(Z), s_state(R),
    X = Q, Y != P.

common(e(X, Y, Z), e(P, Q, R)) :-
    node(X), node(Y), node(P), node(Q),
    s_state(Z), s_state(R),
    Y = P, X != Q.

common(e(X, Y, Z), e(P, Q, R)) :-
    node(X), node(Y), node(P), node(Q),
    s_state(Z), s_state(R),
    Y = Q, X != P.


%%%% starting premise: %%%%%%
%start(X, a, aa):-edge(X, a, aa).
%start(b, a, aa).
%start(u, s, aa).

% start from anywhere:
start(X, Y, Z):-edge(X, Y, Z).

%start(h, g, aa).

%%%% target status: %%%%%%%%%
target_position(X, Y, Z) :- node(X), node(Y), s_state(Z).
target_position(p, f, oo).
%target_position(m, i, oo).
%target_position(s, w, oo).
%target_position(g, q, oo).



%%%% reasoner navigation: %%%%%%%%%


fluent(reasoner_permissible_status(X, Y, Z, S)) :- node(X), node(Y), s_state(Z), syllogism(S).

fluent(reasoner_position(X, Y, Z)) :- node(X), node(Y), s_state(Z).

holds(reasoner_position(X, Y, Z), T) :- holds(reasoner_permissible_status(X, Y, Z, S), T), step(T).

holds(reasoner_permissible_status(X, Y, Z, null), 0):- start(X, Y, Z).

action(move_next(X, Y, Z, e(P, Q, R))) :- common(e(X, Y, Z), e(P, Q, R)), not target_position(X, Y, Z), block(e(P, Q, R)).

action(stay(X, Y, Z)) :- fluent(reasoner_position(X, Y, Z)).

holds(reasoner_position(X, Y, Z), T+1) :- occurs(stay(X, Y, Z), T), holds(reasoner_position(X, Y, Z), T), step(T).

:-occurs(stay(X, Y, Z), T), not target_position(X, Y, Z).

{occurs(A, T):action(A)}=1 :- step(T).

:-occurs(move_next(X, Y, Z, e(_, _, _)), T), not holds(reasoner_position(X, Y, Z), T), step(T).

:-occurs(move_next(X, Y, Z, e(P, Q, R)), T), not common(e(X, Y, Z), e(P, Q, R)), step(T).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% Syllogistic Section %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%===============First Figure:====================

%----------------Barbara:------------------------
holds(reasoner_permissible_status(X, Z, aa, barbara), T+1) :- occurs(move_next(X, Y, aa, e(Y, Z, aa)), T), holds(reasoner_position(X, Y, aa), T), step(T).
holds(reasoner_permissible_status(X, Z, aa, barbara), T+1) :- occurs(move_next(Y, Z, aa, e(X, Y, aa)), T), holds(reasoner_position(Y, Z, aa), T), step(T).

%----------------Celarent:------------------------
holds(reasoner_permissible_status(X, Z, ee, celarent), T+1) :- occurs(move_next(X, Y, aa, e(Y, Z, ee)), T), holds(reasoner_position(X, Y, aa), T), step(T).
holds(reasoner_permissible_status(X, Z, ee, celarent), T+1) :- occurs(move_next(Y, Z, ee, e(X, Y, aa)), T), holds(reasoner_position(Y, Z, ee), T), step(T).

%----------------Darii:------------------------
holds(reasoner_permissible_status(X, Z, ii, darii), T+1) :- occurs(move_next(X, Y, ii, e(Y, Z, aa)), T), holds(reasoner_position(X, Y, ii), T), step(T).
holds(reasoner_permissible_status(X, Z, ii, darii), T+1) :- occurs(move_next(Y, Z, aa, e(X, Y, ii)), T), holds(reasoner_position(Y, Z, aa), T), step(T).

%----------------Ferio:------------------------
holds(reasoner_permissible_status(X, Z, oo, ferio), T+1) :- occurs(move_next(X, Y, ii, e(Y, Z, ee)), T), holds(reasoner_position(X, Y, ii), T), step(T).
holds(reasoner_permissible_status(X, Z, oo, ferio), T+1) :- occurs(move_next(Y, Z, aa, e(X, Y, ii)), T), holds(reasoner_position(Y, Z, ee), T), step(T).



%===============Second Figure:====================

%----------------Cesare:------------------------
holds(reasoner_permissible_status(X, Z, ee, cesare), T+1) :- occurs(move_next(Z, Y, ee, e(X, Y, aa)), T), holds(reasoner_position(Z, Y, ee), T), step(T).
holds(reasoner_permissible_status(X, Z, ee, cesare), T+1) :- occurs(move_next(X, Y, aa, e(Z, Y, ee)), T), holds(reasoner_position(X, Y, aa), T), step(T).

%----------------Camestres:------------------------
holds(reasoner_permissible_status(X, Z, ee, camestres), T+1) :- occurs(move_next(Z, Y, aa, e(X, Y, ee)), T), holds(reasoner_position(Z, Y, aa), T), step(T).
holds(reasoner_permissible_status(X, Z, ee, camestres), T+1) :- occurs(move_next(X, Y, ee, e(Z, Y, aa)), T), holds(reasoner_position(X, Y, ee), T), step(T).

%----------------Festino:------------------------
holds(reasoner_permissible_status(X, Z, oo, festino), T+1) :- occurs(move_next(Z, Y, ee, e(X, Y, ii)), T), holds(reasoner_position(Z, Y, ee), T), step(T).
holds(reasoner_permissible_status(X, Z, oo, festino), T+1) :- occurs(move_next(X, Y, ii, e(Z, Y, ee)), T), holds(reasoner_position(X, Y, ii), T), step(T).

%----------------Baroco:------------------------
holds(reasoner_permissible_status(X, Z, oo, baroco), T+1) :- occurs(move_next(Z, Y, aa, e(X, Y, oo)), T), holds(reasoner_position(Z, Y, aa), T), step(T).
holds(reasoner_permissible_status(X, Z, oo, baroco), T+1) :- occurs(move_next(X, Y, oo, e(Z, Y, aa)), T), holds(reasoner_position(X, Y, oo), T), step(T).



%===============Third Figure:====================

%----------------Datisi:------------------------
holds(reasoner_permissible_status(X, Z, ii, datisi), T+1) :- occurs(move_next(Y, Z, aa, e(Y, X, ii)), T), holds(reasoner_position(Y, Z, aa), T), step(T).
holds(reasoner_permissible_status(X, Z, ii, datisi), T+1) :- occurs(move_next(Y, X, ii, e(Y, Z, aa)), T), holds(reasoner_position(Y, X, ii), T), step(T).

%----------------Disamis:------------------------
holds(reasoner_permissible_status(X, Z, ii, disamis), T+1) :- occurs(move_next(Y, Z, ii, e(Y, X, aa)), T), holds(reasoner_position(Y, Z, ii), T), step(T).
holds(reasoner_permissible_status(X, Z, ii, disamis), T+1) :- occurs(move_next(Y, X, aa, e(Y, Z, ii)), T), holds(reasoner_position(Y, X, aa), T), step(T).

%----------------Ferison:------------------------
holds(reasoner_permissible_status(X, Z, oo, ferison), T+1) :- occurs(move_next(Y, Z, ee, e(Y, X, ii)), T), holds(reasoner_position(Y, Z, ee), T), step(T).
holds(reasoner_permissible_status(X, Z, oo, ferison), T+1) :- occurs(move_next(Y, X, ii, e(Y, Z, ee)), T), holds(reasoner_position(Y, X, ii), T), step(T).

%----------------Bocardo:------------------------
holds(reasoner_permissible_status(X, Z, oo, bocardo), T+1) :- occurs(move_next(Y, Z, oo, e(Y, X, aa)), T), holds(reasoner_position(Y, Z, oo), T), step(T).
holds(reasoner_permissible_status(X, Z, oo, bocardo), T+1) :- occurs(move_next(Y, X, aa, e(Y, Z, oo)), T), holds(reasoner_position(Y, X, aa), T), step(T).

%----------------Felapton:------------------------
holds(reasoner_permissible_status(X, Z, oo, felapton), T+1) :- occurs(move_next(Y, Z, ee, e(Y, X, aa)), T), holds(reasoner_position(Y, Z, ee), T), step(T).
holds(reasoner_permissible_status(X, Z, oo, felapton), T+1) :- occurs(move_next(Y, X, aa, e(Y, Z, ee)), T), holds(reasoner_position(Y, X, aa), T), step(T).

%----------------Darapti:------------------------
holds(reasoner_permissible_status(X, Z, ii, darapti), T+1) :- occurs(move_next(Y, Z, aa, e(Y, X, aa)), T), holds(reasoner_position(Y, Z, aa), T), step(T).
holds(reasoner_permissible_status(X, Z, ii, darapti), T+1) :- occurs(move_next(Y, X, aa, e(Y, Z, aa)), T), holds(reasoner_position(Y, X, aa), T), step(T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%% optimization section: %%%%%%

goal(T):- T<=horizon, holds(reasoner_position(X, Y, Z), T), target_position(X, Y, Z).

success:-goal(T), step(T).
:-not success.


redundant(T) :-
    occurs(stay(X, Y, Z), T),
    occurs(stay(X, Y, Z), T-1),
    step(T).


trimmed_occurs(A, T) :- occurs(A, T), not redundant(T).

trimmed_holds(A, T) :- holds(A, T), not redundant(T), A = reasoner_permissible_status(_, _, _, _).


%#minimize { 1, A, T : trimmed_occurs(A, T) }.

#show trimmed_occurs/2.
#show trimmed_holds/2.





