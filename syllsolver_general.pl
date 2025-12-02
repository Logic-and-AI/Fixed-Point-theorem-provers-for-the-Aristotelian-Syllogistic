
% ---- basic types ----
type(X) :- X = atype.   % A (universal affirmative)
type(X) :- X = etype.   % E (universal negative)
type(X) :- X = itype.   % I (particular affirmative)
type(X) :- X = otype.   % O (particular negative)

term(X) :- atom(X), \+ type(X).


premises([[atype, b, a], [atype, c, b], [itype, c, d], [itype, c, f], [atype, d, e], [atype, e, f], [etype, f, g],
         [atype, h, g], [atype, h, j], [atype, j, k], [atype, k, i], [atype, a, l], [atype, l, m], [otype, m, n],
        [atype, k, p], [etype, p, q], [atype, q, r], [atype, r, t], [atype, i, h], [atype, u, s], [atype, u, v],
         [etype, v, w], [atype, m, u]]).

% Well-formed formula: [Type, Subject, Predicate]
swff([Q, S, P]) :- type(Q), term(S), term(P).


% -------- First figure --------

% Barbara
syllogistic_conclusion(A, B, [atype, Y, R]) :-
    swff(A), swff(B),
    A = [atype, Y, Z], B = [atype, Q, R], Z = Q, dif(Y, R).

% Celarent
syllogistic_conclusion(A, B, [etype, Y, R]) :-
    swff(A), swff(B),
    A = [atype, Y, Z], B = [etype, Q, R], Z = Q, dif(Y, R).




% -------- Second figure --------

% Cesare
syllogistic_conclusion(A, B, [etype, Q, Y]) :-
    swff(A), swff(B),
    A = [etype, Y, Z], B = [atype, Q, R], Z = R, dif(Y, Q).

% Camestres
syllogistic_conclusion(A, B, [etype, Q, Y]) :-
    swff(A), swff(B),
    A = [atype, Y, Z], B = [etype, Q, R], Z = R, dif(Y, Q).




% -------- Third figure --------

% Darapti
syllogistic_conclusion(A, B, [itype, R, Z]) :-
    swff(A), swff(B),
    A = [atype, Y, Z], B = [atype, Y, R], dif(Z, R).

% Felapton
syllogistic_conclusion(A, B, [otype, R, Z]) :-
    swff(A), swff(B),
    A = [etype, Y, Z], B = [atype, Y, R], dif(Z, R).





/* ============================
   Fixed-point saturation
   ============================ */

subtract_list([], _, []).
subtract_list([H|T], L, R) :-
    ( memberchk(H, L) ->
        subtract_list(T, L, R)
    ;   R = [H|R1],
        subtract_list(T, L, R1)
    ).

step_once(Current, NewOnly, Next) :-
    findall(C,
        ( member(A, Current),
          member(B, Current),
          syllogistic_conclusion(A, B, C)
        ),
        Raw),
    sort(Raw, Cand),                 % de-dup generated
    subtract_list(Cand, Current, NewOnly),
    append(Current, NewOnly, Tmp),
    sort(Tmp, Next).

derive_closure(Premises, Closure) :-
    sort(Premises, Start),
    closure_loop(Start, Closure).

closure_loop(Current, Closure) :-
    step_once(Current, NewOnly, Next),
    ( NewOnly == [] ->
        Closure = Current
    ;   closure_loop(Next, Closure)
    ).

derive_from_premises(Closure) :-
    premises(Ps),
    derive_closure(Ps, Closure).







