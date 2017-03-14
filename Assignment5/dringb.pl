% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X, Y) :- parent(Y, X).
% parent of Y is X


% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- parent(X,_), female(X).
isFather(X) :- parent(X,_), male(X).
% parent of X is someone and is female
% Vice versa

% 3. Define a predicate `grandparent/2`.
grandparent(X,Y) :- parent(X,Z), parent(Z,Y).
% Parent of X is Z and parent of Z is Y

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X,Y) :- parent(Z,Y) , parent(Z,X) , X \== Y.


% 5. Define two predicates `brother/2` and `sister/2`.
brother(X,Y) :- sibling(X,Y) , male(X).
sister(X,Y) :- sibling(X,Y) , female(X).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X,Y) :- married(X,Z), sibling(Z,Y).
siblingInLaw(X,Y) :- sibling(X,Z), married(Z,Y).


% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(X,Y) :- female(X), sibling(X,Z), child(Y,Z).
aunt(X,Y) :- female(X), sibling(Q,Z), child(Y,Z), married(X,Q).
uncle(X,Y) :- male(X), sibling(X,Z), child(Y,Z).
uncle(X,Y) :- male(X), married(X,Q), sibling(Q,Z), child(Y,Z).

% 8. Define the predicate `cousin/2`.
cousin(X,Y) :- parent(Z,X), sibling(Z,Q), parent(Q,Y).


% 9. Define the predicate `ancestor/2`.
ancestor(X,Y) :- child(Y,Z), child(Z,X).
ancestor(X,Y) :- child(Y,X).

% Extra credit: Define the predicate `related/2`.

related_(X,Y) :- child(X,Y).
related_(X,Y) :- parent(X,Y).
related_(X,Y) :- grandparent(X,Y).
related_(X,Y) :- cousin(X,Y).
related_(X,Y) :- brother(X,Y).
related_(X,Y) :- sister(X,Y).
related_(X,Y) :- sibling(X,Y).
related_(X,Y) :- aunt(X,Y).
related_(X,Y) :- uncle(X,Y).
related_(X,Y) :- siblingInLaw(X,Y).
related_(X,Y) :- ancestor(X,Y).
related_(X,Y) :- ancestor(Y,X).
related_(X,Y) :- married(X,Y).
related_(X,Y) :- inLaws(X,Y).
related_(X,Y) :- inLaws(Y,X).


inLaws(X,Y) :- married(X,Y).
inLaws(X,Y) :- married(X,Z), child(Z,Y).
inLaws(X,Y) :- married(X,Z), sibling(Z,Y).
inLaws(X,Y) :- married(X,Z), sibling(Q,Z), child(Y,Q).

print(X,Y) :- male(Y), X \= Y.
print(X,Y) :- female(Y), X \= Y. 

%%
% Part 2. Language implementation
%%

% bool(true)
% bool(false)

% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.
%cmd(X,Y,Z) :-


% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack.
