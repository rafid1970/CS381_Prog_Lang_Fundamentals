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
brother(X,Y) :- sibling(Y,X) , male(X).
sister(X,Y) :- sibling(X,Y) , female(Y).

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
% Not pretty but it works
related(_,Y) :- siblingInLaw(Y,_).
related(_,Y) :- married(Y,_).
related(_,Y) :- cousin(Y,_).
related(_,Y) :- ancestor(Y,_).


% ======================================================

%%
% Part 2. Language implementation
%%

% ======================================================


% num 	::= 	(any number literal)
% str 	::= 	(any string literal)
% bool 	::= 	t   |   f 	                boolean literals
% prog 	::= 	cmd∗                        sequence of commands
% cmd 	::= 	num   |   str   |   bool 	  push a literal onto the stack
%	     | 	add   |   lte 	              number addition/comparison
%	     | 	if(prog,prog) 	              conditional branching




% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.

bool(true).
bool(false).

lit(X) :- number(X).
lit(X) :- string(X).
lit(X) :- bool(X).
lit(X) :- bool(X).

% pushing literal values on the stack
cmd(Push, OldStack,NewStack) :- lit(Push), NewStack = [Push|OldStack].

% add stuff
cmd(add, [Left,Right|OldStack], NewStack) :- NewStack = [Result|OldStack], Result is Left+Right.

% less than or equal too stuff
cmd(lte, [Left,Right|OldStack],NewStack) :- NewStack = [t|OldStack], Left =< Right.
cmd(lte, [Left,Right|OldStack],NewStack) :- NewStack = [f|OldStack], Left >= Right.

% if stuff
cmd(if(True,_), [t|OldStack], NewStack) :- prog(True, OldStack, NewStack).
cmd(if(_,False), [f|OldStack], NewStack) :- prog(False, OldStack, NewStack).



% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack.
prog([Cmd], OldStack, NewStack) :- cmd(Cmd, OldStack, NewStack). % base case
prog([Cmd|Cmds], OldStack, FinalStack) :- cmd(Cmd, OldStack, NewStack), prog(Cmds, NewStack, FinalStack).
