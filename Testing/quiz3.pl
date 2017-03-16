lives(sloth, forest).
lives(zebra, savanna).
lives(lion, savanna).
lives(deer, forest).
lives(shark, ocean).

eats(zebra, grass).
eats(lion, meat).
eats(sloth, leaves).
eats(deer, grass).
eats(shark, meat).

portal(1,blue,2).      portal(3,red,4).      portal(2,green,1).
portal(2,blue,3).      portal(4,red,1).      portal(3,green,2).
portal(3,blue,4).      portal(4,red,2).      portal(4,green,2).

neighbor(X,Y) :- lives(X,Z), lives(Y,Z), X \= Y.

prey(X) :- neighbor(X,Y), eats(Y,meat).


path(X,C,Y) :- portal(X,C,Y). % base case 
path(X,C,Y) :- portal(X,C,Z), path(Z,C,Y).
