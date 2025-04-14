r(0).
r(N):-write(N),write(' Scio nescio'),nl,N1 is N-1,r(N1).

z(Ret) :- random_between(1,10,X), Ret = X.
