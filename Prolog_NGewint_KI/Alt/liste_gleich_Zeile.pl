% Board = 
%	[[0,1,2,5,7],[9,5,6],[],[3,4,5,7]],
% L = 1,
% He = 5,
% r1(Board,L,He).

r1(_,_,0).	
r1(_,0,_).
r1(_,1,1):-Board=[H|T], H=[K|R], write(K).
r1(Board,1,He):-Board=[H|_], (length(H) < He 
					-> write(" "),
					nl,
					He1 = He - 1,
					r1(H,1,He1)
					; H = [_|R],
					He1 = He - 1,
					r2(R,1,He1),
					r1(H,1,He1)).
r1(Board,L,1):- Board = [H|T], H = [K|_], write(K), L1 = L - 1, r1(T,L1,1).
r1(Board,L,He):-r2(Board,L,He).

r2(R,1,1):- R = [H|_], write(H), nl.
r2(R,1,He):- R = [_|T],
		He1 = He - 1,
		r2(T,1,He1).





% r2(Board,He):-(length(Board) < H 
%		-> write(" ")
%		; write(N1,H)),
%	  N2 is H-1, r2(N1,N2).	
