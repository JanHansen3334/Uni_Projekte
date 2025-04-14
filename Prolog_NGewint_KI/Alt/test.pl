Board = [[0,1,2,5,7],[9,5,6],[],[3,4,5,7]],
L = 4,
He = 5,
create_board(Board,L,He):-
	
r1(Board,0,He).
r1(Board,1,1):-Board=[H|T], H=[K|R], write(K),nl.
r1(Board,1,He):-Board=[H|T], (length(H) < H 
					-> write(" "),
					nl,
					He = He - 1,
					r1(H,1,He)
					; H = [K|R],
					He = He - 1,
					r1(R,1,He),
					r1(H,1,He)).
r1(Board,L,He):-r2(Board,L,He).

r2(Board,He):-(length(Board) < H 
		-> write(" ")
		; write(N1,H)),
	  N2 is H-1, r2(N1,N2).		
