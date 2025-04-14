% 1.

% Wir defnieren uns die Repreäsentation vom Spielfeld (Listen von Listen). wir prüfen, ob die Listen alle leer sind.

empty_list([]). 
empty_list([e|X]):-empty_list(X).


% Prüft, ob das angegeben Board ein leeres Feld ist.

empty_board([]).
empty_board(Board):- 
		Board = [K|R],
		empty_list(K),
		empty_board(R).

		length(K,Len), % berechnet die Länge der ersten Zeile.
		row_length(R,Len), %TODO zeichenketten haben länge 1
		correct_form(K), %TODO Zeichenkette richtig prüfen
		correct_table(K,R).

% Prüft, ob die erste Zeile von der Form "| | |" ist.
		
correct_form(K):-
	K = [H|T],
	H == "|", % Prüft das erste Zeichen.
	correct_form2(T).
correct_form2(T):-
	T = [K|R],
	K == " ", % Prüft, ob jedes zweite Zeichen ein Leerzeichen ist.
	R = [F|B],
	F == "|", % Prüft, üb nach dem Leerzeichen ein | folgt.
	correct_form2(B).

% Prüft, ob die erste Zeile gleich mit allen anderen Zeilen ist.

correct_table(_,[]).
correct_table(K,R):-
		R = [H|T],
		K == H,
		correct_table(K,T).

% Vorläufiges test Objekt.
% empty_board(Board):- Board == 	[["| | | | | | | |"],
%				 ["| | | | | | | |"],
%				 ["| | | | | | | |"],
%				 ["| | | | | | | |"],
%				 ["| | | | | | | |"],
%				 ["| | | | | | | |"]].


% Prüft, ob alle Zeilen gleichlang sind.

row_length([],_).
row_length(Board, Length):-
		Board = [K|R],
		length(K,N),              %TODO fehler er gibt das Feld wieder nicht true.
		Length == N,
		row_length(R, Length).


% 2.
% Gibt das Spielfeld aus.

show_board([]).
show_board(Board):- 
Board = [K|R], 
write_only_string(K), 
nl, 
show_board(R). 

% Sorgt dafür, dass die Listenklammern verschwinden.

write_only_string([]).
write_only_string([K|R]):-
write(K),
write_only_string(R).

% Board = 
%	[["| | | | | | | |"],
%	 ["| | | | | | | |"],
%	 ["| | | | | | | |"],
%	 ["| | | | | | | |"],
%	 ["| | | | | | | |"],
%	 ["| | | | | | | |"]],
%
%	show_board(Board).

% 3.
% Prüft, ob ein Spieler gewonnen hat.
		%TODO win_board(Player, Board)  Number to win?


% 4.
% Prüft, ob es noch Züge gibt. Ansonsten wird das Feld gezeichnet.

draw_board(Board):- 
	Board = [K|_],
	full(K) % ,
	% write(show_board(Board),
	%	nl,
	%	"Unentschieden es gibt keine Züge mehr.")
.

% Prüft, ob die oberste Zeile voll ist (keine Züge mehr).

full(Top_row):-
	Top_row = [H|T],
	H =\= " ",
	full(T).


