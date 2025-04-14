% Autor: Jan Hansen, Timo Konrad
% Datum: 23.02.2017

% libary import
:-use_module(library(clpfd)).

%TODO
:- dynamic show_player/1.
:- dynamic init/1.
:- dynamic test/1.
% Player 1: x
% Player 2: o
% empty field: " "

token(x).
token(o).
token(" ").

% ------------------------------------------------------------------------------
% (Task 1) Determine if we have an empty board:
% An empty board is either an empty list
% or a list consisting of 1 to n (widthBoard) lists contains n-times the string " " (heightBoard).

empty(" ").
empty_and_long(X) :- length(X,L), heightBoard(H), L = H, maplist(empty,X).
empty_board(Board) :- length(Board,L), widthBoard(W), L = W, maplist(empty_and_long,Board), !.


%Test: empty_board([[" "," "," "," "," "],[" "," "," "," "," "],[" "," "," "," "," "],[" "," "," "," "," "]]).


% ------------------------------------------------------------------------------
% (Task 2) Implement "show_board" in form of "Pretty_Printing",
% that prints out a visually pleasing board
% The board is saved in a list of lists with every inner list representing a column
% Targeted presentation:
% | | | | | | | |
% | | | | | | | |
% | | |x| |x| | |
% | | |o|x|o| | |
% | | |o|o|x|x| |
% | | |o|x|o|x| |


%Test show_board([[x,x,x,o,o],[x,o,x,o," "],[x,x,o,x," "],[o,o," "," ",x]]).


% Print the last divider and change to new line
show_row([]) :- write("|\n").
% Fill in the token to its chosen place and print the dividers in between
show_row([H|T]) :- write("|"), show_player(H), show_row(T).

% Print the headline above the board
show_board([[]|_]) :- write("board:\n").
% Create printable lists for the rows, printed from last to first element
show_board(Board) :- getHeads(Board, Hs), getTails(Board, Ts), show_board(Ts), show_row(Hs).

% Get the first elements and combine them in a new list, the result is a printable row
getHeads([],R) :- R = [].
getHeads([H0],Ret) :- H0 = [H1|_], Ret = [H1].
getHeads([H0|T0],Ret) :- H0 = [H1|_], getHeads(T0,R),Ret = [H1|R].

% Remove the already printed elements and return the elements for the missing rows
getTails([],R) :- R = [[]].
getTails([H0],Ret) :- H0 = [_|T],Ret = [T].
getTails([H0|T0],Ret) :- H0 = [_|T1], getTails(T0,R),Ret = [T1|R].

% Functions to fill in the corresponding tokens
 show_player(x) :- write("x").
 show_player(o) :- write("o").
 show_player(_) :- write(" ").

% ------------------------------------------------------------------------------
% (Task 3) Define 'win_board(Player, Board)'
% to determine if the player has won the game


%Test o win win_board([[x,x,x,o,o],[x,o,x,o," "],[x,x,o,x," "],[o,o," "," "," "]],o).
%Test2 x dont win win_board([[x,x,x,o,o],[x,o,x,o," "],[x,x,o,x," "],[o,o," "," "," "]],x).
%Test3 x win win_board([[x,x,x,o,o],[x,o,x,o," "],[x,x,o,x," "],[o,o,x,o,x]],x). 


%TODO
% The amount of tokens to win by traditional rules
%AMOUNTTOWIN we dont need it after 4
% amountToWin(4).

% Ignore all elements after Y (works with line 97|getPos(Y,[_|T0],Result)) to get the chosen element
getPos(1,[H0|_],Result) :- Result = H0.
% Ignore all elements before Y in the column (works with line 105|getPos(1,Y,[H0|_],Result))
getPos(Y,[_|T0],Result) :- N is Y - 1, getPos(N,T0,Result). 
% Position in Liste von Listen, nutzt die Position in einfacher Liste 
% Get the first element of the list of lists
getPos(1,1,[[H0|_]|_],Result) :- Result = H0. 
% Return first element of chosen column (works with line 107|getPos(X,1,[_|T0],Result))
getPos(1,1,[H0|_],Result) :- Result = H0. 
% The columns coming after X (works with line 109|getPos(X,Y,[_|T0],Result)) will be ignored
getPos(1,Y,[H0|_],Result) :- getPos(Y,H0,Result). 
% Search the correct column (X)
getPos(X,1,[_|T0],Result) :- N is X - 1, getPos(N,1,T0,Result). 
% Select the column to work with (X), the other columns before X will be ignored
getPos(X,Y,[_|T0],Result) :- N is X - 1, getPos(N,Y,T0,Result).

% Determine if any of the diagonals contain any winner combinations

%TODO
% Width of the board
%WIDTH we dont need it after 4
% widthBoard(4).

%TODO
% Height of the board
%HEIGHT we dont need it after 4
% heightBoard(5).

% Returns the value at position (x,1)
getSingleDia(X,1,Board,Ret) :- getPos(X,1,Board,R), [R] = Ret, !.
% If the last element of a column is reached
% - then return the element at position (X,Y)
% - else go to the element left above the previous one and return the resulting list consisting of previous and following element
getSingleDia(X,Y,Board,Ret) :- (widthBoard(B), B == X
                                -> getPos(X,Y,Board,R), Ret = [R]
                                ; Nx is X + 1, Ny is Y - 1, getPos(X,Y,Board,W), getSingleDia(Nx,Ny,Board,R), Ret = [W|R]), !. 

% Generate the lower diagonals (running from bottom right to upper left) of the board from middle downward
getAllColDia(1,Board,Ret) :- getSingleDia(1,1,Board,R), Ret = [R], !.
getAllColDia(Y,Board,Ret) :- Ny is Y - 1, getSingleDia(1,Y,Board,R0), getAllColDia(Ny,Board,R1), [R0|R1] = Ret, !.
% We expand the pradicate with the height of our board
getAllColDia(Board,Res) :- heightBoard(S), getAllColDia(S,Board,Res), !.

% Generate the upper diagonals (running from bottom right to upper left) of the board from top of the board downward to the middle
getAllRowDia(2,Y,Board,Ret) :- getSingleDia(2,Y,Board,R), Ret = [R], !. 
getAllRowDia(X,Y,Board,Ret) :- Nx is X - 1, getSingleDia(X,Y,Board,R), getAllRowDia(Nx,Y,Board,Rx), Ret = [R|Rx], !.
getAllRowDia(Board,Res) :- widthBoard(S), heightBoard(K), getAllRowDia(S,K,Board,Res), !.

% Combine all generated diagonals in a list (from line 137 - 146)
getDownRightDia(Board,Ret) :- getAllRowDia(Board,R), getAllColDia(Board,C), append(R,C,Ret), !. 

% Rotate the board 90 degrees
rotateBoard(Dias,Rotated) :- transpose(Dias,X), maplist(reverse, X, Rotated).

% Check if the diagonals (running from bottom right to top left) hold a winning combination (combined in line 149)
winInRDiags(Board,Player) :- getDownRightDia(Board,X), wonInColumns(X,Player), !. 

                % for Rotated Board switch height and width
% Board (90 degree rotated) checked for winning combinations in diagonals (running from top right to bottom left) (combined in line 162)
winInLDiags(Board,Player) :- rotateBoard(Board,Rot), getRotDownRightDia(Rot,Res), wonInColumns(Res,Player), !.

% Combine all generated diagonals in a list of lists (rotated board)
getRotDownRightDia(Board,Ret) :- getRotAllRowDia(Board,R), getRotAllColDia(Board,C), append(R,C,Ret), !.

% Returns the value at position (x,1) in rotated board
getRotSingleDia(X,1,Board,Ret) :- getPos(X,1,Board,R), [R] = Ret, !.
% If the last element of a column is reached (rotated board)
% - then return the element at position (X,Y)
% - else go to the element left above the previous one and return the resulting list consisting of previous and following element
getRotSingleDia(X,Y,Board,Ret) :- (heightBoard(B), B == X
                                -> getPos(X,Y,Board,R), Ret = [R];
                                   Nx is X + 1, Ny is Y - 1, getPos(X,Y,Board,W), getRotSingleDia(Nx,Ny,Board,R), Ret = [W|R]), !.

% Generate the lower diagonals (running from bottom right to upper left) of the (rotated) board from middle downward
getRotAllColDia(1,Board,Ret) :- getRotSingleDia(1,1,Board,R), Ret = [R], !.
getRotAllColDia(Y,Board,Ret) :- Ny is Y - 1, getRotSingleDia(1,Y,Board,R0), getRotAllColDia(Ny,Board,R1), [R0|R1] = Ret, !.
% We expand the pradicate with the height of our board (width of rotated board)
getRotAllColDia(Board,Res) :- widthBoard(S), getRotAllColDia(S,Board,Res), !.

% Generate the upper diagonals (running from bottom right to upper left) of the (rotated) board from top downward to the middle
getRotAllRowDia(2,Y,Board,Ret) :- getRotSingleDia(2,Y,Board,R), Ret = [R], !. 
getRotAllRowDia(X,Y,Board,Ret) :- Nx is X - 1, getRotSingleDia(X,Y,Board,R), getRotAllRowDia(Nx,Y,Board,Rx), Ret = [R|Rx], !.
getRotAllRowDia(Board,Res) :- heightBoard(S), widthBoard(K), getRotAllRowDia(S,K,Board,Res), !.
                %TODOEnd

% Player wins if one of his tokens (Parameter Player) equals H
columnWin([H|_],1,Player) :- H = Player, !.
% Checks the first x elements for a winner combination
columnWin([H|T],X,Player) :- H = Player, N is X-1, columnWin(T,N,Player), !.
% Ripple through one column to check for a winner combination
columnWin(X,Player) :- X = [_|T], amountToWin(N), (columnWin(X,N,Player); columnWin(T,Player)), !.

% Check for a winner combination column by column
wonInColumns([H],Player) :- columnWin(H,Player), !.
wonInColumns([H|T],Player) :- columnWin(H,Player); wonInColumns(T,Player), !.

% Check bottom row for a winner combination and continue from bottom to top
wonInRows(X,Player):- getHeads(X,H),getTails(X,T), (columnWin(H,Player);wonInRows(T,Player)), !.

% Check for a winning player
win_board(Board,Player) :- wonInRows(Board,Player); wonInColumns(Board,Player); winInRDiags(Board,Player); winInLDiags(Board,Player), !.

% ------------------------------------------------------------------------------
% (Task4) Define 'draw_board(Board)' to determine
% if the board is completely filled


%Test draw_board([[x,x,x,o,o],[x,o,x,o," "],[x,x,o,x," "],[o,o," "," "," "]]).
%Test2 draw_board([[x,x,x,o,o],[x,o,x,o,x],[x,x,o,x,o],[o,o,x,x,x]]).


% An empty board is full
isFull([]).
% Check if H containts token x or o
isFull([H]) :- H \= " ", !.
% Check if headelement is empty and continue with tail
isFull([H|T]) :- H \= " ", isFull(T), !.

% Check if list of empty list is full, maybe, who knows? Half-Life 3 confirmed!
draw_board([[]]).
% Check if a list is full
draw_board([H]) :- isFull(H), !.
% Check if a list of lists is full
draw_board([H0|T0]) :- isFull(H0), draw_board(T0), !.


% ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

% (2)
% (Task 1) Define 'move(State, Column, NextState)' to
% check if the intented turn is a valid one
% More detailed would be, that we want to test if the chosen column can hold
% another token without exceeding the maximum height of our board

% 'State'  holds the current board and the player whose turn it is
% 'Column' holds the column chosen by the player who has the next turn

% (Idea) The idea would be to count down 'column' and another predicate to
% compare the lenght of the targeted column to the maximum height of the board.
% That results in the following two cases:
% lenght(Column)  < height of board =   valid,
% lenght(Column) => height of board = invalid

% state = [Board, CurrentPlayer]
% Switch players
otherPlayer(x,o).
otherPlayer(o,x).


%Test move([[[x,x,x,o,o],[x,o,x,o," "],[x,x,o,x," "],[o,o," "," "," "]],x],4,NewState).


% If the turn is correct set NewState on NewBoard and NewPlayer
move(State, Column, NewState) :- 
                State = [Board|[Player]],
                columnPossible(Board,Column),
                placeInColumn(Board,Player,Column,NewBoard),
                otherPlayer(Player,NewPlayer),
                NewState = [NewBoard|[NewPlayer]], !.
% If the turn is incorrect, we return state unchanged as new state
move(X,_,Y) :- X = Y, !.

% Check if the last element of the column is empty
columnPossible(Board,Column) :- heightBoard(Top), getPos(Column,Top,Board,Value), Value = " ", !.

% Place the symbol in the column chosen by the player
place([H|T],Player,Ret) :- H = " ", Ret = [Player|T], !.
place([H|T],Player,Ret) :- place(T,Player,R), Ret = [H|R], !.

% Going to the correct column and place the player's symbol in column.
placeInColumn([H|T],Player,1,Ret) :- place(H,Player,R), Ret = [R|T], !.
placeInColumn([H|T],Player,Column,Ret) :- Col is Column - 1, placeInColumn(T,Player,Col,R), Ret = [H|R], !.

% Check if the chosen column is in Board range
safeColumn(X) :- integer(X), widthBoard(End), X > 0, (X < End; X = End), !.


% ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

% (3)
% (Task 1) Define 'turn(State, NextState)' to implement the transition
% from 'State' to 'NextState', only used if 'move(State, Column, NextState)'
% tested for a valid turn.

% The new token will be added to the current board and the indicator for
% the next player taking turn will be changed by the following cases:
% - if Player x made a valid move, Player y has the next turn
% - if Player x made an invalid move, Player x has to make his turn again

%TODO
%HUMANPLAYER we dont need it after 4
% human has the symbol x
% human(x).
% human turn:


%TEST turn([[[x,x,x,o,o],[x,o,x,o," "],[x,x,o,x," "],[o,o," "," "," "]],x],NewState).


% If CurrentPlayer is the human player, we ask which column he wants.
% If the chosen column has a free slot, the token will be added to the
% board and the current player will be changed.
turn(State,NextState) :- State = [_|[CurrentPlayer]],
                         human(CurrentPlayer),
                         write("Choose your column: "),
                         read(Column),
                         safeColumn(Column),
                         move(State,Column,NextState), !.
% If the chosen column is full, ask for another column
turn(X,Y) :- X = [_|[CurrentPlayer]],
            human(CurrentPlayer),
            write("\nYour chosen column has no space or doesn't exist. Choose another column: \n"),
            turn(X,Y), !.


% Computer's turn:
% random_between(+L:int, +U:int, -R:int) is semidet
% Binds R to a random integer in [L,U] (i.e., including both L and U). Fails silently if U<L.

%TODO
% Not needed after 5
% the computer choose a random column if its his turn.
% turn(State,NextState) :-
%                 State = [_|[CurrentPlayer]],
%                 \+human(CurrentPlayer),
%                 write("Computer does move\n"),
%                 widthBoard(Columns),
%                 random_between(1,Columns,Column),
%                 move(State,Column,NextState), !.
%% if the chosen column is full, the computer try again a random column
%turn(X,Y) :- turn(X,Y), !.

%TODO for 5
% Check if it's the AI's turn, get the board's width and check for
% best possible turns (with checkForWin(State,Columns,Columns,Ret))
turn(State,NextState) :-
                State = [_|[CurrentPlayer]],
                \+human(CurrentPlayer),
                write("Computer does move\n"),
                widthBoard(Columns),
		checkForWin(State,Columns,Columns,Ret),                
		move(State,Ret,NextState), !.
% If the chosen column is full, the computer tries again with another column
turn(X,Y) :- turn(X,Y), !.


% ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

% (4)
% (Task 1) Make sure that the game runs the available actions in a correct order,
% that includes:
% - start of the game with the choice of the token for the human player
% - show the board at the beginning of every turn
% - x always makes the first move!

% Initialize the game (connect(start) included), for a restart -> restart Prolog
%Test connect(init).
%Test connect(start). human, amountToWin, heightBoard, widthBoard have to be 'in a comment'.


% t starts with the original message, f is used in case of invalid input
choseStart(f) :- write("\nPlease answer with y/n, look out for noncapital letters!\n"), choseStart(t), !.

% asking for board attributes
choseStart(t) :- write("Do you want to start? y/n: "),
                 read(Answer),
                 (Answer == y
                        -> write("Your symbol is x.\n");
                           write("Your symbol is o.\n")),
                 startAssert(Answer).
choseHeight(t) :- write("What's your desired number of rows?: "), read(Answer),heightAssert(Answer).
choseWidth(t) :- write("What's your desired number of columns?: "), read(Answer),widthAssert(Answer).
choseN(t) :- write("How many tokens should be required to win?: "), read(Answer),nAssert(Answer).

% Initialize the Board (width, height), amountToWin, starting player (computer|player) with x.
startAssert(y) :- assert(human(x)), !.
startAssert(n) :- assert(human(o)), !.
startAssert(_) :- choseStart(f).
% The player should choose a new height if height < 2.
heightAssert(X) :- integer(X), X > 1,
                   assert(heightBoard(X)), !.
heightAssert(_) :- write("Height should be greater than 1!\n"),
                   choseHeight(t).
% The player should choose a new width if width < 2.
widthAssert(X) :- integer(X), X > 1,
                  assert(widthBoard(X)), !.
widthAssert(_) :- write("Width should be greater then 1!\n"),
                  choseWidth(t).
% The player should choose a new amountToWin if amountToWin < 2.
nAssert(X) :- integer(X), X > 1,
              assert(amountToWin(X)), !.
nAssert(_) :- write("Number to win should be greater than 1!\n"),
              choseN(t).

% If the initialization fails, restart Prolog.
init(f).
connect(init) :- init(t), write("For new initialization restart swipl."), !.
% Start initialization and start a new game.
connect(init) :- \+init(t),
                write("Welcome Player! =)\n"),
                assert(init(t)),
                choseStart(t),
                choseHeight(t),
                choseWidth(t),
                choseN(t),
                connect(start).
% Start the game
% check if board is empty
connect(start) :- empty_board(Board), StartState = [Board|[x]], connect(StartState), !.
% show the board and start next turn
connect(State) :- State = [H|_], show_board(H), turn(State,NextState), continue(NextState), !.
% information to initialising the game.
connect(_):- write("After initializing one time, you need to restart swipl and the game file to initialize again.").

% check if player wins
continue(State) :- State = [Board|[Player]], otherPlayer(Player,PlayerX), win_board(Board,PlayerX), show_board(Board), show_player(PlayerX), write(" WON!\n"), !.
% check if board is full
continue(State) :- State = [Board|_], draw_board(Board), write("No one wins!\n"), show_board(Board), !.
% if game not end go to next turn.
continue(State) :- connect(State).


% ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

% (5)
% (Task 1) Add an AI to the implementation with the following abilities:
% - avoid random turns
% - take the best turn available


% (Idea)
% Check every column for a win with the next turn
% and for a victory of the opposing player.

% Prüfe in welcher Spalte man am meisten Nachbarn hat UND prüfe, ob der Gegner mit seinen Nachfolgezug gewinnt,
%        wenn ja dann prüfe für die nächste Spalte mit den als nächstes meiste anzahl Nachbarn,
%        wenn keine Spalte übrig ist nehme beliebige Spalte. 

%TODO in (3)
% computer chose a column to win.
%turn(State,NextState) :-
%                State = [_|[CurrentPlayer]],
%                \+human(CurrentPlayer),
%                write("Computer does move\n"),
%                widthBoard(Columns),
%               checkForWin(State,Columns,Columns,Ret),
%               move(State,Ret,NextState), !.
%% if the chosen column is full, the computer try again an other column
%turn(X,Y) :- turn(X,Y), !.
%TODO

% Check for a winner combination, if all columns are checked call 'stopWinning' with state (with human player)
checkForWin(State,0,Columns,Ret) :- 
		State = [Board|[Player]], 
		otherPlayer(Player,Human), 
		NewState = [Board|[Human]], 
		stopWinning(NewState,Columns,Columns,Ret), !.
checkForWin(State,Col,Columns,Ret) :- 
		(State = [Board|[Player]], 
		columnPossible(Board,Col), 
		placeInColumn(Board,Player,Col,NewBoard)
		, win_board(NewBoard,Player)
                        -> Ret = Col;
                        Column is Col - 1, 
			checkForWin(State,Column,Columns,Ret)), !.




% Check if the human player can win the game with her/his/its next turn,
% - if she/he/it can, counter the turn
% - else call 'intelligentMove' (with state including AI as player)
stopWinning(State,0,Columns,Ret) :- 
		State = [Board|[Player]], 
		otherPlayer(Player,Computer), 
		NewState = [Board|[Computer]], 
		intelligentMove(NewState,Columns,Ret), !.
stopWinning(State,Col,Columns,Ret) :- 
		(State = [Board|[Player]], 
		columnPossible(Board,Col), 
		placeInColumn(Board,Player,Col,NewBoard), 
		win_board(NewBoard,Player)
                        -> Ret = Col;
                        Column is Col - 1, 
			stopWinning(State,Column,Columns,Ret)), !.


%TODO
% intelligentMove prüft, ob der menschliche Spieler gewinnt wenn er in die gleiche Spalte wirft wie der Spieler evtl. auch in Spalte die meherere Nachbarn hat vom Computer(Max).



intelligentMove(State,Columns,Ret) :-	
					State = [Board|[_]],
					allPossibleColumns(Board,Columns,Return,[]),   
					% macht move mit col elem von AllPossiCol 
					maybeTurn(State,Return,NextStateList,[]),
					% verkleinert allpossiCol auf Cols wo gegner nicht dannach gewinnen kan
					searchTurn(NextStateList,PossibleList,[]),
					% führt computer Turn mit verkleinerter col liste aus.
					computerTurn(State,PossibleList,ComputerTurnList,[]),
					betterTurn(ComputerTurnList,BetterReturn),
					% BetterReturn Liste mit gewinn nach 2 Computer Zügen
					(BetterReturn == [] 
						% PossibleList Züge wo der Human nicht gewinnt
						-> (PossibleList == [] 
						% Return Liste aller Möglichen Züge (Computer verliert)
							-> choose(Return,Elem),
							Ret = Elem;			
							choose(PossibleList,Elt),			
							Ret = Elt);
						choose(BetterReturn,El),
						Ret = El), !.


% return list of all columns with empty Slot
allPossibleColumns(Board,1,Ret,List) :- 
		(columnPossible(Board,1) 
			-> Ret = [1|List]; 
			Ret = List), !.
allPossibleColumns(Board,X,Ret,List) :- 
		(columnPossible(Board,X) 
			-> List2 = [X|List], 
			Xs is X - 1, 
			allPossibleColumns(Board,Xs,Ret,List2); 
			Xs is X - 1, 
			allPossibleColumns(Board,Xs,Ret,List)), !.


% return a list of all NewStates (possible states with necessary columns)
maybeTurn(_,[],ListOfNewStates,List) :- ListOfNewStates = List, !.
maybeTurn(State,ColList,ListOfNewStates,List) :- 
		ColList = [H|T], 
		move(State,H,NewState),
		NewList = [[NewState|[H]]|List], 
		NewColList = T, 
		maybeTurn(State,NewColList,ListOfNewStates,NewList), !.

% returns a list of Columns, with valid turns without a following win for the opposing human player 
% move(State,Columns,NextState) don`t let the human player win.
searchTurn([],PossibleList,List) :- PossibleList = List, !.
searchTurn(NextStateList,PossibleList,List) :- NextStateList = [H|T],
						H = [State|[Col]], 
						State = [Board|[Player]], 
						(columnPossible(Board,Col) 
							-> move(State,Col,NewState),
							(NewState = [NewBoard|_], 
							win_board(NewBoard,Player) 
								-> List1 = List; 
								List1 = [Col|List]); 
							List1 = [Col|List]),
						NextStateList1 = T, 
						searchTurn(NextStateList1,PossibleList,List1), !.



% Computer führt alle Möglichkeiten aus in denen der Gegner nicht im Folgezug gewinnt.
% Returned Folgezustand mit ausgeführter Column als Liste.		
computerTurn(_,[],ComputerTurnList,List) :- ComputerTurnList = List, !.
computerTurn(State,PossibleList,ComputerTurnList,List) :- 
		PossibleList = [Col|T], 
		move(State,Col,NewState), 
		List2 = [NewState|[Col]], 
		List1 = [List2|List], 
		computerTurn(State,T,ComputerTurnList,List1), !.


% Wenn derComputer in zwei Zügen gewinnen kann, returned die WinColumns eine Liste mit Columns bei denen die Möglichkeit besteht in zwei Zügen zu gewinnen.
betterTurn(NextStateList,BetterReturn) :- 
		nextStateHuman(NextStateList,NextStateListHuman,[]),
		nextStateComputer(NextStateListHuman,ComputerTurnTwo,[]), 
		winInTwoTurns(ComputerTurnTwo,WinColumns,[]), 
		BetterReturn = WinColumns, !.


% erstellt eine Liste mit Listen von States in denen der Human auf den Zug reagiert hat.
nextStateHuman([],NextStateListHuman,List) :- NextStateListHuman = List, !.
nextStateHuman(StateList,NextStateListHuman,List) :- 
		StateList = [StateCol|T], 
		StateCol = [State|[Colu]], 
		State = [Board|_], 
		widthBoard(Columns), 
		allPossibleColumns(Board,Columns,Col,[]), 
		nextStep(State,Col,NextStepList,[],Colu), 
		append(NextStepList,List,List1), 
		nextStateHuman(T,NextStateListHuman,List1), !.


% nextStep bekommt einen State eine Liste von Columns mit freiem Platz und gibt eine Liste von nextStates mit Column die der Computer am Anfang gewählt hat wieder, wo move für jede Column einmal ausgeführt wurde.
nextStep(_,[],NextStepList,List,_) :- NextStepList = List, !.
nextStep(State,Col,NextStepList,List,Colu) :- 
		Col = [Column|ColX], 
		move(State,Column,NextState), 
		List2 = [NextState|[Colu]], 
		List1 = [List2|List], 
		nextStep(State,ColX,NextStepList,List1,Colu), !.


% erstellt eine Liste mit Listen von States in denen der Computer auf einen Zug reagiert den der Human gemacht hat, nach dem Human nicht gewinnen kann.
nextStateComputer([],ComputerTurnTwo,List) :- ComputerTurnTwo = List, !.
nextStateComputer(Head,ComputerTurnTwo,List) :- 
		Head = [StateCol|T], 
		StateCol = [State|[Colu]], 
		State = [Board|_], 
		widthBoard(Columns), 
		allPossibleColumns(Board,Columns,Col,[]), 
		nextStep(State,Col,NextStepList,[],Colu), 
		append(NextStepList,List,List1), 
		nextStateComputer(T,ComputerTurnTwo,List1), !.

  

% holt alle Columns wo Computer in zwei Zügen gewinnen kann.
winInTwoTurns([],WinColumns,List) :- WinColumns = List, !.
winInTwoTurns(ComputerTurnTwo,WinColumns,List) :- 
		ComputerTurnTwo = [StateCol|T], 
		StateCol = [State|[Colu]], 
		State = [Board|[Player]], 
		otherPlayer(Player,PlayerX), 		
		(win_board(Board,PlayerX) 
			-> List1 = [Colu|List]; 
			List1 = List), 
		winInTwoTurns(T,WinColumns,List1), !.


% choose(List, Elm) - chooses a random element
% in List and unifies it with Elm.
choose([], []).
choose(List, Elm) :-
        length(List, Length),
        random(0, Length, Index),
        nth0(Index, List, Elm), !.

