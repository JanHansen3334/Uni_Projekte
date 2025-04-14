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
	                           
	                           bestNext(Col, State),
				   move(State, Col, NextState), !.
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

% ------------------AUFGABE 5-----------------------
%
% ----------------------MINIMAX--------------------------------%
% allNextStates sucht alle möglichen nächsten züge für den aktuelen
% state und schreibt diese in eine liste.
allNextStates(_,States,0,List) :- States = List.
allNextStates(State,States,Width,List) :- State=[Board|[_]], W is Width -1,
					  (columnPossible(Board,Width)
						-> move(State,Width,NextState), List1=[NextState|List], allNextStates(State,States,W,List1);
						allNextStates(State,States,W,List)).

% getMiniMax soll auf eine liste von states auf jedenstate minimax
% aufrufen und so eine liste von werten schreiben.
getMiniMax([], [], _).
getMiniMax([X|XS], [State|LS], H) :- miniMax(X, State, H),
				     getMiniMax(XS, LS, H).

% getMaxH ermittelt mittels akumolator argument (AkkH) den maximalen
% wert aus einer liste um dies einfacher aufzurufen wird an get max nur
% 2 argumente übergeben.
getMaxH(High, [], High).
getMaxH(High, [X|XS], AkkH) :- X>AkkH,
			       getMaxH(High, XS, X).
getMaxH(High, [X|XS], AkkH) :- X=< AkkH,

	                       getMaxH(High, XS, AkkH).
getMax(High, List) :- getMaxH(High, List, -10000000).

% getHighCikH liefert aus einer liste die stelle so das das listen elem
% das größte der liste ist.
getHighColH(BCol, [], _, _, BCol).
getHighColH(Col, [X|XS], Akku, High, BCol) :- X =< High,
	                                      NAkku is Akku +1,
	                                      getHighColH(Col, XS, NAkku, High, BCol).
getHighColH(Col, [X|XS], Akku, High, _) :- X > High,
				           NAkku is Akku +1,
			                   getHighColH(Col, XS, NAkku, X, NAkku).

getHighCol(Col, List) :- getHighColH(Col, List, 0, -10000000,_).

%-------------------------
% wie getMaxH nur das diesmal überprüft wird welches klein ist.
getMinH(High, [], High).
getMinH(High, [X|XS], AkkH) :- X>AkkH,
			       getMinH(High, XS, AkkH).
getMinH(High, [X|XS], AkkH) :- X=< AkkH,
	                       getMinH(High, XS, X).
getMin(High, List) :- getMinH(High, List, 10000000).


getLowColH(SCol, [], _, _, SCol).
getLowColH(Col, [X|XS], Akku, Low, _) :- X < Low,
					 NAkku is Akku +1,
					 getLowColH(Col, XS, NAkku, X, NAkku).
getLowColH(Col, [X|XS], Akku, Low, SCol) :- X >= Low,
					    NAkku is Akku +1,
					    getLowColH(Col, XS, NAkku, Low, SCol).

getLowCol(Col, List) :- getLowColH(Col, List, 0, 10000000,_).


% berechnet den index des besten next state
indexOf_single([Z|_],Z,1).
indexOf_single([_|T],Z,H) :- indexOf_single(T,Z,H1), H is H1 + 1.
% bestNext bestimt zu einem state die beste nächste Col s.d. der dadurch
% erzeugte state optimal ist. allNextStates liefert hier alle möglichen
% spielzüge.
bestNext(Col, State) :- widthBoard(Z),
			allNextStates(State, Posible, Z,[]),
				  State=[_|[Player]],
				  \+human(Player),
				  getMiniMax(R, Posible, 1),
				  getHighCol(HCol ,R),
                                  indexOf_single(Posible, Next, HCol),
                                  moveCol(Z, State, Col, Next).

bestNext(Col, State) :- widthBoard(Z),
			allNextStates(State, Posible,Z,[]),
				  State=[_|[Player]],
				  human(Player),
				  getMiniMax(R, Posible, 1),
				  getLowCol(HCol ,R),
                                  indexOf_single(Posible, Next, HCol),
                                  moveCol(Z, State, Col, Next).

% Returned die Spalte die geändert wurde
moveCol(0, State, Column, NewState):- Column = 7.
moveCol(Z, State, Column, NewState) :- State=[X|[XS]],NewState=[Y|[YS]], X = [H|T], Y=[K|R],(H=K -> B is Z-1, State1=[T|[XS]], NewState1=[R|[YS]], moveCol(B, State1,Column, NewState1); widthBoard(J), A is J +1, Column is A - Z).
                
%heightBoard(3).
%widthBoard(3).
%amountToWin(3).
%human(o).
% [[[[x,o,x],[o,x,o],[x," "," "]],o], [[[x,o," "],[o,x,o],[x,x," "]],o]]

%----------------minimaxImpl------------------------
% minimax haben wir dadurch implementiert das wir den baum "rekursiv"
% nach unten aufbaun indem wir die höhe des baumes mitgeben und solange
% minimax auf alle möglichen states anwenden.
% ist die höhe 0 springt unsere heuristik ein.
miniMax( 1000000, State, _) :- State = [Board|[CurrentPlayer]],
                			\+human(CurrentPlayer),
					 win_board(Board, CurrentPlayer).
miniMax(-1000000, State, _) :- State = [Board|[CurrentPlayer]],
                			\+human(CurrentPlayer),
					otherPlayer(CurrentPlayer,Player),
					 win_board(Board, Player).

miniMax(-1000000, State, _) :- State = [Board|[CurrentPlayer]],
                			 human(CurrentPlayer),
					 win_board(Board, CurrentPlayer).
miniMax( 1000000, State, _) :- State = [Board|[CurrentPlayer]],
                			human(CurrentPlayer),
					otherPlayer(CurrentPlayer,Player),
					 win_board(Board, Player).

miniMax(       0, State, _) :- State = [Board|[CurrentPlayer]],
					not(win_board(Board,CurrentPlayer)),
					otherPlayer(CurrentPlayer,Player),
					not(win_board(Board,Player)),
				         draw_board(Board).
%TODO State ist ein anderer State
miniMax(      Val,          States, 0) :- heuristik(Val, States).

miniMax(      Val, State, H) :- State = [Board|[CurrentPlayer]],
					 \+human(CurrentPlayer),
					 otherPlayer(CurrentPlayer,Player),
					 not(win_board(Board, Player)),
					 not(draw_board(Board)),
					 H >0,
					 widthBoard(Z),
					 allNextStates(State, Posible,Z,[]),
					 NewH is H-1,
					 getMiniMax(R, Posible, NewH),
					 getMax(Val, R).

miniMax(     Val, State, H) :- State = [Board|[CurrentPlayer]],
					human(CurrentPlayer),
					otherPlayer(CurrentPlayer,Player),
					not(win_board(Board, Player)),
					not(draw_board(Board)),
					H>0,
					widthBoard(Z),
					allNextStates(State, Posible,Z,[]),
					NewH is H-1,
					getMiniMax(R, Posible, NewH),
					getMin(Val, R).





% ------------------------------------------------------------------------
% ---------HEURISTIK------------------------------------
% wir haben die heuristik so implementiert das wir zunächst aus dem
% board alle GewinnReihen gesucht haben. und diesen dann werte zuordnen.
% ist in der gewinnreihe ein o bzw x zählt diese für den gegner nicht
% mehr. eine leere reihe hat auch den wert 0 !
% eine liste mit 1 x hat wert 10 2*x hat 100 usw.

% entfernt aus einer liste von listen alle leeren listen
leerRaus([],[]).
leerRaus(X,[[]|YS]) :- leerRaus(X, YS).
leerRaus([X|XS],[Y|YS]) :- Y \= [],
			   X = Y,
			   leerRaus(XS, YS).

% wie concat in haskell
conCat([],[]).
conCat([],[[]]).
conCat([Y|XS], [[Y|[]]|YSS]) :- conCat(XS, YSS).
conCat([Y|XS], [[Y,D|YS]|YSS]) :- conCat(XS, [[D|YS]|YSS]).

%TODO
% um festzustellen ob beispielsweise [x,x,x,x]in liste Y ist nutzen wir
% append der art das [x,x,x,x] mitbeliebigen listen List ergeben muss.
subList( Sublist, List ) :- append( [_, Sublist, _], List ).

% die erste spalte einer liste von listen erhalten wir dadurch das wir
% erst das erste elem gleich Y setzten und dann das für die restlichen
% listen wiederholen
ersteSpalte([],[]).
ersteSpalte([Y|YS], [[X|_]|XSS]) :- Y = X, ersteSpalte(YS, XSS).

% alleSpalten erhät eine liste von listen der gleichen länge und gibt
% dann eine liste von listen zurück die wenn man die listen
% untereinander schreibt die spalten erzeugt.
alleSpalten([],[[]|_]).
alleSpalten([Y|YS], [[X|XS]|XSS]) :- ersteSpalte(Y,[[X,_]|XSS]),
				     erstesElemEntfernen(Z,XSS),
				     alleSpalten(YS,[XS|Z]).
% um aus einer liste von listen jeweils das erste elem zu entfernen
% nutzen wir diese relation.
erstesElemEntfernen([], []).
erstesElemEntfernen([Y|YS], [[_|XS]|XSS]) :- Y = XS,
	                                     erstesElemEntfernen(YS, XSS).
% erhält zwei listen in der Y|YS steht eine liste mit der diagonalen
% ausgehen vom ersten elem der ersten liste in X|XS und der
% entspechenden diagonalen
midVertikalLR([], []).
midVertikalLR([], [[]|_]).
midVertikalLR([Y|YS], [[X|_]|XSS]) :- Y=X,
				      erstesElemEntfernen(Z, XSS),
				      midVertikalLR(YS, Z).

% allVertikalLRDown nutzt midVertikalLR um eine liste von listen zu
% erstelen die alle Vertikalen von links nach rechts unterhalb
% der mitleren vertikalen ausgibt.
allVertikalLRDown([], []).
allVertikalLRDown([], [[]|_]).
allVertikalLRDown([Y|YS], [X|XSS]) :- midVertikalLR(Y,[X|XSS]),
				      allVertikalLRDown(YS,XSS).

% allVertikalLRUp ist das gegenstück zu allVertikalLRDown nutzt aus das
% das spielfeld an der mitleren diagonalen gespiegeld es leicht macht
% alle Vertikalen zu berechnen.
allVertikalLRUp(Y,X) :- alleSpalten([_|ZS], X),
	                allVertikalLRDown(Y,ZS).

% allVertikalLR ist zusamen gesetzt aus allen vertikalen oberhalb und
% allen vertikalen unterhalb der mitleren diagonalen und fügt am ende
% nur die listen zusammen zu einer.
allVertikalLR(Y, X) :- allVertikalLRDown(Z,X),
	               allVertikalLRUp(L, X),
		       append(Z,L,Y).

% allVertikalRL nutzt nun aus das wenn das spielfeld umgedreht wird die
% relation allVertikalLR alle Vertikalen von Rechts nach Links erzeugt.
allVertikalRL(Y, X) :- alleReihenUmdrehen(Z, X),
		       allVertikalLR(Y, Z).

% umdrehenH ist eine relation die eine liste umgedreht vor die Liste
% Y setzt und falls die umzudrehende liste leer ist die die ergebnisse
% nach Rev schreibt
umdrehenH([], X, X).
umdrehenH([X|XS], Y, Rev) :- umdrehenH(XS, [X|Y], Rev).

% umdrehen ist nun eine Relation die Listen nimmt und überprüft ob die
% liste X umgedreht die liste Y ergibt
umdrehen(Y, X) :- umdrehenH(X, [], Y).

% alleReihenUmdrehen nimmt eine liste von listen und dreht jede einzelne
% liste um jedoch nicht die liste von listen
alleReihenUmdrehen([], []).
alleReihenUmdrehen([Y|YS], [X|XS]) :- umdrehen(Y, X),
				      alleReihenUmdrehen(YS,XS).

%TODO

% gewinnReihe mit subList und length erhalten wir alle Listen die in X
% enthalten sind und länge Stones in Row hat.
gewinnReihe(Rueck, X) :- amountToWin(Stones),
			 subList(Rueck, X),
			 length(Rueck, Stones).

% allGewinnReihenH erzeugt mit findall eine liste von listen von gewinn
% reihen.
alleGewinnReihenH([],[]).
alleGewinnReihenH([X|XS] ,[Y|YS]) :- findall(Z, gewinnReihe(Z, Y),X),
                                     alleGewinnReihenH(XS, YS).

% allGewinnReihen ruft zunächst die hilfsfunktion auf und schmeist dann
% alle leerenReihen raus da dies nun eine listen von listen von listen
% ist wenden wir concat an um eine liste von gewinnreihen zu erhalten.
allGewinnReihen( Rueck, Board) :- alleGewinnReihenH(R, Board),
	                           leerRaus(R2, R),
	                           conCat(Rueck, R2).

% hier und in den nächsten funktionen wird zunächst das spielfeld
% gedreht und dann bestimmt welche gewinnreihen in diesen vorhanden ist.
allGewinnSpalten(Rueck, Board) :- alleSpalten(R, Board),
				   allGewinnReihen(Rueck, R).

allGewinnDiagoLR(Rueck, Board) :- allVertikalLR(R, Board),
	                           allGewinnReihen(Rueck, R).

allGewinnDiagoRL(Rueck, Board) :- allVertikalRL(R,Board),
	                           allGewinnReihen(Rueck,R).

% allGewinnPosH liefert true falls Rueck, eine der listen von listen aus
% den hilfsfunktionen ist.
allGewinnPosH(Rueck, Board) :- allGewinnReihen(Rueck,Board);
			      allGewinnSpalten(Rueck,Board);
			      allGewinnDiagoLR(Rueck,Board);
			      allGewinnDiagoRL(Rueck,Board).

% allGewinnPosible erzeugt mitels findall eine liste von gewinn
% möglichkeiten.
allGewinnPosible(Rueck,Board) :- findall(Z, allGewinnPosH(Z, Board), R),
	                         leerRaus(R2,R),
				 conCat(Rueck, R2).

% inList ist wie member nur das nach dem ein elem gefunden wurde false
% ausgegeben wird selbst wenn noch einmal player in der liste ist.
inList(Player, [Player|_]).
inList(Player, [X|XS]) :- Player \= X,
	                  inList(Player, XS).

% getEmptyOrList schmeist unsere gewinnListe raus
% falls das sybol des gegners enthalten ist und ersetzt diese durch eine
% leere liste.
getEmptyOrList([], o, R) :- inList(x, R).
getEmptyOrList(R, o, R) :- not(inList(x, R)).

getEmptyOrList([], x, R) :- inList(o, R).
getEmptyOrList(R, x, R) :- not(inList(o, R)).

% removeAllWithOX wendet emptyOrList auf die gesamte liste von listen
% an.
removeAllWithOX([],_,[]).
removeAllWithOX([R|RS], Player, [X|XS]) :- getEmptyOrList(R, Player, X),
	                                   removeAllWithOX(RS, Player, XS).

% getListOX ist nun die endrelation die uns alle gewinnreihen in einer
% liste ausgibt die nicht das symbol des gegners besitzt.
getListOX(Rueck, Player, Board) :- allGewinnPosible(R, Board),
	                           removeAllWithOX(R2, Player, R),
				   leerRaus(Rueck,R2).

% extendesTimes überprüft ob ein symbol genau anz mal in einer liste
% enthalten ist.
extendesTimes(Player, 0,L) :- not(subList([Player], L)).
extendesTimes(Player, Anz, [X|XS]) :- Anz > 0,
	                              Player = X,
	                              NewAnz is Anz - 1,
				      extendesTimes(Player, NewAnz, XS).

extendesTimes(Player, Anz, [X|XS]) :- Anz > 0,
	                              Player \= X,
	                              extendesTimes(Player, Anz, XS).

% getList ist wie getEmptyOrList nur das es diesmal leer ist wenn es
% mehr oder weniger als anzal mahl player symbol in der liste enthalten
% ist.
getList([], Anz, Player, List) :- not(extendesTimes(Player, Anz, List)).
getList(L, Anz, Player, L) :- extendesTimes(Player, Anz, L).

% diese relation wendet getList auf alle Listen an
allExtendesNTimes( [], _, _ ,[]).
allExtendesNTimes([R|RS], Anz, Player, [X|XS]) :- getList(R, Anz, Player, X),
						  allExtendesNTimes(RS, Anz, Player, XS).

% allLists bzw allListsH sucht aus den gewinnListen alle listen mit 1
% elem und schreibt diese ins erste elem einer neuen liste.
% danach wiederholt sich das bis amountToWin grade die anzahl an steinen
% ist.
allListsH([], Anz, _, _ ):- amountToWin(X),
			    Anz is X + 1.
allListsH([R|RS], Anz, Player, Win) :- amountToWin(X),
	                              Anz =< X,
	                              allExtendesNTimes(Rueck, Anz, Player, Win),
	                              leerRaus(R,Rueck),
	                              NewAnz is Anz +1,
				      allListsH(RS, NewAnz, Player, Win).

allLists(Ruck, Player, Win) :- allListsH(Ruck, 1, Player, Win).

% diese relation weist einer bestimten anzahl an x bzw o einen wert zu
% also 10 für 1 x 100für 2 x und 1000für drei x usw.
punktProKastenH(Punkte, 0, Punkte).
punktProKastenH(Punkte, AnzX, R) :- AnzX > 0,
	                           NewR is 10*R,
                                   NewAnz is AnzX -1,
				   punktProKastenH(Punkte, NewAnz, NewR).

punktProKasten(Punkte, AnzX) :- punktProKastenH(Punkte, AnzX, 1).

% diese relation angewendet auf listen von listen erzeugt unsere
% heuristischen werte. dies geschied dadurch das durch allLists eine
% liste von listen von listen erzeugt wo in der ersten liste alle listen
% sind mit einem stein und in der zweiten mit zwei steinen usw.
% diese länge nehmen wir mal mit den punkten für die entsprechende
% anzahl.
gesamtPunkteH(Rueck, [], _, Rueck).
gesamtPunkteH(Rueck, [X|XS], AnzX, Akku) :- length(X, L),
				            punktProKasten(P, AnzX),
				            Points is L * P,
					    NewGesP is Akku + Points,
				            NewAnzX is AnzX + 1,
				            gesamtPunkteH(Rueck, XS, NewAnzX, NewGesP).


gesamtPunkte(Points, WinBoard) :- gesamtPunkteH(Points, WinBoard, 1,0).

% heuristikX erzeugt nun die punkte für player x und heuristik o für
% player o nur das player o der min player ist und somit der wert
% negativ ist.
heuristikX(Wert, State) :- State = [Board|[CurrentPlayer]],
				   getListOX(Rueck, CurrentPlayer, Board),
				   allLists(Rueck2, CurrentPlayer, Rueck),
				   gesamtPunkte(Wert, Rueck2).

heuristikO(Wert, State) :- State = [Board|[CurrentPlayer]],
				   getListOX(Rueck, CurrentPlayer, Board),
				   allLists(Rueck2, CurrentPlayer, Rueck),
				   gesamtPunkte(W, Rueck2),
				   Wert is 0 - W.

% die endgültige heuristik ergibt sich dadurch das das die werte für o
% und x zusammen gerechnet wird.
heuristik(Wert, State) :- State = [Board|[CurrentPlayer]],
				   \+human(CurrentPlayer),
				   otherPlayer(CurrentPlayer,Player),
				   heuristikX(W1,State),
				   NewState = [Board|[Player]],
	                           heuristikO(W2, NewState),
				   Wert is W1 + W2.
heuristik(Wert, State) :- State = [Board|[CurrentPlayer]],
				   human(CurrentPlayer),
				   otherPlayer(CurrentPlayer,Player),
				   NewState = [Board|[Player]],
				   heuristikX(W1,NewState),
	                           heuristikO(W2, State),
				   Wert is W1 + W2.
