%----------------------AUFGABE1-------------------------%


%------------------Hilfs relationen---------------------%

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


% um aus einer liste von listen jeweils das erste elem zu entfernen
% nutzen wir diese relation.
erstesElemEntfernen([], []).
erstesElemEntfernen([Y|YS], [[_|XS]|XSS]) :- Y = XS,
	                                     erstesElemEntfernen(YS, XSS).

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


% wir haben die leere Zeile durch leerzeichen " " definiert sind alle
% zeichen einer liste Leerzeichen so ist diese leer
emptyRow([" "]).
emptyRow([X|XS]) :- X=" ", emptyRow(XS).

% um ein Board auszugeben wollten haben wir zunächst eine Line ausgeben
% wollen dies funktioniert so das wir erst | ausgeben und danach das
% listen element
showLine([]).
showLine([X|XS]) :-  write('|'), write(X),
	             showLine(XS).

% show_Index soll für unser spielfeld eine numerierung der spalten
% erzeugen ähnlich wie show line soll hier auch zwischen den zeichen ein
% | stehen damit die zeilen noch besser von einander unterschieden
% werden können.
show_Index(L) :- width(L).
show_Index(L) :- width(X),
		 L<X,
	         write('|'),
		 write(L),
		 NL is L + 1,
		 show_Index(NL).

% die liste der reihe nach mittels
% showLine ausgegeben und damit am ende jeder zeile ein "|" ist haben
% wir dieses hier eingefügt sowie den absatzt
show_boardH([]).
show_boardH([Z|ZS]) :-  showLine(Z),write('|'),nl,show_boardH(ZS).

% show_Lineeee ist eine zeile der länge L die nur '=' ausgeben soll
% diese soll später zwischen dem spielfeld und der index zeile stehen.
show_LINEEEE(L) :- width(L).
show_LINEEEE(L) :- width(X),
		   L<X,
	           write('='),
		   NL is L + 1,
		   show_LINEEEE(NL).

% eine reihe ist dann voll wenn sie kein leeres Element enthält also
% wenn keines der element =" "
fullRow([]).
fullRow([X|XS]) :- X\=" ", fullRow(XS).

% falls ein spieler 4 bzw N gleiche in einer reihe hat muss entweder
% sein zeichen in X enthalten sein oder in einer anderen Liste um zu
% gewinnen makeList erzeugt hier eine liste die nur den char des players
% enthält.
winRows(Player, [X|XS]) :- stonesInRow(L),
	                   makeList(Ruck, L, Player),
	                   subList(Ruck, X);
                           winRows(Player, XS).

% winColl dreht das spielfeld und wendet dann winRows an.
winColl(Player, X) :- alleSpalten(Y, X),
		      winRows(Player, Y).

% winVertikalLR erzeugt zunächst alle vertikalen aus dem Board (X) und
% überprüft dann ob eine gewinn situation vorliegt.
winVertikalLR(Player , X) :- allVertikalLR(Z,X),
	                     winRows(Player, Z).

% wie winVertikalLR nur mit anderen Vertikalen
winVertikalRL(Player, X) :- allVertikalRL(Y, X),
			    winRows(Player, Y).


%---------------------Wichtige Relation------------------------------

% ein ganzes spielfeld ist leer wen jede einzelne reihe leer ist.
empty_board([]).
empty_board([R|RS]) :- emptyRow(R), empty_board(RS).

% show_board nutzt die hilfs relationen show_BoardH und showLine wie
% show Index hier soll zunächst ein absatzt ausgegeben werden und dann
% das spielfeld die linie "=" muss doppelt ausgegeben werden da doppelt
% soviele chars ex wegen "|".
show_board(Board) :- nl,
	             show_boardH(Board),
	             show_LINEEEE(0),
		     show_LINEEEE(0),
		     write('='),
		     nl,
		     show_Index(0),
		     write('|'),
		     nl.

% unentschieden herscht sobalt die oberste reihe voll ist also die erste
% der liste
draw_board([]).
draw_board([X|XS]) :- fullRow(X),
	             draw_board(XS).



% win_board ist nun das zusammen setzt mittels oder von winRow , coll,
% oder vertikalen.
win_board(Player, Board) :- winRows(Player, Board);
                            winColl(Player, Board);
			    winVertikalRL(Player, Board);
			    winVertikalLR(Player, Board).


%----------------------------AUFGABE2-------------------------------%

%----------------------------Definition-----------------------------%
% in state speichern wir ein Board und wer grade am zug ist da das Board
% beliebig ist ist es hier unterstrichen
state(o,_).
state(x,_).

%----------------------------HielfsRelationen-----------------------%

%
indexOf_single([Z|_],Z,0).
indexOf_single([_|T],Z,H) :- indexOf_single(T,Z,H1), H is H1 + 1.


% possibleCollum überprüft ob zu einem board in liste X an stelle Column
% ein " " steht also ob es leer ist. die abfragen integer(Column) und
% Column >=0 , Column <L sorgen zusätzlich dafür das keine buchstaben oä
% denn wert true erzeugen.
possibleCollum([X|XS], Column) :- length(X,L),
	                          integer(Column),
	                          Column >=0, Column <L,
				  indexOf_single(X, " ", Column);
                                  possibleCollum(XS , Column).

% getRow sucht aus einer liste von listen die Ztes Liste Raus.
getRow(X, [X|_], 0).
getRow(R, [_|XS], Z) :- getRow(R, XS, Zl), Z is Zl+1.

% sameLength füllt eine Liste mit " " auf bis sie die länge der zweiten
% liste hat.
sameLength(Y, [E|R],[W|T]) :- length([E|R],K),
	                      length([W|T],X),
			      K<X,
			      sameLength(Y, [" ",E|R], [W|T]).
sameLength([E|R], [E|R],[W|T]) :-length([E|R],X),
				 length([W|T],X).


% putChar nimmt eine liste und fügt einen Char an der ersten stelle ein
% die /= " " ist.
putChar( L, [], C) :- append([C],[],L).
putChar( Y , [" "|XS], C) :- putChar(Y, XS, C).
putChar(Y, [X|XS], C) :- X \= " ",
	                 append([C], [X|XS], Y).


% getNewList nutzt putChar und sameLength um nach dem putChar aufruf die
% rest stellen wieder aufzufüllen und so die nach dem zug entstehende
% collum zu kreieren.
getNewList([Person] , [], Person).
getNewList(Z , Y, Person) :- putChar(L, Y, Person),
	                     sameLength(Z, L, Y).

% getNewColl erhält einen Player, Board eine Column und schriebt nach L
% die nun erzeugte neue Liste bzw collumn
getNewColl(Player,State, Column, L) :- alleSpalten(Z,State),
				       getRow(R, Z, Column),
				       getNewList(L, R, Player).

% diese Relation erhält ein Board und eine Zahl und gibt alle Rows raus
% die vom index kleiner sind als diese zahl.
getBelowRows([], _ , 0).
getBelowRows([X|XS], [Y|YS], Z) :- X = Y,
	                           getBelowRows(XS,YS,Zl),
				   Z is Zl + 1.
% wie BelowRows nur das das spielfeld umgedreht wird und die anzahl
% berechnet wird.
getUpperRows(X,Y,Z) :- length(Y,L),
		       D is L - Z,
		       D2 is D  -1,
		       umdrehen( R, Y ),
		       getBelowRows( R2, R, D2),
		       umdrehen(X, R2).

% makeWholeAgain erhält zwei Listen von Listen und eine liste und fügt
% diese derart zusammen das die eine liste zwischen der ersten un der
% zweiten ist.
makeWholeAgain(New ,X ,L ,Y ) :- append(X,[L],Z),
	                         append(Z,Y,New).


% switchState erhält einen state und eine column und erzeugt einen neuen
% state hier wird noch nicht auf falsche eingaben geachtet!!!
switchState(state(x,Anfang),Column,state(o, X)) :-  getNewColl(x, Anfang, Column, L),
					            alleSpalten(R, Anfang),
					            getBelowRows(Z , R, Column),
					            getUpperRows(Z2, R, Column),
					            makeWholeAgain(End, Z, L, Z2),
					            alleSpalten(X, End).


switchState(state(o,Anfang),Column,state(x, X)) :-  getNewColl(o, Anfang, Column, L),
					            alleSpalten(R, Anfang),
					            getBelowRows(Z , R, Column),
					            getUpperRows(Z2, R, Column),
					            makeWholeAgain(End, Z, L, Z2),
					            alleSpalten(X, End).

%----------------------Wichtige Relation----------------------------%

% hier überprüfen wir zunächst ob die eingabe möglich ist und wenn ja
% ändern wir diesen state mittels switchState.
move(state(o,[Y|YS]),Column, NextState) :- possibleCollum([Y|YS], Column),
					   switchState(state(o,[Y|YS]),Column,NextState).

move(state(x,[Y|YS]),Column, NextState) :- possibleCollum([Y|YS], Column),
					   switchState(state(x,[Y|YS]),Column,NextState).



%------------------Aufgabe3-----------------------------------------

% ----------------------------HilfsRelation-----------------%
% diese soll falls der spielzug möglich ist diesen ausführen sonst soll
% sie nach einem neuen wert fragen.
moveOrTurn(State, Collumn, NextState) :- move(State, Collumn, NextState);
					 turn(State, NextState).
% -----------------------------Wichtiges----------------------------------%

% Player:
% hier nutzen wir paternMatching da entweder player o ist oder x aber
% nicht beides. falls der spieler drann ist nutzen wir write wie in der
% show metode zu auforderung und read zum einlesen.
turn(state(o,X), NextState) :- person(o),
	                       write("number pls"),
	                       nl,
	                       read(Z),
	                       moveOrTurn(state(o,X), Z, NextState).

turn(state(x,X), NextState) :- person(x),
			       write("number pls"),
	                       nl,
	                       read(Z),
	                       moveOrTurn(state(x,X), Z, NextState).


% Computer:
% wie beim Player nutzen wir auch hier paternMatching jedoch wird hier
% mitels bestNext die beste Column rausgesucht und dann weiter gegeben.
turn(state(o,[X|XS]),NextState) :- computer(o),
	                           write("computer playing"),
	                           nl,
	                           bestNext(Col, state(o,[X|XS])),
				   moveOrTurn(state(o,[X|XS]), Col, NextState).



turn(state(x,[X|XS]),NextState) :- computer(x),
	                           write("computer playing"),
	                           nl,
	                           bestNext(Col, state(x, [X|XS])),
				   moveOrTurn(state(x,[X|XS]), Col, NextState).





% -----------------------------Aufgabe4---------------------------------------

%----------------------------Definitionen----------------------%
width(7).
hight(6).
stonesInRow(4).
computer(x) :- person(o).
computer(o) :- person(x).

% spielEnde ist dazu dar festzustellen ob entweder eine gewinnsituation
% vorliegt oder unentschieden und wenn beides nicht vorliegt soll das
% spiel vortgesetzt werden.
spielEnde(state(x,Board),NextState) :- (win_board(x,Board) ,write("Congratulation Player x has won."));
                                       (draw_board(Board),write("The game resulted in a Draw"));
                                       spielfluss(state(o,Board),NextState).

spielEnde(state(o,Board),NextState) :- (win_board(o,Board) ,write("Congratulation Player o has won"));
                                       (draw_board(Board),write("The game resulted in a Draw"));
                                       spielfluss(state(x,Board),NextState).


% spielfluss ist eine "endlosSchleife" die aufgreufen wird bis spielEnde
% True ausgiebt.
spielfluss(state(x,Board),NextState) :- nl,
			                turn(state(x,Board),state(o,NextBoard)),
				        show_board(NextBoard),
				        spielEnde(state(x,NextBoard),NextState).

spielfluss(state(o,Board),NextState) :- nl,
			                turn(state(o,Board),state(x,NextBoard)),
			                show_board(NextBoard),
					spielEnde(state(o,NextBoard),NextState).

% makeListH soll zu gegebenem Char (C) gegebenem Int eine liste bauen
% die den char C N mal enthält.
makeListH(Ruck, 0, _, Ruck).
makeListH(Ruck, Int, C, L) :- Int >= 0,
	                      append([C],L, Z),
			      X is Int-1,
			      makeListH(Ruck, X, C, Z).


makeList(Ruck, Int, C) :- makeListH(Ruck, Int, C, []).

% erzeugt ein "leeres spielfeld" mit höhe und Breite,
makePlayingField(Board) :- width(Breite),
	                   hight(Hoehe),
	                   makeList(R, Breite, " "),
	                   makeList(Board, Hoehe, R).
% readPlayer ruft sich so lange selber auf bis der spieler entweder x
% oder o eingibt und so das spiel gestartet werden kann.
readPlayer(X) :- write("chose ur role"),
	         nl,
	         (read(X),
		 (X = x;
		  X = o));
		 readPlayer(X).

% hier wird nun das spiel gestartet hierzu wird erst ein spielfeld
% gebaut dann die auswahl des spielers eingelesen und anschliesend mit
% asserta(person(X)) person gesetzt um mehrfaches spielen zu ermöglichen
% wird nach dem spiel person wieder zurückgesetzt.
fourWinning :- makePlayingField(Board),
	       empty_board(Board),
	       readPlayer(X),
	       asserta(person(X)),
	       show_board(Board),
	       spielfluss(state(x,Board),_),
	       retract(person(X)).


% ------------------AUFGABE 5-----------------------
%
% ----------------------MINIMAX--------------------------------%
% allNextStates sucht alle möglichen nächsten züge für den aktuelen
% state und schreibt diese in eine liste.
allNextStates(state(x,Board),States) :-findall(X,switchState(state(x,Board),_,X),States).
allNextStates(state(o,Board), States) :-findall(X,switchState(state(o,Board),_,X),States).

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

getHighCol(Col, List) :- getHighColH(Col, List, -1, -10000000,_).

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

getLowCol(Col, List) :- getLowColH(Col, List, -1, 10000000,_).


% bestNext bestimt zu einem state die beste nächste Col s.d. der dadurch
% erzeugte state optimal ist. allNextStates liefert hier alle möglichen
% spielzüge.
bestNext(Col, state(x, Board)) :- allNextStates(state(x, Board), Posible),
				  getMiniMax(R, Posible, 1),
				  getHighCol(HCol ,R),
                                  indexOf_single(Posible, Next, HCol),
                                  switchState(state(x, Board), Col, Next).

bestNext(Col, state(o, Board)) :- allNextStates(state(o, Board), Posible),
				  getMiniMax(R, Posible, 1),
				  getLowCol(HCol, R),
				  indexOf_single(Posible, Next, HCol),
                                  switchState(state(o, Board), Col, Next).


%----------------minimaxImpl------------------------
% minimax haben wir dadurch implementiert das wir den baum "rekursiv"
% nach unten aufbaun indem wir die höhe des baumes mitgeben und solange
% minimax auf alle möglichen states anwenden.
% ist die höhe 0 springt unsere heuristik ein.
miniMax( 1000000, state(x, Board), _) :- win_board(x, Board).
miniMax(-1000000, state(x, Board), _) :- win_board(o, Board).

miniMax(-1000000, state(o, Board), _) :- win_board(o, Board).
miniMax( 1000000, state(o, Board), _) :- win_board(x, Board).

miniMax(       0, state(X, Board), _) :- not(win_board(X, Board)),
				         draw_board(Board).

miniMax(      Val,          State, 0) :- heuristik(Val, State).

miniMax(      Val, state(x,Board), H) :- not(win_board(o, Board)),
					 not(draw_board(Board)),
					 H >0,
					 allNextStates(state(x,Board), Posible),
					 NewH is H-1,
					 getMiniMax(R, Posible, NewH),
					 getMax(Val, R).

miniMax(     Val, state(o,Board), H) :- not(win_board(x, Board)),
					not(draw_board(Board)),
					H>0,
					allNextStates(state(o,Board), Posible),
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


% gewinnReihe mit subList und length erhalten wir alle Listen die in X
% enthalten sind und länge Stones in Row hat.
gewinnReihe(Rueck, X) :- stonesInRow(Stones),
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
% danach wiederholt sich das bis StonesInRow grade die anzahl an steinen
% ist.
allListsH([], Anz, _, _ ):- stonesInRow(X),
			    Anz is X + 1.
allListsH([R|RS], Anz, Player, Win) :- stonesInRow(X),
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
heuristikX(Wert, state(x,Board)) :- getListOX(Rueck, x, Board),
				   allLists(Rueck2, x, Rueck),
				   gesamtPunkte(Wert, Rueck2).

heuristikO(Wert, state(o,Board)) :- getListOX(Rueck, o, Board),
				   allLists(Rueck2, o, Rueck),
				   gesamtPunkte(W, Rueck2),
				   Wert is 0 - W.

% die endgültige heuristik ergibt sich dadurch das das die werte für o
% und x zusammen gerechnet wird.
heuristik(Wert, state(_,Board)) :- heuristikX(W1,state(x,Board)),
	                           heuristikO(W2, state(o,Board)),
				   Wert is W1 + W2.
















