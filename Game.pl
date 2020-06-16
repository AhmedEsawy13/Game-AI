nextplayer(player,computer).
nextplayer(computer,player).
beginningBoard([1,-1,0,1,0,1,0,1,-1]).
indexedBoard([[0,0],[0,1],[0,2],[1,0],[1,1],[1,2],[2,0],[2,1],[2,2]]).

%print answer on screen

printt1(_,10):-
nl,!.
printt1([H|T],R):-
0 is mod(R,3),
write(' || '),
write(H),
nl,
R1 is R+1,
printt1(T,R1).

printt1([H|T],R):-
write(' || '),
write(H),
R1 is R+1,
printt1(T,R1).


%moves player left,right,up,down
moveplayerto([Row,Coloumn],[Row,Coloumn],[Row,NewColoumn]):-
    NewColoumn is Coloumn-1,
    NewColoumn<3,
    NewColoumn>=0.

moveplayerto([Row,Coloumn],[Row,Coloumn],[Row,NewColoumn]):-
    NewColoumn is Coloumn+1,
    NewColoumn<3,
    NewColoumn>=0.

moveplayerto([Row,Coloumn],[Row,Coloumn],[NewRow,Coloumn]):-
    NewRow is Row-1,
      NewRow<3,
    NewRow>=0.

moveplayerto([Row,Coloumn],[Row,Coloumn],[NewRow,Coloumn]):-
    NewRow is Row+1,
       NewRow<3,
    NewRow>=0.


changeIndexboardtoBox([[X,_]],[H1]):-
   H1 is X.

  changeIndexboardtoBox([[X,_]|T],[H1|T1]):-
   H1 is X,
    changeIndexboardtoBox(T,T1).


%computer move and put number in cell from range [-1,1]
putRandomNumber([Row,Coloumn],Box,NBox):-
    indexedBoard(K1),
    createindexedboard(K1,Box,List),
    findelement(List,[Row,Coloumn],V1),
    random_between(-1,1,R),
    select([V1,[Row,Coloumn]],List,[R,[Row,Coloumn]],List2),
    changeIndexboardtoBox(List2,NBox)
  .

%the main operation of sum and make new board
gettingTarget([Row,Coloumn],[NewRow,NewColoumn],Box,NewValue):-

indexedBoard(K1),
createindexedboard(K1,Box,List),
findelement(List,[Row,Coloumn],V1),
findelement(List,[NewRow,NewColoumn],V2),
V is V1 + V2,
    NewValue is V.


%get new state win ,lose,draw
% main function that call function that creates new board either player
% or computer
%
gettingmoves([player,[R,C],_,Box,RestMove],[Player,[NewRow,NewColoumn],Nopos,NBox,NRestMove]):-
    RestMove>0,
    nextplayer(player,Player),
    moveplayerto([R,C],Nopos,[NewRow,NewColoumn]),
       gettingTarget([R,C],[NewRow,NewColoumn],Box,NewTarget),
       indexedBoard(K1),
       createindexedboard(K1,Box,List),
       findelement(List,[NewRow,NewColoumn],V1),
       select([V1,[NewRow,NewColoumn]],List,[NewTarget,[NewRow,NewColoumn]],List2),

       changeIndexboardtoBox(List2,NBox),

    NRestMove is RestMove-1.

%computer
gettingmoves([computer,Cpos,Opos,Box,RestMove],[Player,Cpos,Opos,NBox,NRestMove]):-
    RestMove>0,
    nextplayer(computer,Player),
   putRandomNumber(Opos,Box,NBox),
       NRestMove is RestMove-1.



%alpha beta algorithm
min_to_move([computer, _,_,_,_]).
max_to_move([player,_,_,_,_]).

utility(G,[_, Ncpos,_,B,_], 1):-
indexedBoard(K1),

     createindexedboard(K1,B,List),

     findelement(List,Ncpos,Val),
    % M is 0,
     Val > G.

utility(G,[_, Ncpos,_,B,_], -1):-
indexedBoard(K1),

     createindexedboard(K1,B,List),

     findelement(List,Ncpos,Val),
    % M is 0,
         G > Val.

utility(G,[_,Ncpos,_,B,_], 0):-
indexedBoard(K1),

     createindexedboard(K1,B,List),

     findelement(List,Ncpos,Val),
    % M is 0,
     Val is G.

alphabeta(Pos, BestNextPos, Val,G) :-
    bagof(NextPos, gettingmoves(Pos, NextPos), NextPosList),
    best(NextPosList, BestNextPos, Val,G).

    alphabeta(Pos, _, Val,G) :-
    utility(G,Pos, Val).

best([Pos], Pos, Val,G) :-
    alphabeta(Pos, _, Val,G), !.
best([Pos1 | PosList], BestPos, BestVal,G) :-
    alphabeta(Pos1, _, Val1,G),
    best(PosList, Pos2, Val2,G),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-
    min_to_move(Pos0),
    Val0 > Val1, !
    ;
    max_to_move(Pos0),
    Val0 < Val1, !.
betterOf(_, _, Pos1, Val1, Pos1, Val1).


%make board according to player choice not computer
realplayermove(Direction,[Player,[R,C],_,RestMoves,Box],[X,[NewRow,NewColoumn],Nopos,NewRestMoves,NBox]):-
   nextplayer(Player,X),
   realMove(Direction,[R,C],[NewRow,NewColoumn],Nopos),
        NewRestMoves is RestMoves-1,
       gettingTarget([R,C],[NewRow,NewColoumn],Box,NewTarget),
       indexedBoard(K1),
       createindexedboard(K1,Box,List),
       findelement(List,[NewRow,NewColoumn],V1),
       select([V1,[NewRow,NewColoumn]],List,[NewTarget,[NewRow,NewColoumn]],List2),
       changeIndexboardtoBox(List2,NBox).


bestMove(Pos, NextPos,Goal):-
	alphabeta(Pos,NextPos,_,Goal).

%player choice move left
realMove(left,[Row,Coloumn],[Row,NewColoumn],[Row,Coloumn]):-
        NewColoumn is Coloumn-1.

%player choice move right.

realMove(right,[Row,Coloumn],[Row,NewColoumn],[Row,Coloumn]):-
        NewColoumn is Coloumn+1.
%player choice move down

realMove(down,[Row,Coloumn],[NewRow,Coloumn],[Row,Coloumn]):-
        NewRow is Row+1.

%player choice move up

realMove(up,[Row,Coloumn],[NewRow,Coloumn],[Row,Coloumn]):-
        NewRow is Row-1.


%get input from user
getinputfromuser(Moves,Goal,B):-
  write('Enter Maximum moves & MiniGoal Please ?'), nl,
  write('First Enter Maximum Moves : '),nl,
  read(Moves), nl,

  write('Second Enter MiniGoal : '),nl,
  read(Goal), nl,
  nl,
  beginningBoard(B),
  printt1(B,1), nl.


%start of the game
start() :-
 getinputfromuser(Moves,Goal,B),
 realgame([player,[2,0],null,Moves,B],Goal).


% function that decide who play computer or player and call it is
% functions and check if any of them win or draw
realgame([Player,PointerPos,Opos,Ms,B],Goal) :-

(Player=player->nl,
 write('Next move ?'),
 nl,
 read(Direction),
 nl,


  realMove(Direction,PointerPos,[Row,Coloumn],[NewOldR,NewOldC]),
  (

   checkNewPointerPos([3,3],[Row,Coloumn]) ;  write('Try Again'),

			  nl, realgame([player,PointerPos,_,Ms,B],Goal)
 ),
  realplayermove(Direction,[Player,PointerPos,[NewOldR,NewOldC],Ms,B],[OtherPlayer,Ncpos,Nopos,NM,NBox]),

      printt1(NBox,1),

      realgame([OtherPlayer,Ncpos,Nopos,NM,NBox],Goal)

;
      nl, write('Computer play : '), nl, nl,
      bestMove([computer,PointerPos,Opos,B,Ms],[OtherPlayer,Ncpos,Nopos,NBox,NM],Goal),
      printt1(NBox,1),

    (
          not(NM is 0),  realgame([OtherPlayer,Ncpos,Nopos,NM,NBox],Goal)

          ;

     indexedBoard(K1),

     createindexedboard(K1,NBox,List),

     findelement(List,Ncpos,Val),

      nl, write('Game Ends : '),

           indexedBoard(K1),

          createindexedboard(K1,NBox,List),

    findelement(List,Ncpos,Val),


(
    not(NM is 0) ,  realgame([OtherPlayer,Ncpos,Nopos,NM,NBox],Goal)

    ;

    Val > Goal ,write(' Player win !') ; Val is Goal, write(' No One Win! '), nl, nl,!;write(' Computer win !'),nl, nl,!

 )


      )).


% get list of lists and each element and each element contain value and it
% is position
createindexedboard([H],[H1],[HR]):-
HR = [H1,H].

createindexedboard([H|T],[H1|T1],[HR|TR]):-
HR = [H1,H],
createindexedboard(T,T1,TR).


% get  element from  position in the indexedboard
findelement([],_,-1).

findelement([[V1,[R1,C1]]|T],[R,C],Re):-
(
R1 is R,
C1 is C ,
Re is V1
    ;
    findelement(T,[R,C],Re)).

%check boundary of game
checkNewPointerPos([RR,RC],[NR,NC]):-
    NR>=0,
    NR<RR,
    NC>=0,
    NC<RC.
