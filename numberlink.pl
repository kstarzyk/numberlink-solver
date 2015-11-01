/***********************************************\
	Krzysztof Starzyk 2014	
\************************************************

/*

	 
		
	Main:
		readData -  take data from input
		solve/4 :
			emptyMatrix/3	- create empty matrix with uninitialised variables
			startMatrix/2	- setting start matrix
			findSolution/3- returns soulution ( if solutions exists )
		findPath/6 			- returns path between pair of fields

	Pomocniczne :

		conn/6 			- check if move is possibly in any direction N\S\E\W
		node/4 			- check the value of field
		putNum/3 		- initialised variable 
		fillWithNum/3 		- fill matrix with values from list
		row/3 			- returns n-th row
		column/3		- returns m-th column

	Zmienne:
		(N,M) 			- cordinates of field, N - row, M - column 
		Nsize, Msize 		- size of matrix NsizexMsize
		Pairs 			- List of pair of points in matrix,  ( (N1,M1), (N2,M2) )
		Acc 			- accumulator

*/
%%%%%%%% 
/*
	How it works:
		Algorithm based on backtracking
		1. Initialazing matrix fullified with uninitialised variables.
		2. Putting start position on matrix ( given by list of pairs ).
		3. Finding way between pair of points i (i = 1,2,..|Pairs|).
			If path between pair of points ...: 
			3a. Exists			--> Step 3 && i++.
			3b. Exists && i == |Pairs| 	--> Step 4.
			3c. Doesn't exists 		--> Step 3 && i--.
			3d. Doesn't exists && i == 1 	--> Step 5. 
		4. Print the solution.
		5. Solution doesn't exist.

*/


readData( File, Solution ) :-
	open( File, read, Str ),
	read( Str, Nsize ),
	read( Str, Msize ),
	read( Str, Pairs ),
	close( Str ),
	solve( Nsize, Msize, Pairs, Solution ). 	

% solve ( +Nsize, +Msize, +Pairs, -Solution ) 
solve( Nsize, Msize, Pairs, Solution ) :-
	emptyMatrix( Nsize, Msize, Matrix ),
	startMatrix( Pairs, Matrix ),
	findSolution(Pairs, Matrix, Solution ).
	%print_matrix(Matrix).


% emptyMatrix( +Nsize, +Msize, -Matrix )
emptyMatrix( Nsize, Msize, Matrix ) :-
	emptyMatrix( Nsize, Msize, Matrix, [] ).
emptyMatrix( 0, _, Matrix, Matrix ) :- !.
emptyMatrix( Nsize, Msize, Matrix, 	Acc ) :-
	NextN is Nsize - 1,
	length( H, Msize ),
	emptyMatrix( NextN, Msize, Matrix, [H|Acc] ).

% startMatrix( +Pairs, ?Matrix )
startMatrix( Pairs, Matrix ) :-
	startMatrix( Pairs, Matrix, 1 ).
startMatrix( [E|Pairs], Matrix, Acc ) :-
	E = ( (N1, M1), (N2, M2) ),	
	row( Matrix, N1, Z ),
	nth1( M1, Z, Acc ),
	row( Matrix, N2, Y ),
	nth1( M2, Y, Acc ),
	NextAcc is Acc + 1,
	startMatrix( Pairs, Matrix, NextAcc ).

startMatrix( [], _Matrix, _ ).


%	findSolution (+Pairs, ?Matrix, -Solution, (Acc) )
findSolution( Pairs, Matrix, Routes ) :-
	findSolution( Pairs, Matrix, Routes, 1 ).

findSolution( [H|T], Matrix, [Route|Solution], Acc ) :- 
	H = ( (SN,SM), (DN,DM) ),
	findPath( SN, SM, DN, DM, Matrix, Route, Acc), 
	NextAcc is Acc + 1,
	length(Route,X), 
	X \== 0,
	fillWithNum( Matrix, Route, Acc ),
	findSolution( T, Matrix, Solution, NextAcc ).

findSolution( [], _Matrix,  [] , _ ) :- !.



% findPath( +N1, M1+, +N2, +M2, Matrix, [Start | Route ], +ACC )
findPath( N1, M1, N2, M2, Matrix, [ (N1, M1) | Route ], Acc ) :-
	findPath( N1, M1, N2, M2, Matrix,  Route, [(N2, M2)], Acc ).

findPath( N1, M1, N2, M2, Matrix, Route, Route, Acc ) :-
	(N1,M1) \== (N2,M2), 
	conn( N1, M1, N2, M2, Matrix, Acc ).

findPath( N1, M1, N2, M2, Matrix, Route, Visited, Acc ) :-
	(N1,M1) \== (N2,M2),	
	conn( CurrentN, CurrentM, N2, M2, Matrix, Acc),
	\+ member( (CurrentN, CurrentM), Visited ), 
	findPath(N1, M1, CurrentN, CurrentM, Matrix, Route, [(CurrentN, CurrentM)|Visited ], Acc ).  

% conn ( +N1, +M1, +N2, +M2 ) // istnienie ścieżki z (N1,M1) do (N2,M2). Odpowiednio N, S, E, W
conn( N1, M1, N2, M2, Matrix, Acc ) :- 
	node( Matrix, N2, M2, Acc ),
	N1 is N2, 
	M1 is M2 - 1.
conn( N1, M1, N2, M2, Matrix, Acc ) :-
	node( Matrix, N2, M2, Acc ),
	N1 is N2,
	M1 is M2 + 1.
conn( N1, M1, N2, M2, Matrix, Acc ) :-
	node( Matrix, N2, M2, Acc ),
	N1 is N2 + 1,
	M1 is M2.
conn( N1, M1, N2, M2, Matrix, Acc ) :-
	node( Matrix, N2, M2, Acc ),	
	N1 is N2 - 1,
	M1 is M2.

% node ( +Matrix, +N, +M, -VAL) // zwraca wartość pola (N,M)
node( Matrix, N, M, Val ) :-
	row( Matrix, N, Z ),
	nth1( M, Z, Val ).


% putNum( ?Matrix, +Point, +Num )
putNum( Matrix, (N, M), Num) :-
	row( Matrix, N, Z ),
	nth1( M, Z, X ),
	X is Num.

% fillWithNum ( ?Matrix, +Path, +Num )
fillWithNum( Matrix, [H|T], Num ) :-
	H = (_,_),
	putNum(Matrix, H, Num),
	fillWithNum( Matrix, T, Num ). 
fillWithNum( _Matrix, [], _ ).

	
% row( +Matrix, +N, -List )
row( [_|Rest], N, List ) :-
	N > 1,
	DecN is N - 1,
	row( Rest, DecN, List ).
row( [T|_], 1, T).

% column ( +Matrix, +M, -List )
column( [Row | Rest], M, [E|List]) :-
	nth1(M, Row, E),
	column( Rest, M,List ).  
column( [], _, [] ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
simple_test(7,7, [ ((6,3), (3,5)), ((7,1),(5,5)), ((2,2),(3,4)), ((1,4),(7,5)), ((4,4),(2,6))], [[ (6, 3), (6, 4), (6, 5), (6, 6), (5, 6), (4, 6), (3, 6), (3, 5)], [ (7, 1), (6, 1), (5, 1), (5, 2), (5, 3), (5, 4), (5, 5)], [ (2, 2), (3, 2), (3, 3), (3, 4)], [ (1, 4), (1, 5), (1, 6), (1, 7), (2, 7), (3, 7), (4, 7), (5, 7), (6, 7), (7, 7), (7, 6), (7, 5)], [ (4, 4), (4, 3), (4, 2), (4, 1), (3, 1), (2, 1), (1, 1), (1, 2), (1, 3), (2, 3), (2, 4), (2, 5), (2, 6)]]).

% 0 3 0 0 2 5 0 
% 0 0 0 3 1 0 0
% 0 0 0 5 0 0 0
% 0 0 0 0 0 0 0
% 0 0 1 0 0 0 0
% 2 0 0 0 4 0 0


simple_test(6,6, [ ((1,4),(1,6)), ((1,5), (5,5)), ((1,1), (2,2)), ((1,3),(2,1)),((3,2),(5,2)) ], [[ (1, 4), (2, 4), (3, 4), (4, 4), (5, 4), (6, 4), (6, 5), (6, 6), (5, 6), (4, 6), (3, 6), (2, 6), (1, 6)], [ (1, 5), (2, 5), (3, 5), (4, 5), (5, 5)], [ (1, 1), (1, 2), (2, 2)], [ (1, 3), (2, 3), (3, 3), (4, 3), (5, 3), (6, 3), (6, 2), (6, 1), (5, 1), (4, 1), (3, 1), (2, 1)], [ (3, 2), (4, 2), (5, 2)]]).

% 3 0 4 1 2 0	
% 4 3 0 0 0 0	
% 0 5 0 0 0 1	
% 0 0 0 0 0 0	
% 0 5 0 0 2 0	
% 0 0 0 0 0 0	

simple_test(4,6, [ ((1,1),(1,6)), ((1,2),(3,5)), ((2,2),(3,4)) ], [[ (1, 1), (2, 1), (3, 1), (4, 1), (4, 2), (4, 3), (4, 4), (4, 5), (4, 6), (3, 6), (2, 6), (1, 6)], [ (1, 2), (1, 3), (2, 3), (2, 4), (2, 5), (3, 5)], [ (2, 2), (3, 2), (3, 3), (3, 4)] ]).

% 1 2 0 0 0 1
% 0 3 0 0 0 0 
% 0 0 0 3 2 0
% 0 0 0 0 0 0 

count_test(5,4,[ ((1,1),(1,5)), ((1,3), (2,4)), ((2,1),(2,3)), ((4,3),(4,5))], 0 ).


%1 0 2 0 1
%3 0 3 2 0 
%0 0 0 0 0
%0 0 4 0 4

count_test(4,6, [ ((1,1),(1,6)), ((1,2),(3,5)), ((2,2),(3,4)) ], 13).

% 1 2 0 0 0 1
% 0 3 0 0 0 0 
% 0 0 0 3 2 0
% 0 0 0 0 0 0 


count_test(6,6, [ ((1,4),(1,6)), ((1,5), (5,5)), ((1,1), (2,2)), ((1,3),(2,1)),((3,2),(5,2)) ], 1).

% 3 0 4 1 2 0	
% 4 3 0 0 0 0	
% 0 5 0 0 0 1	
% 0 0 0 0 0 0	
% 0 5 0 0 2 0	
% 0 0 0 0 0 0	

