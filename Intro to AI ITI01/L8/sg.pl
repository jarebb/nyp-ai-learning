
colorList([red,green]).

states([WC, PN, JR, YH, BB, RM, TP, HBT, JB, BTP, MBT, PP, MPS, MP, HG, AJI, TPN, PRP, EC, CCK, MYT, SBW, HKN, BPJ, NS, AMK, KB, YCK, PW, MM, SK]).


adjacent(WC,[PN, CCK, JR, HKN, HBT, TP, RM]).
%adjacent(PN,[WC]).
adjacent(JR,[WC, YH, BB, HBT, CCK, HKN]).
adjacent(YH,[JR, BB, HKN]).
%adjacent(BB,[JR, YH, HKN, CCK]).
%adjacent(RM,[WC, TP]).
adjacent(TP,[RM, HBK, BTP, JB]).
adjacent(HBT,[TP, WC, JR, CCK, MYT, NS, KB, BTP, BPJ]).
adjacent(JB,[TP, BTP, PP, MPS, MP, MBT]).
adjacent(BTP,[TP, HBT, KB, AMK, MM, AJI, MP, PP, JB]).
adjacent(MBT,[JB, MP]).
adjacent(PP,[JB, BTP, AJI, MP, MPS]).
adjacent(MPS,[JB, PP, MP]).
adjacent(MP,[MBT, MPS, PP, AJI, EC]).
adjacent(HG,[AJI, AMK]).
adjacent(AJI,[HG, MP, BTP, AMK, SK, PRP, TPN, EC]).
adjacent(TPN,[AJI, EC, PRP]).
adjacent(PRP,[EC, TPN, AJI, SK, PW]).
adjacent(EC,[MP, AJI, TPN, PRP]).
adjacent(CCK,[WC, HKN, BB, JR, HBT, MYT]).
adjacent(MYT,[CCK, HBT, SBW]).
adjacent(SBW,[MYT, NS]).
adjacent(HKN,[CCK, WC, JR, BB]).
adjacent(BPJ,[HBT]).
adjacent(NS,[YCK, KB, HBT, MYT, SBW]).
adjacent(AMK,[YCK, MM, AJI, HG, SK]).
adjacent(KB,[YCK, AMK, BTP, HBT, NS]).
adjacent(YCK,[AMK, KB, NS]).
adjacent(PW,[PRP, SK]).
adjacent(MM,[AMK, BTP]).
adjacent(SK,[AMK, AJI, PRP, PW]).


/* Helper predicates. */

head([],[]).
head([A|B], A).

tail([],[]).
tail([_|B], B).

concatenate(X,Y,Z) :- cons(X,Y,Z),!.

cons([],[],[]).
cons([X],[],[X]).
cons([],[X],[X]).
cons(X,[],[X]).
cons([],X,[X]).
cons(X,[H|T],[X,H|T]):-!.
cons(X,Y,[X,Y]).

% ------- Map coloring predicates ----------

%pickColor and enumerateColor recurse throuh the color list and provide next available color, if any.

enumerateColor([],_) :-
	!,
	fail.

enumerateColor([AvailColor|_], AvailColor).

enumerateColor([_|Rest], AvailColor) :-
	enumerateColor(Rest, AvailColor).	

pickColor(AvailColor) :-
		colorList(ColorList),
		enumerateColor(ColorList, AvailColor).

% setColor create state,color pair.

setColor(X,Y, [X,Y]).

% pickAdjacentStateColor collect all colors painted in adjacent states, if any.

pickAdjacentStateColor([],_,SC).

pickAdjacentStateColor([X|AS], [[State|Color] | T], SC) :- 
	member([X|C1],[[State|Color]|T]), 
	concatenate(C1, Temp, SC1), 
	append(C1, Temp, SC1),
	pickAdjacentStateColor(AS,[[State|Color] | T],SC2),
	!,
	concatenate(SC1, SC2, SC3),flatten(SC3,SC).

pickAdjacentStateColor([H|AT], CurrentColor, SC) :- 
	pickAdjacentStateColor(AT, CurrentColor, SC).

% getAvailColor interate through the neighbouring state colors, if any, and pick the first available color which is not used yet

getAvailColor([],_,_) :- 
	!,
	fail.

getAvailColor(AvailableColorList, CurrentColorList, Color) :- 
	permutation(AvailableColorList, CurrentColorList),
	!,
	fail.

getAvailColor(AvailableColorList, CurrentColorList, Color) :- 
	subtract(AvailableColorList, CurrentColorList, [Color|_]). 

getAvailColor(AvailableColorList, CurrentColorList, Color) :- 
	subtract(AvailableColorList, CurrentColorList, [_|RemainingList]),
	getAvailColor(RemainingList, CurrentColorList, Color).
 
% get color picks next available color.

getColor([],X):-
	!,
	pickColor(X).

getColor(CL,C):-
	colorList(X),
	getAvailColor(X, CL, C).

% painttTheStates picks available color for a state and update the state,color pair in the current list of painted states.

paintTheStates(State, ListofPaintedStateAndColorPairs, UpdatedStateColorPairs) :- 
	adjacent(State, AdjacentStateList), 
	pickAdjacentStateColor(AdjacentStateList, ListofPaintedStateAndColorPairs, UsedColor), 
	getColor(UsedColor,AvailableColor), 
	setColor(State,AvailableColor,NewStateColorPair), 
	concatenate(NewStateColorPair, ListofPaintedStateAndColorPairs, UpdatedStateColorPairs).

% paintAll recurse through the entire statelist.

paintAll([], R1, Result):- =(R1,Result).

paintAll([H|T], ListofPaintedStateAndColorPairs, Result):- 
	paintTheStates(H, ListofPaintedStateAndColorPairs, UpdatedListofStateAndColorPairs), 
	paintAll(T, UpdatedListofStateAndColorPairs, Result).

% paint initial the first state and color pair for paintAll to carry out the rest.

paint([H|T], Color, Result) :- 
	setColor(H,Color,StateAndColorPair), 
	paintAll(T, [StateAndColorPair], Result).

% paintmap find all possible solutions for given states with all colors 
 
paintmap(Result) :- 
	states([H|T]), 
	pickColor(AColor),
	paint([H|T], AColor, Result).