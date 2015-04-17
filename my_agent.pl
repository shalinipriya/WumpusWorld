%my_agent.pl

%   this procedure requires the external definition of two procedures:
%
%     init_agent: called after new world is initialized.  should perform
%                 any needed agent initialization.
%
%     run_agent(percept,action): given the current percept, this procedure
%                 should return an appropriate action, which is then
%                 executed.
%
% This is what should be fleshed out

:- dynamic safeCell/1, current_state/1, agent_pit/1, agent_wumpus/1, location/2, b/2, s/2, cell/2, direction/2, visited/1, shortest/4. 
:- dynamic breezy/1, stinks/1, glitter/1, bump/2.
:- dynamic maybePit/1, maybeWumpus/1, hasgold/1, no_pit/1, no_wumpus/1, hasarrow/0, wumpusdead/0.
:- dynamic actionToTake/2, hasgold/0, limitX/1,limitY/1.

init_agent:-
  format('\n=====================================================\n'),
  format('This is init_agent:\n\tIt gets called once, use it for your initialization\n\n'),
  format('=====================================================\n\n'),
  retractall(current_state(_)),
  assert(current_state([])),
  add_to_current_state(safeCell(cell(1,1))),
  add_to_current_state(visited(cell(1,1))),
  assert(hasarrow),
  assert(limitX(100)),
  assert(limitY(100)),
  retractall(location(_,_)),
  assert(location(cell(1,1),1)),
  retractall(direction(_,_)),
  assert(direction(0,1)),
  retractall(current_time(_)),
  assert(current_time(1)),
  current_state(L),
  assert_list1(L).


run_agent([Stench,Breeze,Glitter,Bump,Scream], Action) :-
	hasgold,
	current_time(T),
	direction(D,T),
	location(cell(X,Y),T),
	actionToTake(Action,T),
	update(Action,T),
	display_world.

run_agent([Stench,Breeze,Glitter,Bump,Scream], Action) :-
	current_time(T),
	direction(D,T),
	location(cell(X,Y),T),
	actionToTake(Action,T),
	update(Action,T),
	display_world.	

run_agent([Stench,Breeze,Glitter,Bump,Scream], Action) :-
	current_time(T),
	direction(D,T),
	location(cell(X,Y),T),
	%Newtime is T+1,
	%assert(current_time(Newtime)),
	assert(stinks(Stench, T)),
	s(Stench,T),
	assert(breeze(Breeze, T)),
	b(Breeze,T),
	assert(glitter(Glitter, T)),
	assert(bump(Bump, T)),
	bump(Bump,T),
	assert(scream(Scream, T)),
	scream1(Scream,T),	
	best_action(Action, T),
	display_world.


update(turnleft,T) :-
	T1 is T+1,
	location(cell(X,Y),T),
	direction(D,T),
	D2 is (D+90),
	D1 is mod(D2,360),
	retract(current_time(_)),
	assert(current_time(T1)),
	assert(location(cell(X,Y),T1)),
	assert(direction(D1,T1)).		
	

update(turnright,T) :-
	T1 is T+1,
	location(cell(X,Y),T),
	direction(D,T),
	D2 is (D-90),
	D1 is mod(D2,360),
	retract(current_time(_)),
	assert(current_time(T1)),
	assert(location(cell(X,Y),T1)),
	assert(direction(D1,T1)).		

update(goforward,T) :-
	T1 is T+1,
	location(cell(X,Y),T),
	direction(D,T),
	retract(current_time(_)),
	assert(current_time(T1)),
	new_location(X,Y,D,X1,Y1),
	assert(location(cell(X1,Y1),T1)),
	assert(direction(D,T1)),
	assert(visited(cell(X1,Y1))).		


safeCell(cell(X,Y)) :-
	no_pit(cell(X,Y)),
	(no_wumpus(cell(X,Y));wumpusdead),
	cell_exists(cell(X,Y)).
	
		
		
actionplan(cell(X,Y),D,(cell(X,Y)),[]) :-!.


actionplan(cell(X,Y),D,cell(X1,Y1), ActionPlanList) :-
	actionPlanCells(cell(X,Y),cell(X1,Y1),ActionCellList1),
	%append([cell(X,Y)],ActionCellList,ActionCellList1),
	actionPlanList(ActionCellList1,ActionPlanList, D).

actionPlanList([cell(X,Y)],[],D).
	
actionPlanList([cell(X,Y),cell(X1,Y1)|L],List2,D) :-
	actionList(cell(X,Y),cell(X1,Y1),D,D2,List1),
	actionPlanList([cell(X1,Y1)|L],List,D2),
	append(List1,List,List2).
		

actionList(cell(X,Y),cell(X,Y),D,D,[]).

%,0,above/left	
actionList(cell(X,Y),cell(X1,Y1),0,D1,[turnleft|L]) :-
	((X1=X,
	Y1 =:= Y+1);
	(X1=:=X-1,
	Y1=Y)),
	actionList(cell(X,Y),cell(X1,Y1),90,D1,L).
%0,below
actionList(cell(X,Y),cell(X1,Y1),0,D1,[turnright|L]) :-
	X=X1,
	Y1=:=Y-1,
	actionList(cell(X,Y),cell(X1,Y1),270,D1,L).
%0,right
actionList(cell(X,Y),cell(X1,Y1),0,D1,[goforward|L]):-
	X1=:=X+1,
	Y1=Y,
	actionList(cell(X1,Y1),cell(X1,Y1),0,D1,L).	


%90,below
actionList(cell(X,Y),cell(X1,Y1),90,D1,[turnright|L]) :-
	X=X1,
	Y1=:=Y-1,
	actionList(cell(X,Y),cell(X1,Y1),0,D1,L).
%left	
actionList(cell(X,Y),cell(X1,Y1),90,D1,[turnleft|L]) :-
	X1 =:= X-1,
	Y1 = Y,
	actionList(cell(X,Y),cell(X1,Y1),180,D1,L).
%right
actionList(cell(X,Y),cell(X1,Y1),90,D1,[turnright|L]) :-
	X1 =:= X+1,
	Y1 = Y,
	actionList(cell(X,Y),cell(X1,Y1),0,D1,L).
%above
actionList(cell(X,Y),cell(X1,Y1),90,D1,[goforward|L]):-
	X1=X,
	Y1=:=Y+1,
	actionList(cell(X1,Y1),cell(X1,Y1),90,D1,L).
%180
%below	
actionList(cell(X,Y),cell(X1,Y1),180,D1,[turnleft|L]) :-
	X1=X,
	Y1=:=Y-1,
	actionList(cell(X,Y),cell(X1,Y1),270,D1,L).

%180, turn right 1,1 at 180 to 1,2
%above
actionList(cell(X,Y),cell(X1,Y1),180,D1,[turnright|L]) :-
	X1=X,
	Y1=:=Y+1,
	actionList(cell(X,Y),cell(X1,Y1),90,D1,L).

%left
actionList(cell(X,Y),cell(X1,Y1),180,D1,[goforward|L]):-
	X1=:=X-1,
	Y1=Y,
	actionList(cell(X1,Y1),cell(X1,Y1),180,D1,L).
	
%left
actionList(cell(X,Y),cell(X1,Y1),180,D1,[turnright|L]):-
	X1=:=X+1,
	Y1=Y,
	actionList(cell(X1,Y1),cell(X1,Y1),180,D1,L).

%270
%left
actionList(cell(X,Y),cell(X1,Y1),270,D1,[turnright|L]) :-
	X1=:=X-1,
	Y1 = Y,
	actionList(cell(X,Y),cell(X1,Y1),180,D1,L).
%right
actionList(cell(X,Y),cell(X1,Y1),270,D1,[turnright|L]) :-
	X1=:=X+1,
	Y1 = Y,
	actionList(cell(X,Y),cell(X1,Y1),180,D1,L).

%above	
actionList(cell(X,Y),cell(X1,Y1),270,D1,[turnright|L]) :-
	X1=X,
	Y1 =:= Y+1,
	actionList(cell(X,Y),cell(X1,Y1),180,D1,L).
%below	
actionList(cell(X,Y),cell(X1,Y1),270,D1,[goforward|L]):-
	X1=X,
	Y1=:=Y-1,
	actionList(cell(X1,Y1),cell(X1,Y1),270,D1,L).			


actionPlanCells(cell(X,Y),cell(X1,Y1), Path) :-
	reachable_cells(cell(X,Y),C),
	shortest(cell(X,Y),cell(X1,Y1),C,Visited),
	member(p(cell(X,Y),cell(X1,Y1),Dist,Path1),Visited),
	reverse(Path1,Path).
	%append([cell(X,Y)],Path,Path1),!.


reversePathList([],[]).
reversePathList([H1|P1],[H|P]) :-
	reverse(H1,H),
	reversePathList(P1,P).
	
shortest(A,B,[],Visited):-!.
shortest(A,B,V,Visited) :-
	leastDist(V,L),
	findall(N,neighbors(L,[N],V), Neighbor),
	update_cell(Neighbor,L,Updated),
	select(L,V,Vertex2),
	changeList(Updated, Vertex2, Vertex),
	shortest(A,B,Vertex,Visited2),
	append([L],Visited2,Visited).
	
changeList([],[],[]).
changeList([],V,V).
changeList([p(S1,V1,D1,P1)|Updated], [p(S1,V2,D2,P2)|Vertex1], [p(S1,V1,D1,P1)|Vertex2]) :-
	V1==V2,
	changeList(Updated, Vertex1, Vertex2).
changeList([p(S1,V1,D1,P1)|Updated], [p(S1,V2,D2,P2)|Vertex1], [p(S1,V2,D2,P2)|Vertex2]) :-
	V1\==V2,
	changeList([p(S1,V1,D1,P1)|Updated],Vertex1, Vertex2).
	
	


update_cell([],p(_,_,_,_),[]).

update_cell([p(S1,V1,D1,P1)|T1],p(S1,V2,D2,P2),[p(S1,V1,D1,P1)|T2]) :-
	adj(V2,V1),
	D1 =< 1+D2,
	update_cell(T1,p(S1,V2,D2,P2),T2).
update_cell([p(S1,V1,D1,P1)|T1],p(S1,V2, D2, P2),[p(S1,V1,D3,P3)|T2]) :-
	adj(V2,V1),
	D1 > 1+D2,	
	D3 is D2+1,
	append([V1],P2,P3),
	update_cell(T1,p(S1,V2,D2,P2),T2).
	
	
	
neighbors(p(S1,V1,D1,P1),[p(S1,V2,D2,P2)], [p(S1,V2,D2,P2)|V]) :- 
	adj(V1,V2).
neighbors(p(S1,V1,D1,P1),[p(S1,V2,D2,P2)], [p(S1,V3,D3,P3)|V]) :-
	neighbors(p(S1,V1,D1,P1),[p(S1,V2,D2,P2)], V).
neighbors(p(S1,V1,D1,P1),[], []).
 	



leastDist([Min],Min).
leastDist([H,K|T],L) :-
	lesser(H,K),
	leastDist([H|T],L).
leastDist([H,K|T],L) :-
	greater(H,K),
	leastDist([K|T],L).	

lesser(p(_,_,X,_),p(_,_,Y,_)) :-
	X =< Y.

greater(p(_,_,X,_),p(_,_,Y,_)) :-
	X > Y.
	
		
nodeDist(A,A,0).
nodeDist(A,B,10000):-
	safeCell(B),
	A\==B.

reachable_cells(cell(X,Y),Cells) :-
	%setof(cell(X1,Y1), (is_path(cell(X,Y),cell(X1,Y1),[cell(X,Y)])), Cells).
	setof(p(cell(X,Y),A,D,[A]), (nodeDist(cell(X,Y),A,D)), Cells).
	
in_list(cell(X,Y),[cell(X,Y)|L]).
	
in_list(cell(X,Y),[cell(X1,Y1)|L]) :-
	in_list(cell(X,Y),L).


search_safe(cell(X,Y), cell(X1,Y1)) :-
	setof(A,(safeCell(A),not(visited(A))),SafeSet),
	findmin(cell(X,Y),SafeSet, cell(X1,Y1)).

distance(cell(X,Y),cell(X1,Y1), D) :-
	DX is abs(X-X1),
	DY is abs(Y-Y1),
	D is DX+DY.



findmin(T,[F],F).

findmin(T,[F,S|R],M) :-
	distance(T,F,Df),
	distance(T,S,Ds),
	Df =< Ds,
	findmin(T,[F|R],M).
	 	
findmin(T,[F,S|R],M) :-
	distance(T,F,Df),
	distance(T,S,Ds),
	Df > Ds,
	findmin(T,[S|R],M).
	
	

adj(cell(X,Y), cell(X1,Y1)) :-
	X = X1,
	Y2 is Y+1,
	Y3 is Y-1,
	(Y1 = Y2;
	Y1 = Y3).

adj(cell(X,Y), cell(X1,Y1)) :-
	Y = Y1,
	X2 is X+1,
	X3 is X-1,
	(X1 = X2;
	X1 = X3).	

best_action(climb,T) :-
	hasgold,
	location(cell(X,Y),T).

best_action(grab, T) :-
	glitter(yes,T),
	location(cell(X,Y),T),
	direction(D,T),
	assert(hasgold),
	Newtime is T+1,
	actionplan(cell(X,Y),D,cell(1,1), ActionPlanList),
	action_to_take(ActionPlanList, Newtime),
	retract(current_time(_)),
	assert(current_time(Newtime)),
	assert(direction(D,Newtime)),
	assert(location(cell(X,Y),Newtime)).
		
best_action(goforward,T) :-
	location(cell(X,Y),T),
	direction(D,T),
	new_location(X,Y,D,X1,Y1),
	cell_exists(cell(X1,Y1)),
	safeCell(cell(X1,Y1)),
	Newtime is T+1,
	retract(current_time(_)),
	assert(current_time(Newtime)),
	assert(direction(D,Newtime)),
	assert(location(cell(X1,Y1),Newtime)),
	assert(visited(cell(X1,Y1))).
		
best_action(Action,T) :-
	location(cell(X,Y),T),
	direction(D,T),
	new_location(X,Y,D,X1,Y1),
	not(safeCell(cell(X1,Y1))),
	%some more conditions
	search_safe(cell(X,Y), cell(X2,Y2)),		
	actionplan(cell(X,Y),D,cell(X2,Y2), ActionPlanList),
	first_elem(Action,ActionPlanList),
	action_to_take(ActionPlanList, T),
	direction(D,Action,D1),
	Newtime is T+1,
	retract(current_time(_)),
	assert(current_time(Newtime)),
	assert(direction(D1,Newtime)),
	assert(location(cell(X,Y),Newtime)).

best_action(shoot,T) :-
	location(cell(X,Y),T),
	direction(D,T),
	stinky(cell(X,Y)),
	hasarrow,
	retract(hasarrow),
	new_location(X,Y,D,X1,Y1),
	assert(no_wumpus(cell(X1,Y1))).
	
best_action(Action,T) :-
	location(cell(X,Y),T),
	direction(D,T),
	actionplan(cell(X,Y),D,cell(1,1), ActionPlanList),
	action_to_take(ActionPlanList, T),
	first_elem(Action,ActionPlanList),
	direction(D,Action,D1),
	newLoc(Action,X,Y,D,X1,Y1),
	Newtime is T+1,
	retract(current_time(_)),
	assert(current_time(Newtime)),
	assert(direction(D1,Newtime)),
	assert(location(cell(X,Y),Newtime)).
	
best_action(climb,T) :-
	location(cell(1,1),T).	
	
newLoc(goforward,X,Y,D,X1,Y1) :-
	new_location(X,Y,D,X1,Y1).

newLoc(turnleft,X,Y,D,X,Y).
newLoc(turnright,X,Y,D,X,Y).	
	
	
	

direction(D,turnleft,D1) :-
	D1 is D+90.
	
direction(D,turnright,D1) :-
	D1 is D-90.
	
direction(D,goforward,D).
	
first_elem(H,[H|A]).	

action_to_take([],T) :-
	location(cell(1,1),T),
	hasgold,
	assert(actionToTake(climb,T)).
	
	
action_to_take([],T).


action_to_take([A|ActionList],T) :-
	assert(actionToTake(A,T)),
	T1 is T+1,
	action_to_take(ActionList,T1).
	
			
	
add_to_current_state(Fact):-
	retract(current_state(L)),
	assert(current_state([Fact|L])).

	
b(no,T) :-
	location(cell(X,Y),T),
	X1 is X + 1,
  	X0 is X - 1,
  	Y1 is Y + 1,
  	Y0 is Y - 1,
	assert(no_pit(cell(X,Y0))),
	assert(no_pit(cell(X,Y1))),
	assert(no_pit(cell(X0,Y))),
	assert(no_pit(cell(X1,Y))).

b(yes,T) :-
	location(cell(X,Y),T),
	X1 is X + 1,
  	X0 is X - 1,
  	Y1 is Y + 1,
  	Y0 is Y - 1,
	assert(maybePit(cell(X,Y0))),
	assert(maybePit(cell(X,Y1))),
	assert(maybePit(cell(X0,Y))),
	assert(maybePit(cell(X1,Y))).	


s(yes,T) :-
	location(cell(X,Y),T),
	X1 is X + 1,
  	X0 is X - 1,
  	Y1 is Y + 1,
  	Y0 is Y - 1,
	assert(maybeWumpus(cell(X,Y0))),
	assert(maybeWumpus(cell(X,Y1))),
	assert(maybeWumpus(cell(X0,Y))),
	assert(maybeWumpus(cell(X1,Y))).
	
s(no, T) :-
	location(cell(X,Y),T),
	X1 is X + 1,
  	X0 is X - 1,
  	Y1 is Y + 1,
  	Y0 is Y - 1,
	assert(no_wumpus(cell(X,Y0))),
	assert(no_wumpus(cell(X,Y1))),
	assert(no_wumpus(cell(X0,Y))),
	assert(no_wumpus(cell(X1,Y))).

bump(yes,T) :-
	location(cell(X,Y),T),
	direction(D,T),
	boundary(X,Y,D,T).

bump(no,T).

scream1(yes,T) :-
	assert(wumpusdead).

scream1(no,T).	
	

boundary(X,Y,0,T) :-
	retract(limitX(_)),
	assert(limitX(X)),
	location(cell(X,Y),T),
	retract(location(cell(X,Y),T)),
	X1 is X-1,
	assert(location(cell(X1,Y),T)).		
	
boundary(X,Y,90,T) :-
	retract(limitY(_)),
	assert(limitY(Y)),
	location(cell(X,Y),T),
	retract(location(cell(X,Y),T)),
	Y1 is Y-1,
	assert(location(cell(X,Y1),T)).		
			
	
cell_exists(cell(X,Y)) :-
	X > 0,
	Y > 0,
	limitX(X1),
	limitY(Y1),
	X < X1,
	Y < Y1.	
	

assert_list1([]).

assert_list1([Fact|Facts]) :-
  assert(Fact),
  assert_list1(Facts).
  
  % new_location(X,Y,Orientation,X1,Y1): returns new coordinates X1,Y1
%   after moving from X,Y along Orientation: 0, 90, 180, 270 degrees.

new_location(X,Y,0,X1,Y) :-
  X1 is X + 1.

new_location(X,Y,90,X,Y1) :-
  Y1 is Y + 1.

new_location(X,Y,180,X1,Y) :-
  X1 is X - 1.

new_location(X,Y,270,X,Y1) :-
  Y1 is Y - 1.
	
breezy(cell(X,Y)) :-
	location(cell(X,Y),T),
	breeze(yes,T),
	X1 is X + 1,
  	X0 is X - 1,
  	Y1 is Y + 1,
  	Y0 is Y - 1,
	assert(maybePit(cell(X,Y0))),
	assert(maybePit(cell(X,Y1))),
	assert(maybePit(cell(X0,Y))),
	assert(maybePit(cell(X1,Y))).	
  
  

stinky(cell(X,Y)) :-
	location(cell(X,Y),T),
	stinks(yes, T),
	X1 is X + 1,
  	X0 is X - 1,
  	Y1 is Y + 1,
  	Y0 is Y - 1,
	assert(maybeWumpus(cell(X,Y0))),
	assert(maybeWumpus(cell(X,Y1))),
	assert(maybeWumpus(cell(X0,Y))),
	assert(maybeWumpus(cell(X1,Y))).	
  
	
glittery(cell(X,Y)) :-
	location(cell(X,Y),T),
	glitter(yes,T).

wall_limits_x(X) :-
	location(cell(X,Y),T),
	direction(0,T),
	bump(yes,T).
	
wall_limits_y(Y) :-
	location(cell(X,Y),T),
	direction(90,T),
	bump(yes,T).

dead_wumpus :-
	scream(yes,T),
	retractall(maybeWumpus(cell(_,_))).


		
agent_pit(cell(X,Y)) :-
	X1 is X+1,
	X0 is X-1,
	Y1 is Y+1,
	Y0 is Y-1,
	visited(cell(X,Y0)),
	breezy(cell(X,Y0)),
	visited(cell(X,Y1)),
	breezy(cell(X,Y1)),
	visited(cell(X0,Y)),
	breezy(cell(X0,Y)),
	visited(cell(X1,Y)),
	breezy(cell(X1,Y)).

agent_pit1(cell(X,Y)) :-
	breezy(cell(X1,Y1)),
	adj(cell(X,Y),cell(X1,Y1)).

check_pit(cell(X,Y)) :-
	X1 is X+1,
	X0 is X-1,
	Y1 is Y+1,
	Y0 is Y-1,
	(breezy(cell(X,Y0));
	breezy(cell(X,Y1));
	breezy(cell(X0,Y));
	breezy(cell(X1,Y))).


agent_wumpus(cell(X,Y)) :-
	X1 is X+1,
	X0 is X-1,
	Y1 is Y+1,
	Y0 is Y-1,
	stinky(cell(X,Y0)),
	stinky(cell(X,Y1)),
	stinky(cell(X0,Y)),
	stinky(cell(X1,Y)).
	
check_wumpus(cell(X,Y)) :-
	X1 is X+1,
	X0 is X-1,
	Y1 is Y+1,
	Y0 is Y-1,
	(stinky(cell(X,Y0));
	stinky(cell(X,Y1));
	stinky(cell(X0,Y));
	stinky(cell(X1,Y))).
		
