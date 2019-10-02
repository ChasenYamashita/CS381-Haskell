%Homework6

when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).


%1a -------------------------------------------------

schedule(N,P,T):- enroll(N,A), 
					where(A,P), when(A,T).
					
schedule(S,B,T):- where(C,B), 
					enroll(S,C), when(C,T).
					
%1b	-------------------------------------------------

usage(X,Y):- where(Class,X), when(Class,Y).

%1c -------------------------------------------------

conflict(X,Y):- when(X,T), when(Y,T), where(X,S), where(Y,S), Y \= X.

%1d -------------------------------------------------



meet(X,Y):- enroll(X,C), enroll(Y,C); %if they are in the same class
            enroll(X,C), enroll(Y,D), where(C,R), where(D,R), 
			when(C,H1), when(D,H2), (succ(H1,H2); succ(H2,H1)).
			
			% if they're in the same room, in succesive hours.

%2a -------------------------------------------------

%rdup(L,M):- sort(L,M).
rdup([], []).
rdup([E],[E]).
rdup([D,D], [D]).

rdup([L|X], [L|Y]) :- rdup(X, Y), not(member(L, X)).
rdup([L|X], Y) :- rdup(X, Y), member(L, X).



%2b -------------------------------------------------

flat([], []).
%If the list element is also a list, traverse it recursively.
flat([H|T], F):- flat(H, F2), flat(T, F3), append(F2,F3,F).
flat(H, [H]).  %If an element is given for H, put it in flat list F.

%2c -------------------------------------------------

%base cases
project(_, [], []).
project([], _, []).
project(1,[X|_], [X]) :- !.

%append the list of positions recursively
project([H|T], X, Proj):- projectRecur(H, X, Proj2, 0), project(T, X, Proj3), append(Proj2, Proj3, Proj).


%increment if not at position, store result as a single element list
projectRecur(Pos, [_|Xs], Result, Cur):- incr(Cur, CurNew), \+ CurNew == Pos, projectRecur(Pos, Xs, Result, CurNew).
projectRecur(Pos, [X|_], Result, Cur):- incr(Cur, CurNew), CurNew == Pos, Result = [X].

incr(X, X1) :- X1 is X+1.