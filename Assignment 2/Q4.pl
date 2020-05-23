divisible([H|T],N):- 0 =:= (N rem H),!, divisible(T,N).

smallNumbers(D,L):- min_list(D,N), bagof(X,between(1,N,X),L).

allListBelongs(_,0,_).
allListBelongs([H|T],Length,SuperList):-
    member(H,SuperList),
    length([H|T], Length),
    SubLength is Length - 1,
    allListBelongs(T,SubLength,SuperList).

newDivisible([H|_],_):- H =:=1, fail.
newDivisible([H|T],N):- 0 =:= (N rem H),!, divisible(T,N).
divisibleList([],_).
divisibleList([FactorH|FactorT],DividedList):-
    newDivisible(FactorH,DividedList),
    divisibleList(FactorT,DividedList).

generateList(D,Length,L):-
    smallNumbers(D,Range),
    allListBelongs(L,Length,Range),
    \+divisibleList(D,L).