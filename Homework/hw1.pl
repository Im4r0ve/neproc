% 1. domácí úloha
%
% a) Implementujte logaritmus o základu 2 (dolní celou část) na unárně
% reprezentovaných číslech.
%
% logtwo(+N, ?Vysledek)
%
% Nápověda: Může se vám hodit pomocný predikát pro půlení.
%
% logtwo(0, R).
% false.
%
% logtwo(s(s(s(0))), R).
% R = s(0).
%
% logtwo(s(s(s(s(0)))), R).
% R = s(s(0)).

% Půlení.
half(0, 0).
half(s(0), 0).
half(s(s(X)), s(R)) :- 
    half(X, R).

logtwo(s(0), 0).
logtwo(X,R) :-  
    X \= 0,
    half(X,X1),
    logtwo(X1,R1),
    R = s(R1). %pri vynorovani +1

% b) Implementujte predikát, který spočte n-té Fibonacciho číslo lépe než
% v exponenciálním čase (ideálně pouze lineárně mnoho sčítání).
%
% fib(+N, ?Vysledek)
%
% Nápověda: Zkuste nejdřív implementovat obecnější predikát, kde si můžete
% zvolit počáteční čísla.
%
% F_0 = 4
% F_1 = 5
% F_2 = 4 + 5 = 9
% F_3 = 5 + 9 = 14
%
% generalizedFib(3, 4, 5, R).
% R = 14.

fib(0,0).
fib(N,Result) :-
    generalizedFib(N,1,1,Result).   

generalizedFib(1,_,R,R).
generalizedFib(N,F0,F1,R) :-
    N > 0,
    N2 is N - 1,
    R1 is F0 + F1,
    generalizedFib(N2,F1,R1,R).
 
% c) (BONUSOVÁ ÚLOHA) Implementuje predikát pro sčítání dvou binárních čísel.
%
% Můžete použít např. následující reprezentaci:
%
% 13[dec] = 1101[bin] = b(1, b(0, b(1, b(1, e))))
%
% Příklad použití:
% addBin(b(1, b(0, b(1, e))), b(1, b(1, b(0, b(1, e)))), R).
% R = b(0, b(0, b(0, b(0, b(1, e))))).
%
% resp.
%
% addBin([1, 0, 1], [1, 1, 0, 1], R).
% R = [0, 0, 0, 0, 1].


%nefunguje uplne dobre, uz asi nestiham opravit

%addBin(first,second,carry,result)
addBin(0,0,C,R) :- C = 0,R = 0.
addBin(1,1,C,R) :- C = 1,R = 1.
addBin(1,0,C,R) :- C = 0,R = 1.
addBin(0,1,C,R) :- C = 0,R = 1.
%posunie sa v array
%pozrie ktory pripad nastal
%prida podla carry
addBin([],[],1,append([1],Res)).
addBin([],[],0,Res).


addBin([H1|T1],[H2|T2],C,Res) :- 
    addBin(H1,H2,C1,R1),
    C2 is C + C1,
    R2 is R1 + C2,%0..3
    (
        R2 =:= 2 -> append([0],Res,Res1), C2 is 1; 
        R2 =:= 3 -> append([1],Res,Res1), C2 is 1;
        append([R2],Res,Res1)
    ), 
    addBin(T1,T2,C2,Res1).



toNat(N, R) :-
    integer(N),
    toNat_(N, R).
  
toNat_(N, R) :- N > 0 ->
    (N2 is N - 1, toNat_(N2, R2), R = s(R2));
    R = 0.
  
fromNat(0, 0).
  fromNat(s(N), R) :-
    fromNat(N, R2),
    R is R2 + 1.
  
  nat(0).
  nat(s(X)) :- nat(X).
  
  add(0, Y, Y) :- nat(Y).
  add(s(X), Y, s(Z)) :-
    add(X, Y, Z).
  
  less(0, s(Y)) :- nat(Y).
  less(s(X), s(Y)) :- less(X, Y).