% 2. domácí úloha
%
% a) Implementujte predikát flat(+List, ?Result), který zploští libovolně
% zanořený seznam seznamů List.
%
% flat([], R).
% R = [].
%
% flat([[]], R).
% R = [].
%
% flat([a,b,c], R).
% R = [a,b,c].
%
% flat([a,[[],b,[]],[c,[d]]], R).
% R = [a,b,c,d].
%
% Tento predikát měl být deterministický (speciálně otestujte, že po odmítnutí
% neprodukuje duplikátní/nesprávné výsledky). Pokuste se o efektivní
% implementaci pomocí akumulátoru.

%otacanie zoznamu
rev(XS, R) :- rev_(XS, [], R).

rev_([], A, A).
rev_([X|XS], A, R) :-
  rev_(XS, [X|A], R).

%volanie
flat(X, R) :-
    flat_(X,[], R1),
    rev(R1,R).

%koniec
flat_([], R, R).

%odstranenie prazdneho zoznamu zo zaciatku
flat_([[]| T], A, R) :-
   flat_(T, A, R).

%pismenko/cislo
flat_([H | T], A, R) :-
   \+ is_list(H),
   A1 = [H | A],
   flat_(T, A1, R),
   !.

%vnoreny zoznam
flat_([H], A, R) :-
  flat_(H, A, R),
  !.

%obecny pripad
flat_( [[H | T] | T2] , A, R) :-
  flat_([H | T], A, R1),
  A1 = R1,
  flat_(T2, A1, R). 

% b) Implementuje predikát transp(+M, ?R), který transponuje matici M (uloženou
% jako seznam seznamů). Pokud M není ve správném formátu (např. řádky mají
% různé délky), dotaz transp(M, R) by měl selhat.
%
% transp([], R).
% R = [].
%
% transp([[],[],[]], R).
% R = [].
%
% transp([[a,b],[c,d],[e,f]], R).
% R = [[a,c,e],[b,d,f]].
%
% transp([[a],[b,c],[d]], R).
% false.
% access([[a,b],[c,d],[e,f]], 1,2, Value).
% get_row([[a,b],[c,d],[e,f]], 1, Row).

%pristup k prvku v 2D poli
access(Matrix, I, J, Value) :-
    nth0(I, Matrix, Row),
    nth0(J, Row, Value).

%vrati stlpec
get_col(Matrix, J ,Col) :-
    findall(Value, access(Matrix, _, J, Value), Col).

%vrati dlzku najdlhsieho pola alebo fail
check_size([],_):-!.
check_size([H|T], Max_length) :-
    length(H, Len),
    Max_length = Len,
    check_size(T, Max_length). 

%transponuje maticu
transp([],[]).
transp(In_matrix, Out_matrix) :-
    check_size(In_matrix,Len),
    Len2 is Len -1,
    transp_(In_matrix, Len2, Out_matrix1),
    rev(Out_matrix1,Out_matrix).

transp_(_,-1,[]) :-!.
transp_(Matrix,Len, Out_matrix) :-
    Len > -1,    
    get_col(Matrix, Len, Col),
    Len2 is Len - 1,
    transp_(Matrix, Len2, Out),
    Out_matrix = [Col|Out].

    




% c) (BONUSOVÁ ÚLOHA) Implementuje vkládání prvku pro AVL stromy.
%
% Použijte následující reprezentaci:
% prázdný strom: nil
% uzel: t(B,L,X,R) kde
%   L je levý podstrom,
%   X je uložený prvek,
%   R je pravý podstrom,
%   B je informace o vyvážení:
%     B = l (levý podstrom je o 1 hlubší)
%     B = 0 (oba podstromy jsou stejně hluboké)
%     B = r (pravý podstrom je o 1 hlubší)
%
% avlInsert(+X, +T, -R)
% X je vkládané číslo, T je strom před přidáním, R je strom po přidání
%
% avlInsert(1, nil, R).
% R = t(0, nil, 1, nil).
%
% avlInsert(2, t(0, nil, 1, nil), R).
% R = t(r, nil, 1, t(0, nil, 2, nil)).
%
% avlInsert(1, t(0, nil, 1, nil), R).
% R = t(0, nil, 1, nil).
