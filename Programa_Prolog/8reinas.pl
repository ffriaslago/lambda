:- use_module(library(clpfd)).

valid_queen((Row, Col), N) :-
  between(1, N, Col),
  between(1, N, Row).

valid_board([], _).
valid_board([Head|Tail], N) :- valid_queen(Head, N), valid_board(Tail, N).

rows([], []).
rows([(Row, _)|QueensTail], [Row|RowsTail]) :-
  rows(QueensTail, RowsTail).

cols([], []).
cols([(_, Col)|QueensTail], [Col|ColsTail]) :-
  cols(QueensTail, ColsTail).

diags1([], []).
diags1([(Row, Col)|QueensTail], [Diagonal|DiagonalsTail]) :-
  Diagonal is Col - Row,
  diags1(QueensTail, DiagonalsTail).

diags2([], []).
diags2([(Row, Col)|QueensTail], [Diagonal|DiagonalsTail]) :-
  Diagonal is Col + Row,
  diags2(QueensTail, DiagonalsTail).

eight_queens(Board, N) :-
  length(Board, N),
  valid_board(Board, N),

  rows(Board, Rows),
  cols(Board, Cols),
  diags1(Board, Diags1),
  diags2(Board, Diags2),

  all_distinct(Rows),
  all_distinct(Cols),
  all_distinct(Diags1),
  all_distinct(Diags2).

