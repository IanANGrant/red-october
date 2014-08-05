val () = load "Prolog";

val () = Meta.quotation := true;

val rs = Prolog.rules `
   male(albert).
   male(edward).

   female(alice).
   female(victoria).

   father_of(albert,edward).
   father_of(albert,alice).

   mother_of(victoria,edward).
   mother_of(victoria,alice).

   parents(X,M,F):-mother_of(M,X),father_of(F,X).

   sister_of(X,Y):-
      female(X),
      parents(X,M,F),
      parents(Y,M,F).

   brother_of(X,Y):-
      !=(X,Y),
      male(X),
      parents(X,M,F),
      parents(Y,M,F).
`;

val g1 = Prolog.goal `!=(alice,alice)`
val r1 = Prolog.prolog rs g1

val g = Prolog.goal `sister_of(alice,edward)`
val r = Prolog.prolog rs g

val g' = Prolog.goal `parents(edward,X,Y)`
val r' = Prolog.prolog rs g'

(* According to John Harrison, every girl is her own sister. Maybe
   he's right! *)

val g'' = Prolog.goal `sister_of(alice,alice)`
val r'' = Prolog.prolog rs g''

val g''' = Prolog.goal `brother_of(X,alice)`
val r''' = Prolog.prolog rs g'''
 
val rs' = Prolog.rules `
   append([],L,L).
   append([H|T],L,[H|A]):-append(T,L,A).
`;

val g'''' = Prolog.goal `append([1,2],[3],[1,2,3])`
val r'''' = Prolog.prolog rs' g''''
   
val g''''' = Prolog.goal `append([1,2],[3],[1,2])`
val r''''' = Prolog.prolog rs' g'''''

val rs'' = Prolog.rules `
   edge(g,h).
   edge(e,f).
   edge(f,e).
   edge(a,b).
   edge(b,c).
   edge(c,d).
   edge(d,a).
   path(A,B):-edge(A,B).
   path(A,B):-edge(A,C),path(C,B).
`;

val g2 = Prolog.goal `path(a,d)`
val r2 = Prolog.prolog rs'' g2

val g3 = Prolog.goal `path(b,X)`
val r3 = Prolog.prolog rs'' g3

val g4 = Prolog.goal `path(a,a)`
val r4 = Prolog.prolog rs'' g4

val g5 = Prolog.goal `path(g,g)`
val r5 = Prolog.prolog rs'' g5
