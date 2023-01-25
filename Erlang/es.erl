-module(es).
-compile(export_all).

%the expressions are evaluated in the order in witch they arrive, and the last is the returned one
add(A, B) -> A + B.

%haskell-like functions definitions ("by example")
factorial(0) -> 1;
factorial(N) -> 
 N*factorial(N - 1).
   
 

