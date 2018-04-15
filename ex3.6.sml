(*
Exercise 3.6 What would happen if we changed [x] to x in the definition of
nrev?
*)

fun nrev [] = []
  | nrev (x :: xs) = (nrev xs) @ x;

val test = nrev [1, 2, 3, 4, 5];
