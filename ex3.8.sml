(*
Exercise 3.8 Compare the following function with concat, considering its effect
and efficiency:
fun f [] = []
| f ([]::ls) = f (ls)
| f ((x::l)::ls) = x :: f (l::ls);
*)

fun f [] = []
  | f ([]::ls) = f (ls)
  | f ((x::l)::ls) = x :: f (l::ls);

fun concat [] = []
  | concat (l :: ls) = l @ concat ls;

(I guess equal because @ usage equal to ::)
