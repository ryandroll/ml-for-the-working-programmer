(*
Exercise 3.10 Is rev(rtake(l,i,[])) more efficient than take(l,i)? Consider
all the costs involved.
*)

fun rtake ([], _, taken) = taken
  | rtake (x::xs, i, taken) =
    if i>0 then rtake(xs, i-1, x::taken)
    else taken;

fun take ([], i) = []
  | take (x::xs, i) = if i>0 then x::take(xs, i-1)
                      else [];

fun revAppend ([], ys) = ys
  | revAppend (x::xs, ys) = revAppend (xs, x::ys);

fun rev xs = revAppend(xs,[]);

(*
應該 rtake 較高，一個是 2n 另外一個還是 n^2/2
*)
