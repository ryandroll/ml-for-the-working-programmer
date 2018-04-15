(*
Exercise 3.3 What do take(l,i) and drop(l,i) return when i > length(l),
and when i < 0? (The library versions of take and drop would raise exceptions.)
*)

fun take ([], i) = []
  | take (x::xs, i) = if i>0 then x::take(xs, i-1)
                      else [];

fun drop ([], _) = []
  | drop (x::xs, i) = if i>0 then drop (xs, i-1)
                      else x::xs;

drop (["Never","shall","sun","that","morrow","see!"], ~1);
take (explode"Throw do", ~1);
