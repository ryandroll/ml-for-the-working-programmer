(*
Exercise 3.2 Write a version of maxl using null, hd and tl, instead of patternmatching.
*)

fun take ([], i) = []
  | take (x::xs, i) = if i>0 then x::take(xs, i-1)
                      else [];

fun drop ([], _) = []
  | drop (x::xs, i) = if i>0 then drop (xs, i-1)
                      else x::xs;

drop (["Never","shall","sun","that","morrow","see!"], ~1);
take (explode"Throw do", ~1);
