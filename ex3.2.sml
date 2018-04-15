(*
Exercise 3.2 Write a version of maxl using null, hd and tl, instead of patternmatching.
*)

fun last lst =
    case lst
     of [] => nil
      | [x] => x
      | _ :: xs => last xs;
