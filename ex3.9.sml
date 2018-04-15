(*
Exercise 3.9 Give an equivalent definition of zip that does not depend upon
the order in which patterns are considered.
*)

fun zip (xlst, ylst) =
    case (xlst, ylst)
     of (x :: xs, y :: ys) => (xs, ys) :: zip (xs, ys)
      | ([], ylst) => []
      | (xlst, []) => []
