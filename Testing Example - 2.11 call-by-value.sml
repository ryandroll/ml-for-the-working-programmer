(*exprssion*)

fun sqr (x) : int = x * x;
fun zero (x : int) = 0;

fun merge (xlst, ylst) : real list =
    case (xlst, ylst)
     of ([], ylst) => ylst
      | (xlst, [])  => xlst
      | (x :: xs, y :: ys) => if x < y then x :: merge (xs, ylst)
                             else y :: merge (xlst, ys)
