(*
Exercise 3.16 Write a function to divide one binary numeral by another
*)

fun bincarry (0, ps) = ps
  | bincarry (1, []) = [1]
  | bincarry (1, 0::ps) = 1 :: bincarry(0, ps)
  | bincarry (1, 1::ps) = 0 :: bincarry(1, ps)

fun binsum (c, [], qs) = bincarry (c,qs)
  | binsum (c, ps, []) = bincarry (c,ps)
  | binsum (c, p::ps, q::qs) =
    ((c+p+q) mod 2) :: binsum((c+p+q) div 2, ps, qs);

fun binprod ([], _) = []
  | binprod (0::ps, qs) = 0::binprod(ps,qs)
  | binprod (1::ps, qs) = binsum(0, qs, 0::binprod(ps,qs));
