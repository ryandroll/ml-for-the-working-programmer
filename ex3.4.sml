(*
Exercise 3.4 Write a function nth(l,n) to return the nth element of l (where
the head is element 0).
*)

fun nth (l, n) =
    if n = 0 then hd l
    else nth (tl l, n - 1)
