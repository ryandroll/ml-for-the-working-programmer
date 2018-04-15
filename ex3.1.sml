(*
Exercise 3.1 Write a version of maxl using null, hd and tl, instead of patternmatching.
*)

fun maxl lst =
    let
       fun checker (lst, temp) =
           if null lst then temp
           else if hd lst < temp then checker (tl lst, hd lst)
           else checker (tl lst, temp)
    in
        checker (tl lst, hd lst)
    end
