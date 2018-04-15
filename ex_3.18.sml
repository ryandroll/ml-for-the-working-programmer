(*
Exercise 3.18 Decimal numerals can be held as lists of integers from 0 to 9.
Write functions to convert between binary and decimal: both directions. Compute
the factorial of 100.
*)

fun bincarry (0, ps) = ps
  | bincarry (1, []) = [1]
  | bincarry (1, p::ps) = (1-p) :: bincarry(p, ps);

fun binsum (c, [], qs) = bincarry (c,qs)
  | binsum (c, ps, []) = bincarry (c,ps)
  | binsum (c, p::ps, q::qs) =
    ((c+p+q) mod 2) :: binsum((c+p+q) div 2, ps, qs);

fun binprod ([], _) = []
  | binprod (0::ps, qs) = 0::binprod(ps,qs)
  | binprod (1::ps, qs) = binsum(0, qs, 0::binprod(ps,qs));

fun bi_gi_num num =
    case num
     of 0 => []
      | num => (num mod 2) :: bi_gi (num div 2);

fun ten_x lst =
    case lst
     of [] => []
      | x :: xs => (x * 10) :: (ten_x xs);


fun dig_bin_lst lst =
    let
        fun dig_bin_num num =
            case num
             of 0 => []
              | num => (num mod 2) :: dig_bin_num (num div 2);
        fun ten_x lst =
            case lst
             of [] => []
              | x :: xs => (x * 10) :: (ten_x xs);
        fun recu (lst, accu) =
            case lst
             of [] => accu
              | x :: xs => binsum (0, dig_bin_num x, recu (ten_x xs, accu))
    in
        recu (lst, [])
    end;

fun digcarry (0, ps) = ps
  | digcarry (x, []) = [x]
  | digcarry (x, p::ps) = (x + p) mod 10 :: digcarry((x + p) div 10, ps);

fun digsum (c, [], qs) = digcarry (c,qs)
  | digsum (c, ps, []) = digcarry (c,ps)
  | digsum (c, p::ps, q::qs) =
    ((c+p+q) mod 10) :: digsum((c+p+q) div 10, ps, qs);

fun bin_dig_lst lst =
    let
        fun dig_num num =
            case num
             of 0 => []
              | num => (num mod 10) :: bin_dig_num (num div 10);
        fun twice_x lst =
            case lst
             of [] => []
              | x :: xs => (x * 2) :: (twice_x xs);
        fun recu (lst, accu) =
            case lst
             of [] => accu
              | x :: xs => digsum (0, dig_num x, recu (twice_x xs, accu))
   in
        recu (lst, [])
    end



fun num_lst num =
    case num
     of 0 => []
      | _ => (num mod 10) :: (num_lst (num div 10));

fun fact_lst dig_num =
    case dig_num
     of 0 => [1]
      | _ => binprod (bin_dig_lst (num_lst dig_num), fact_lst (dig_num -1));
