(*
Exercise 3.39 Write a function find such that find(xs, i) returns the ith smallest
item in the list xs. This is called selection. Hoare’s algorithm for selection
is related to quick sort, and is much faster than sorting the list and returning the
ith element.
*)


fun find (lst, i, j) =
    let
        fun partition (l)
                (left,right,[]) = (left,right,[])
          | partition (left,right,[x]) = (left,right,[x])
          | partition (left,right, x::xs) =
            if x<=a then partition (x::left, right, xs)
            else partition (left, x::right, xs);

        fun left_accu ([], i) = []
          | left_accu (x :: xs, i) =
            let
                val (left, right) = partition ([],[],xs);
                val check = length (left) + 1;
            in
                if i - check < 0 then left_accu (left, i)
                else if i - check > 0 then left_accu (right, i - check) @ [x] @ left
                else [x] @ left
            end;
        fun right_accu ([], i) = []
          | right_accu (a :: bs, i) =
            let
                fun partition (left,right,[]) =
                    (left,right)
                  | partition (left,right, x::xs) =
                    if x<=a then partition (x::left, right, xs)
                    else partition (left, x::right, xs);
                val (left, right) = partition ([],[],bs);
                val check = length (left) + 1;

            in
                if i - check < 0 then right_accu (left, i) @ [a] @ right
                else if i - check > 0 then right_accu (right, i - check)
                else [a] @ right
            end;
        fun recu ([], i, j) = []
          | recu (a::bs, i, j) =
            let
                fun partition (left,right,[]) =
                    (left,right)
                  | partition (left,right, x::xs) =
                    if x<=a then partition (x::left, right, xs)
                    else partition (left, x::right, xs);

                val (left, right) = partition ([],[],bs);
                val check = length (left) + 1;
            in
                if check < i then recu (right, i - check, j - check)
                else if check > j then recu (left, i, j)
                else right_accu(left, i) @ [a] @ left_accu(right, j - check)
            end;
    in
        recu (a::bs, i, j)
    end
