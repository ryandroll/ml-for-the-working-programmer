(*
Exercise 3.39 Write a function find such that find(xs, i) returns the ith smallest
item in the list xs. This is called selection. Hoare’s algorithm for selection
is related to quick sort, and is much faster than sorting the list and returning the
ith element.
*)

fun quick [] = []
  | quick [x] = [x]
  | quick (a::bs) = (*the head "a" is the pivot*)
    let fun partition (left,right,[]): real list =
            (quick left) @ (a :: quick right)
          | partition (left,right, x::xs) =
            if x<=a then partition (x::left, right, xs)
            else partition (left, x::right, xs)
    in partition([],[],bs) end;

fun find (a::bs, i) =
    let
        fun partition (left,right,[]) =
                (left,right)
              | partition (left,right, x::xs) =
                if x<=a then partition (x::left, right, xs)
                else partition (left, x::right, xs);
        val (left, right) = partition ([],[],bs);
        val checker = length (left) + 1
    in
        if checker < 0 then find (left, i)
        else if checker > 0 then find (right, checker)
        else a
    end;

fun find2 (lst,i) =
    let
        fun partition (a::bs) =
            let
                val pivot = a
                fun main_recu (left,right,[]) = (left, right)
                  | main_recu (left,right, x::xs) =
                    if x<=pivot then main_recu (x::left, right, xs)
                    else main_recu (left, x::right, xs);
                val (left, right) = main_recu ([],[],bs);
                val check = length (left) + 1;
            in
                (pivot, left, right, check)
            end;
        val (pivot, left, right, check) = partition (lst);
    in
        if i - check < 0 then find2 (left, i)
        else if i - check > 0 then find2 (right, i - check)
        else pivot
    end
