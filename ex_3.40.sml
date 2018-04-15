(*
Exercise 3.39 Write a function find such that find(xs, i) returns the ith smallest
item in the list xs. This is called selection. Hoare’s algorithm for selection
is related to quick sort, and is much faster than sorting the list and returning the
ith element.
*)

(*
切割 list 是通用介面，把它提取出來
但是切割後的位置檢查，根據傳入的位置順序有不同處理方式
所以不需要過度提取，強調一個函數做一件事情
函數介面清楚最少功能，勝過提取，多點程式碼沒有關係
如果遞迴要增減參數，主體還是提取出放在 let，減少重複產生的函數
*)

fun find (lst, i, j) =
    let
        fun partition (a::bs) =
            let
                val pivot = a
                fun main_recu (left,right,[]) = (left, right)
                  | main_recu (left,right, x::xs) =
                    if x<=pivot then main_recu (x::left, right, xs)
                    else main_recu (left, x::right, xs);
                val (left, right) = main_recu ([],[],bs);
            in
                (pivot, left, right)
            end;
        fun left_accu ([], i) = []
          | left_accu (lst, i) =
            let
                val (pivot, left, right) = partition (lst);
                val slice_position = i - length (left) - 1;
            in
                if slice_position < 0 then left_accu (left, i)
                else if slice_position > 0 then left_accu (right, slice_position) @ (pivot :: left)
                else pivot :: left
            end;
        fun right_accu ([], i) = []
          | right_accu (lst, i) =
            let
                val (pivot, left, right) = partition (lst);
                val slice_position = i - length (left) - 1;
            in
                if slice_position < 0 then right_accu (left, i) @ (pivot :: right)
                else if slice_position > 0 then right_accu (right, slice_position)
                else pivot :: right
            end;
        fun recu ([], i, j) = []
          | recu (lst, i, j) =
            let
                val (pivot, left, right) = partition (lst);
                val check = length(left) + 1;
            in
                if check < i then recu (right, i - check, j - check)
                else if check > j then recu (left, i, j)
                else pivot :: right_accu(left, i) @ left_accu(right, j - check)
            end;
    in
        recu (lst, i, j)
    end
