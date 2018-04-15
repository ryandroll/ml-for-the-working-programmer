(*
Exercise 2.7 Old English money had 12 pence in a shilling and 20 shillings in
a pound. Write functions to add and subtract two amounts, working with triples
(pounds, shillings, pence).
*)

fun add_eng_money ((pnd1, shl1, pen1), (pnd2, shl2, pen2)) =
    let
        val pence = (pnd1 + pnd2) * 240 + (shl1 + shl2) * 12 + (pen1 + pen2)
    in
        (pence div 240, (pence mod 240) div 12, pence mod 12)
    end

val test = add_eng_money ((1, 7, 3), (1, 15, 12));
