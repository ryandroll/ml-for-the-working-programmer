(*
Exercise 3.14 Modify allChange to accumulate its result in an extra argument,
eliminating the call to append. Compare its efficiency with the original
version by making change for 99 pence.
*)

val gbcoins = [50,20,10,5,2,1]
and uscoins = [25,10,5,1];

fun allChange (coins, coinvals, 0) = [coins]
  | allChange (coins, [], amount) = []
  | allChange (coins, c::coinvals, amount) =
    if amount<0 then []
    else allChange(c::coins, c::coinvals, amount-c) @
         allChange(coins, coinvals, amount);
(*my wrong*)
fun allChange_tmp (coins, coinvals, 0, ans) =  coins :: ans
  | allChange_tmp (coins, [], amount, ans) = ans
  | allChange_tmp (coins, c::coinvals, amount, ans) =
    if amount<0 then allChange_tmp (coins, coinvals, amount, ans)
    else allChange_tmp (c::coins, c::coinvals, amount-c, ans)

fun allChange2 (coins, coinvals, 0, coinslist)       = coins::coinslist
  | allChange2 (coins, [],  amount, coinslist)       = coinslist
  | allChange2 (coins, c::coinvals, amount, coinslist) =
    if amount<0 then coinslist
    else allChange2(c::coins, c::coinvals, amount-c,
                    allChange2(coins, coinvals, amount, coinslist));

val test_01 = allChange([], [5,2], 16);
val test_01 = allChange2 ([], [5,2], 16, []);
val test_02 = allChange([], gbcoins, 16);
