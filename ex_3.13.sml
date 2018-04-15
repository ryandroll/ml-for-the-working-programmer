(*
Exercise 3.13 We are seldom fortunate enough to have an infinite supply of
coins. Modify allChange to make change from a finite purse.
*)

val gb_coins = [50, 20, 10, 5, 2, 1]
and us_coins = [25, 10, 5, 1];

val coin_num = [1, 2, 3, 4, 5, 6]

fun allChange (coins, coinvals, coin_num, 0) = [coins]
  | allChange (coins, [], coin_num, amount) = []
  | allChange (coins, c::coinvals, n::coin_num, amount) =
    if amount<0 orelse n<0 then []
    else allChange(c::coins, c::coinvals, (n-1)::coin_num, amount-c) @
         allChange(coins, coinvals, coin_num, amount);

val test_01 = allChange([], [5,2], [2,10], 16);
val test_02 = allChange([], gb_coins, [1, 1, 1, 3, 2, 1], 16);
