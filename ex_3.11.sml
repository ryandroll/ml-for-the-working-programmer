(*
Exercise 3.11 Write a function to express integers as Roman numerals. Supplied
with suitable arguments, your function should be able to express 1984 as
either MDCCCCLXXXIIII or MCMLXXXIV.
*)

(*範例似乎移除三個限制*)

val orign = [(M, 1000), (D, 500), (C, 100), (L,50), (X, 10), (V, 5), (I, 1)]
val rule_left_minus = [(IV, 4), (IX, 9), (XL, 40), (XC, 90), (CD, 300), (CM, 900)]


fun allChange (coins, coinvals, 0) = [coins]
  | allChange (coins, [], amount) = []
  | allChange (coins, c::coinvals, amount) =
    if amount<0 then []
    else allChange(c::coins, c::coinvals, amount-c) @
         allChange(coins, coinvals, amount);

val test_01 = allChange([], [5,2], 16);
val test_02 = allChange([], gb_coins, 16);

datatype bi  = Z | O

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
