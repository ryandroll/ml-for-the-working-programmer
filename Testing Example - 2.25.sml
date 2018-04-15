(*exprssion*)

signature ARITH =
sig
    type t
    val zero : t
    val sum : t * t -> t
    val diff : t * t -> t
    val prod : t * t -> t
    val quo : t * t -> t
end

structure Rational : ARITH =
struct
type t = int * int;

fun regu_form (n, d) =
    if d < 0 then (~n, ~d)
    else (n, d)

fun gcd (m, n) =
    if m = 0 then n
    else gcd (n mod m, m)


val zero = (0, 1);
fun sum ((n1, d1), (n2, d2)) =
    let
        val n = n1 * d2 + n2 * d1
        val d = d1 * d2
        val s = gcd (abs n, abs d)
    in
        regu_form(n div s, d div s)
    end
fun diff ((n1, d1), (n2, d2)) =
    let
        val n = n1 * d2 - n2 * d1
        val d = d1 * d2
        val s = gcd (abs n, abs d)
    in
        regu_form(n div s, d div s)
    end

fun prod ((n1, d1), (n2, d2)) =
    let
        val n = n1 * n2
        val d = d1 * d2
        val s = gcd (abs n, abs d)
    in
        regu_form(n div s, d div s)
    end

fun quo ((n1, d1), (n2, d2)) =
    let
        val n = n1 * d2
        val d = d1 * n2
        val s = gcd (abs n, abs d)
    in
        regu_form(n div s, d div s)
    end

end
