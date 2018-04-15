(*
Write a function to determine whether one time of day, in the
form (hours, minutes, AM or PM), comes before another. As an example,
(11, 59, "AM") comes before (1, 15, "PM").
*)

fun time_before ((h1, m1, ampm1), (h2, m2, ampm2)) =
    let
        fun digi_time (h, m, ampm) =
            if ampm = "AM"
            then if h = 12 then m
                 else h * 60 + m
            else if h = 12 then 720 + m
            else (h + 12) * 60 + m
    in
        digi_time (h1, m1, ampm1) < digi_time (h2, m2, ampm2)
    end

(* 解答竟然是錯的 XDD，沒有考慮到十二點的情況*)

fun earlier ((h1, m1, apm1), (h2:int, m2:int, apm2)) =
    apm1 = "AM" andalso apm2 = "PM"
    orelse apm1=apm2 andalso (h1<h2 orelse h1=h2 andalso m1<m2);

val test = time_before ((12, 59, "AM"), (1, 15, "AM"));
val test = earlier ((12, 59, "AM"), (1, 15, "AM"));
