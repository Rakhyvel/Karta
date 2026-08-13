; (.t, {}, .t, {}, .t, .t, {}, .t, .t, {}, .t, .t, {})

eval 0 = 0
eval 2 = 0
eval(x, y) = 2
eval(x, y, z) = 3

eval2 x 0 = 0

my-set = {.a, .b}

truthify x = if x then .t else {}

main = (
    (@accepts?(eval, 0)),
    (@accepts?(eval, 1)),
    (@accepts?(eval, 2)),
    (@accepts?(eval, 3)),
    (@accepts?(eval, (1, 2))),
    (@accepts?(eval, (1, 2, 3))),
    (@accepts?(eval, (1, 2, 3, 4))),
    (truthify (@accepts?(eval2, 0))),
    (@accepts?(eval2 1, 0)),
    (@accepts?(eval2 1, 1)),
    (@accepts?(my-set, .a)),
    (@accepts?(my-set, .b)),
    (@accepts?(my-set, .c)),
)