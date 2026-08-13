; (0, 200, 200, 1, 2, 100, 100)

eval(0, 0) = 0
eval(0, x) = x
eval(1, 0) = 1
eval(1, 0, 1) = 2
eval _ = 100

main = (
    (eval(0, 0)),
    (eval(0, 200)),
    (eval {0 = 0, 1 = 200}),
    (eval(1, 0)),
    (eval(1, 0, 1)),
    (eval {}),
    (eval 4),
)