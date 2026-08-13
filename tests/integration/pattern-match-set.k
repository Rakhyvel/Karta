; (10, 100, 200, 400, 300, 300, 300)

eval {{.d}} = 10
eval {.a, .b} = 400
eval {.a} = 100
eval {.b} = 200
eval _ = 300

main = (
    (eval {{.d}}),
    (eval {.a}),
    (eval {.b, .c}),
    (eval {.b, .a}),
    (eval {.c}),
    (eval {}),
    (eval 4)
)