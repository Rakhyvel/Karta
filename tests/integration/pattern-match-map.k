; (400, 200, 500, 300, 300)

eval {.a = 1} = 400
eval {.a = {.b = x}} = x
eval {.a = x} = x
eval _ = 300

main = (
    (eval {.a = 1}),
    (eval {.a = 200}),
    (eval {.a = {.b = 500}}),
    (eval {}),
    (eval 4),
)