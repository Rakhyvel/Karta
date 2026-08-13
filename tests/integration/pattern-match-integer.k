; (0, 1, 0, 1, 0)

even? 0 = 0
even? 1 = 1
even? n = even? (@sub(n, 2))

main = ((even? 0), (even? 1), (even? 4), (even? 5), (even? 10))