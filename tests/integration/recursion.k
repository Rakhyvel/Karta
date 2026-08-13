; 120

fact x when @eql(x, 1) = x
fact x = @mul(fact (@sub(x, 1)), x)

main = fact 5