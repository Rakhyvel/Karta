; (4, 4.5, 'H', "Hello, World!")
main = let
    test = {
        .int = 4,
        .float = 4.5,
        .char = 'H',
        .string = "Hello, World!",
    }
in ((test.int), (test.float), (test.char), (test.string))