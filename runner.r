source("test.r")
v <- Map(function(x) {
    foo(catala_struct_FooIn(x_in = catala_integer_from_numeric(x)))
}, c(1:10000))
