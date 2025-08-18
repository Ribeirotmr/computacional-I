set.seed(123)

intmc <- function(f, a, b, n = 10000,...){
    x_n <- runif(n, min = a, max = b)

    f_xn <- f(x_n, ...) 
    media_fxn <- mean(f_xn)

    integral <- (b - a) * media_fxn
    
    return(integral)
}


f_exotica <- function(x) 500 * abs(sin(x) * cos(sqrt(x)) + log(x + 1) * exp(-x/20))

mc <- intmc(f_exotica, 1, 50, n = 100000)

print(mc)