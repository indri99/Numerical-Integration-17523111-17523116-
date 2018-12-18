f1 <- function(x) {
  return(1/(1+x))
}

df <- data.frame(cbind(c(0,1,1,0), c(0, f2(1), 0, 0)))

ggplot(data = data.frame(x=0), mapping = aes(x=x)) +
  stat_function(fun = f1, size = 1.05, alpha = 0.75, color = 'blue') +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = f1(1))) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = f1(1))) +
  geom_polygon(data = df, aes(x=X1, y=X2), fill = 'blue', alpha = 0.2) +
  geom_area(stat = 'function', fun =  f1, fill = 'black', alpha = 0.3, xlim = c(0, 1)) +
  xlim(c(0,1))

#trapezoid
trapezoid <- function(f1, a, b) {
  if (is.function(f1) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  h <- b-a
  fxdx <- (h/2) * (f1(a) + f1 (b))
  return(fxdx)
}

trapezoid(f1, 0, 1)

#simpsons
simpsons.rule <- function(f1, a, b) {
  if (is.function(f1) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  h <- (b - a) / a
  x0 <- a
  x1 <- a + h
  x2 <- b
  
  s <- (h/3) * (f1(x0) + 4 * f1(x1) + f1(x2))
  
  return(s)
}

simpsons.rule(f1, 0, 1)
