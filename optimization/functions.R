bukin = function(x, y) {
  100 * sqrt(abs(y-.01*x^2)) + .01*abs(x+10)
}

ackley = function(x, y) {
  -20 * exp(-.2*sqrt(.5*(x^2+y^2))) -
    exp(.5*(cos(2*pi*x) + cos(2*pi*y))) + exp(1) + 20
}

levi = function(x, y) {
  sin(3*pi*x)^2 + (x-1)^2 * (1 + sin(3*pi*y)^2) +
    (y-1)^2*(1+sin(2*pi*y)^2)
}

cross_in_tray = function(x, y) {
  -.0001*(abs(sin(x) * sin(y) * exp(abs(100-sqrt(x^2 + y^2)/pi))) + 1)^.01
}

easom <- function(x, y) {
  -cos(x)*cos(y)*exp(-((x-pi)^2 + (y-pi)^2))
}