rastrigin = function(x, y) {
  10*2 + (x^2 - 10*cos(2*pi*x)) + (y^2 + 10*cos(2*pi*y))
}

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

holder <- function(x, y) {
  -abs(sin(x)*cos(y)*exp(abs(1-sqrt(x^2 + y^2)/pi)))
}

himmelblau <- function(x, y) {
  (x^2 + y - 11)^2 + (x + y^2 - 7)^2
}
  
beale <- function(x, y) {
  (1.5 - x*(1 - y))^2 + (2.25 - x*(1 - y^2))^2 + (2.625 - x*(1 - y^3))^2
}

# This function requires the relevant domains for x and y (both as length 2 limits), an optimization
# function that will take x and y arguments, and dplyr and plotly packages.
optimvis = function(domain_x, domain_y, optimfun=bukin, col=viridis::viridis(1000)) {
  require(dplyr)
  d = data.frame(x=seq(domain_x[1], domain_x[2], length.out = 100),
                 y=seq(domain_y[1], domain_y[2], length.out = 100)) %>%
    arrange(x)
  zmat = apply(expand.grid(d), 1, function(x) optimfun(x[1], x[2])) %>% 
    matrix(100, 100, byrow = T)

  a <- list(
    visible=FALSE
  )
  
  library(plotly)
  
  plot_ly(x=d$x, y=d$y, z=zmat, 
          type = 'surface', 
          colors = col, 
          hoverinfo='none') %>% 
    hide_colorbar() %>% 
    lazerhawk::theme_blank() %>% 
    layout(plot_bgcolor='black',
           paper_bgcolor='black',
           scene = list(xaxis = a,
                        yaxis = a,
                        zaxis = a
           )
    )
}
