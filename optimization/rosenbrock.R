# https://en.wikipedia.org/wiki/Test_functions_for_optimization#Test_functions_for_multi-objective_optimization

# rosenbrock function

source('optimization/functions.R')

library(dplyr)
d = data.frame(x = seq(-2, 2, length.out = 100),
               y = seq(-1, 3, length.out = 100)) %>%
  arrange(x)

zmat = apply(expand.grid(d), 1, function(x)
  rosenbrock(x = x[1], y = x[2])) %>% matrix(100, 100, byrow = T)


a <- list(
  visible=FALSE
)

library(plotly)

plot_ly(
  x = d$x,
  y = d$y,
  z = zmat,
  type = 'surface',
  colors = scico::scico(1000, palette = 'buda'),
  hoverinfo = 'none'
) %>%
  hide_colorbar() %>%
  lazerhawk::theme_blank() %>%
  layout(
    plot_bgcolor = 'black',
    paper_bgcolor = 'black',
    scene = list(
      xaxis = a,
      yaxis = a,
      zaxis = a
    )
  )
