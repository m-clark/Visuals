# https://en.wikipedia.org/wiki/Test_functions_for_optimization#Test_functions_for_multi-objective_optimization

# Beale function

source('optimization/functions.R')

library(dplyr)
d = data.frame(x=seq(-4.5, 4.5, length.out = 100),
               y=seq(-4.5, 4.5, length.out = 100)) %>%
  arrange(x)
zmat = apply(expand.grid(d), 1, function(x) beale(x[1], x[2])) %>% matrix(100, 100, byrow = T)


a <- list(
  visible=FALSE
)

library(plotly)


plot_ly(
  x = d$x,
  y = d$y,
  z = zmat,
  type = 'surface',
  colors = c(scico::scico(13), scico::scico(1e3, palette = 'hawaii'))
) %>%
  hide_colorbar() %>%
  visibly::theme_blank() %>%
  layout(
    plot_bgcolor = 'black',
    paper_bgcolor = 'black',
    scene = list(
      xaxis = a,
      yaxis = a,
      zaxis = a
    )
  )
