# https://en.wikipedia.org/wiki/Test_functions_for_optimization#Test_functions_for_multi-objective_optimization

# Himmelblau's function

source('optimization/functions.R')

library(dplyr)
d = data.frame(x=seq(-5, 5, length.out = 100),
               y=seq(-5, 5, length.out = 100)) %>%
  arrange(x)
zmat = apply(expand.grid(d), 1, function(x) himmelblau(x[1], x[2])) %>% matrix(100, 100, byrow = T)


a <- list(
  visible=FALSE
)

library(plotly)

plot_ly(x=d$x, y=d$y, z=zmat, 
        type = 'surface', 
        colors = viridis::viridis(1000), hoverinfo='none') %>% 
  hide_colorbar() %>% 
  lazerhawk::theme_blank() %>% 
  layout(plot_bgcolor='black',
         paper_bgcolor='black',
         scene = list(xaxis = a,
                      yaxis = a,
                      zaxis = a
         )
  )
