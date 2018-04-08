# https://en.wikipedia.org/wiki/Test_functions_for_optimization#Test_functions_for_multi-objective_optimization

# Cross in tray

source('optimization/functions.R')

library(dplyr)
d = data.frame(x=seq(-10, 10, length.out = 100),
               y=seq(-10, 10, length.out = 100)) %>%
  arrange(x)
zmat = apply(expand.grid(d), 1, function(x) cross_in_tray(x[1], x[2])) %>% matrix(100, 100, byrow = T)


a <- list(
  visible=FALSE
)

library(plotly)

plot_ly(x=d$x, y=d$y, z=zmat, 
        type = 'surface', 
        colors = viridis::inferno(1000)) %>% 
  hide_colorbar() %>% 
  lazerhawk::theme_blank() %>% 
  layout(plot_bgcolor='black',
         paper_bgcolor='black',
         scene = list(xaxis = a,
                      yaxis = a,
                      zaxis = a
         )
  )

# try with domain -1,1 for x and y