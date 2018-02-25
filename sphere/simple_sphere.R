library(plotly)
n <- 500
theta <- runif(n, 0, 2*pi)
u <- runif(n, -1, 1)

a <- list(
  title = '',
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

plot_ly() %>%
  add_markers(x = ~sqrt(1 - u^2) * cos(theta), 
              y = ~sqrt(1 - u^2) * sin(theta), 
              z = ~u, 
              color = I('#ff5500'), 
              opacity=.2, 
              hoverinfo='none') %>% 
  layout(
    title = "",
    scene = list(
      xaxis = a,
      yaxis = a,
      zaxis = a
    )) %>% 
  config(displayModeBar=F) %>% 
  lazerhawk::theme_blank()
