library(dplyr)
d = data.frame(x=seq(-10, 10, length.out = 100),
               y=seq(-10, 10, length.out = 100)) %>%
  arrange(x)
zmat = apply(expand.grid(d), 1, function(x) levi(x[1], x[2])) %>% matrix(100, 100, byrow = T)


a <- list(
  visible=FALSE
)

library(plotly)
# specular can go to 2, fresnel to 5, others are 0 to 1
# smooth 10015 and default n
# nice on zoom .81.515
plot_ly(x=d$x, y=d$y, z=zmat, 
        type = 'surface', 
        colors =  NineteenEightyR::electronic_night(20), hoverinfo='none',
        lighting = list(ambient=.80,
                        diffuse=1.0,
                        specular=.50,
                        roughness=1.0,
                        fresnel=5.0),
        lightposition = list(x=10,
                             y=10,
                             z=0)
        ) %>% 
  hide_colorbar() %>% 
  lazerhawk::theme_blank() %>% 
  layout(
    plot_bgcolor='black',
         paper_bgcolor='black',
         scene = list(xaxis = a,
                      yaxis = a,
                      zaxis = a
         )
  )






d = data.frame(x=seq(-.13, .13, length.out = 100),
               y=seq(-.13, .13, length.out = 100)) %>%
  arrange(x)
zmat2 = apply(expand.grid(d), 1, function(x) cross_in_tray(x[1], x[2])) %>% matrix(100, 100, byrow = T)


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
  add_surface( z=zmat2) %>%
  hide_colorbar() %>% 
  lazerhawk::theme_blank() %>% 
  layout(plot_bgcolor='black',
         paper_bgcolor='black',
         scene = list(xaxis = a,
                      yaxis = a,
                      zaxis = a
         )
  )
