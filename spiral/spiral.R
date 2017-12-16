library(ggplot2); library(grid); library(viridis)


densplot = function() {
  x=rnorm(10)
  col = sample(viridis(1000), 1)
  # x = sample(x, 1000, replace=T)
  ggplot(data.frame(x=x, g=1, dens=dnorm(x))) +
    stat_density(aes(x=x), geom='area', fill=col, alpha=.25, color=col) +
    geom_hline(yintercept=0, colour="white", size=.01, alpha=1) +
    scale_x_continuous(limits=c(-5,5)) +
    theme_void()
}
densplot()


spiral = function(X, Y) {
  x = y = 0
  dx = 0
  dy = -1
  for (i in 0:max(X, Y)^2) {
    
    if ( (x > -X/2 & x <= X/2) & (y > -Y/2 & y <= Y/2) ) {
      print(c(x, y))
    }
    
    if ( (x == y) | (x < 0 & x == -y) | (x > 0 & x == 1-y) ) {
      out = c(-dy, dx)
    }
    
    dx = out[1]
    dy = out[2]
    
    x = x + dx 
    y = y + dy  
  }
}

# debugonce(spiral)
spiral(3,3)
spiral(5,3)


vplayout <- function(x, y) {
  viewport(layout.pos.row = x, 
           layout.pos.col = y)
}

# ,
#                                     angle = sample(1:360, 1))



spiral_plot = function(X, Y, randomWidths=F, randomHeights=F) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(X, Y, 
                                             widths=ifelse(randomWidths, 
                                                          unit(runif(X), "mm"),
                                                          unit(rep_len(1, X), "null")),
                                             heights=ifelse(randomHeights, 
                                                           unit(runif(X), "mm"),
                                                           unit(rep_len(1, Y), "null")))
                        ))
  x = y = 0
  dx = 0
  dy = -1
  origin = c(median(1:X), median(1:Y))
  for (i in 0:max(X, Y)^2) {
    if ( (x > -X/2 & x <= X/2) & (y > -Y/2 & y <= Y/2) ) {
      current_position = origin + c(x, y)
      p = densplot()
      print(p, vp = vplayout(current_position[1], current_position[2]))
      
    }
    if ( (x == y) | (x < 0 & x == -y) | (x > 0 & x == 1-y) ) {
      out = c(-dy, dx)
    }
    dx = out[1]
    dy = out[2]
    x = x + dx 
    y = y + dy  
  }
}

# debugonce(spiral_plot)
spiral_plot(3,3)
spiral_plot(3,3, randomHeights=T, randomWidths=T)
spiral_plot(4,4)
spiral_plot(5,5)
