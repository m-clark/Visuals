# original
# https://github.com/aschinchon/mandalas/blob/master/mandala.R


# Load in libraries
library(tidyverse)
library(deldir)

mandala <- function(iter=4, points=6, radius=3) {
  # Parameters; you'll likely have to change color options at plotting stage
  # in plot to compensate
  # iter: Number of iterations (depth)
  # points: Number of points
  # radius: Factor of expansion/compression
  
  # Angles of points from center
  angles = seq(0, 2*pi*(1-1/points), length.out = points) + pi/2
  
  # Initial center
  df = data.frame(x = 0, y = 0)
  
  # Iterate over centers again and again
  for (k in 1:iter) {
    temp = data.frame()
    
    for (i in 1:nrow(df)) {
      temp = 
        data.frame(x = df[i, "x"] + radius^(k - 1) * cos(angles),
                   y = df[i, "y"] + radius^(k - 1) * sin(angles)) %>% 
        rbind(temp)
    }
    
    df = temp
  }
  
  # Obtain Voronoi regions and return
  df %>%   
    deldir(sort=TRUE) %>% 
    pluck('dirsgs')
}



# Plot regions with geom_segment
df = mandala(iter = 4, points = 8, radius = .3) 

df %>% 
  ggplot() +
  geom_segment(aes(x = x1, 
                   y = y1, 
                   xend = x2, 
                   yend = y2, 
                   color = factor(x1)),   # factor not needed for viridis
               lwd = .5, 
               show.legend = F) +
  scale_x_continuous(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) +
  # viridis::scale_color_viridis(91, option = 'plasma', discrete = T) +
  scale_color_manual(values=colorRampPalette(NineteenEightyR::malibu(n = 5))(n_distinct(df$x1))) +   
  coord_fixed() +
  lazerhawk::theme_trueMinimal() +
  theme_void()

# ggsave('mandala/4_12_pt3.png', width = 10, height = 10, units = 'cm')
# ggsave('mandala/4_12_pt3.svg', width = 10, height = 10, units = 'cm')
