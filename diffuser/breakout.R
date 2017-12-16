library(ggplot2); library(viridis)

df = data.frame(x=1:50^2, y=sin(pi*1:50^2), color=as.numeric(factor(rainbow(2500))))

ggplot(aes(x=x,y=y),  data=df) +
  geom_point(color=viridis(2500, option = 'inferno') ) +
  geom_line(color=viridis(2500, option = 'inferno'), alpha=.85) +
  theme_void() +
  theme(plot.background = element_rect(fill='black'), 
        panel.background = element_rect(fill='black', color = 'black'))

ggsave('diffuser/breakout_inferno.png')
