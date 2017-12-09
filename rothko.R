library(tidyverse); library(ggplot2); library(lazerhawk); library(scales)


df = data_frame(y = c(seq(-5, -.5, length.out=1000),
                      seq(-.5, .5, length.out=1000),
                      seq(.5, 5, length.out=1000)),
                y2 = c(seq(-5, -.5, length.out=1000) + runif(1000, min=-.01, max=.01),
                      seq(-.5, .5, length.out=1000) + runif(1000, min=-.01, max=.01),
                      seq(.5, 5, length.out=1000) + runif(1000, min=-.01, max=.01)),
                g = rep(1:3, each=1000),
                x0 = 0,
                x1 = -.1,
                x2 = 1)

ggplot(df) +
  # geom_line(aes(x=x0, y, color=g), size=100, show.legend=F) +
  geom_point(aes(x=x1, y2, color=g), size=100, show.legend=F) +
  # geom_hline(aes(yintercept=0), color='#FF8D1E') +
  scale_y_continuous(breaks=c(0),
                     limits=c(-5,5),
                     sec.axis=sec_axis(~.*1, breaks=c(0))) +
  theme_void()



N = 1e4
df = data_frame(y = c(seq(-5, -2, length.out=N),
                      seq(-1.5, 1.5, length.out=N),
                      seq(.5, 5, length.out=N)),
                y2 = c(seq(-5, -.5, length.out=N) + runif(N, min=-.01, max=.01),
                       seq(-.25, .05, length.out=N) + runif(N, min=-.01, max=.01),
                       seq(.15, 5, length.out=N) + runif(N, min=-.01, max=.01)),
                g = factor(rep(1:3, each=N)),
                x1 = -1,
                x2 = 1)

ggplot(df) +
  geom_point(aes(x=.05, y2, color=g), 
             size=1, 
             position=position_jitter(width=.5, height=.1),
             show.legend=F) +
  scale_x_continuous(limits=c(-1,1)) +
  scale_color_manual(values=alpha(palettes$tyrian_purple2$triadic, .5)) +
  lazerhawk::theme_trueMinimal() +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_rect(fill=alpha(palettes$tyrian_purple2$tyrian_purple, .8)),
        plot.background=element_rect(fill=alpha(palettes$tyrian_purple2$tyrian_purple, .5)))




# Rust and blue -----------------------------------------------------------

# grep(colors(), pattern='blue', value=T)
# grep(colors(), pattern='red', value=T)
library(colorspace)
hex(sRGB(92/255,1/255,11/255))

N = 1e4
df = data_frame(y = c(seq(-5, -2, length.out=N),
                      seq(-1.5, 1.5, length.out=N),
                      seq(.5, 5, length.out=N)),
                y2 = c(seq(-5, -1.5, length.out=N) + runif(N, min=-.01, max=.01),
                       seq(-1.5, .75, length.out=N) + runif(N, min=-.01, max=.01),
                       seq(1, 5, length.out=N) + runif(N, min=-.01, max=.01)),
                g1 = factor(rep(1:3, each=N)),
                g2 = factor(rep(1:3, each=N)),
                colvec1 = rep(c('slateblue4','cornflowerblue', 'navy'), each=N),
                x1 = -1,
                x2 = 1)
ggplot(df) +
  geom_point(aes(x=.0, y=y2, color=g1), 
             size=1.5, 
             position=position_jitter(width=.5, height=.1),
             show.legend=F) +
  geom_point(aes(x=.0, y=y2, color=colvec1), 
             size=1.5, 
             position=position_jitter(width=.495, height=.095),
             show.legend=F) +
  geom_point(aes(x=0, y=y2), 
             size=2, 
             color=alpha('#5C010B', .5),
             position=position_jitter(width=.5, height=.25),
             show.legend=F,
             data=filter(df, g1==3)) +
  geom_point(aes(x=-.30, y=y2), 
             size=1, 
             color=alpha('brown4', .05),
             position=position_jitter(width=.15, height=.1),
             show.legend=F,
             data=filter(df, g1==3, y2>1.75, y2<4.5)) +
  geom_point(aes(x=.05, y=y2), 
             size=1, 
             color=alpha('brown4', .05),
             position=position_jitter(width=.125, height=.5),
             show.legend=F,
             data=filter(df, g1==3, y2>1.75, y2<4.5)) +
  geom_point(aes(x=.3, y=y2), 
             size=1, 
             color=alpha('brown4', .025),
             position=position_jitter(width=.1, height=.5),
             show.legend=F,
             data=filter(df, g1==3, y2>1.75, y2<4.5)) +
  # scale_x_continuous(limits=c(-1,1)) +
  scale_color_manual(values=c('slateblue4','cornflowerblue', alpha('#5C010B', .95),
                              alpha('cornflowerblue',.5), alpha('slateblue4', .05), alpha('slateblue4', .5))) +
  lazerhawk::theme_trueMinimal() +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_rect(fill=alpha("royalblue", .9), color=alpha("royalblue", .8)))
