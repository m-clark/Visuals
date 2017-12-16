library(ggplot2)

y = seq(-5, 0, length.out=100)
x = 0
df = data.frame(x,y)
ggplot(df) +
  geom_segment(aes(x=x, xend=0, y=y, yend=0), size=50, color='dodgerblue') +
  geom_hline(aes(yintercept=0), color='#FF8D1E') +
  scale_y_continuous(breaks=c(0),
                     limits=c(-5,5),
                     sec.axis=sec_axis(~.*1, breaks=c(0))) +
  lazerhawk::theme_trueMinimal() +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())
