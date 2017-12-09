library(ggplot2)
df = data.frame(x=1:50^2, y=sin(pi*1:50^2), color=as.numeric(factor(rainbow(2500))))
ggplot(aes(x=x,y=y),  data=df) +
  geom_point(color=rainbow(2500)) +
  geom_line(color=rainbow(2500)) +
  lazerhawk::theme_trueMinimal()

