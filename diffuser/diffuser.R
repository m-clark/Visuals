library(ggplot2)
N = 10000
df = data.frame(x=1:N, y=sin(pi*(1:N)^2), color=as.numeric(factor(rainbow(N))))
ggplot(aes(x=x,y=y),  data=df) +
  geom_point(color=rainbow(N), alpha=seq(1,0, length.out=N)) +
  # geom_path(color=rainbow(N)) +
  theme_void()

g = ggplot(aes(x=x,y=y),  data=df) +
  geom_point(color=rainbow(N), alpha=seq(1,0, length.out=N)) +
  # geom_path(color=rainbow(N)) +
  theme_void()


g = ggplot(aes(x=x,y=y),  data=df) +
  theme_void()
# g

for (i in 1:100){
  g = g + geom_point(color=rainbow(1:i), alpha=seq(1,0, length.out=i), data=df[1:i,])
  print(g)
}

library(gganimate)
g = ggplot(aes(x=x,y=y, frame=x),  data=df[1:100,]) +
  theme_void()

gganimate(g)



library(ggplot2)
library(magick)

N = 10000
df0 = data.frame(x=1:N, 
                 y=sin(pi*(1:N)^2), 
                 color=as.numeric(factor(rainbow(N))),
                 splitter = rep(1:10, e=N/10),
                 alpha_seq = seq(.75,0, length.out=N),
                 bow_seq = rainbow(N)
                 )


# N = 1000
dlist = split(df0, df0$splitter)
g = ggplot(aes(x=x,y=y, frame=x),  data=df0) +
  xlim(c(min(df0$x), max(df0$x))) +  # do a separate version without this
  ylim(c(min(df0$y), max(df0$y))) +
  theme_void()

# also do one of just dlist10

img = image_graph(res = 96)
for (i in 1:length(dlist)){
  g = g + 
    # ggplot(aes(x=x,y=y),  data=dlist[[i]]) +
    geom_point(aes(color=I(bow_seq), alpha=I(alpha_seq)),  data=dlist[[i]], show.legend=F)
  print(g)
}
dev.off()

animation <- image_animate(img, fps = 5)
image_write(animation, "diffuser.gif")



dlist = split(df0, df0$splitter)
glr = ggplot(aes(x=x,y=y, frame=x),  data=df0) +
  xlim(c(min(df0$x), max(df0$x))) +  
  ylim(c(min(df0$y), max(df0$y))) +
  theme_void()
grl = ggplot(aes(x=x,y=y, frame=x),  data=df0) +  # nice
  xlim(c(max(df0$x),min(df0$x))) +   
  ylim(c(min(df0$y), max(df0$y))) +
  # scale_x_reverse() +   # don't use you'll lose the fixed min/max
  theme_void()
gbt = ggplot(aes(x=x,y=y, frame=x),  data=df0) +
  xlim(c(min(df0$x), max(df0$x))) +  
  ylim(c(min(df0$y), max(df0$y))) +
  coord_flip() +
  theme_void()
gtb = ggplot(aes(x=x,y=y, frame=x),  data=df0) +
  xlim(c(min(df0$x), max(df0$x))) +  
  ylim(c(max(df0$y), min(df0$y))) +
  # scale_y_reverse() +
  theme_void()



diffuser = function(datalist, gbase, bow_seq, alpha_seq, fps=5, fname=NULL) {
    img = image_graph(res = 96)
    for (i in 1:length(datalist)){
      gbase = gbase + 
        geom_point(aes(color=I(bow_seq), alpha=I(alpha_seq)),  data=datalist[[i]], show.legend=F)
      print(gbase)
    }
    dev.off()
    
    animation <- image_animate(img, fps = fps)
    if(!is.null(fname)) {
      image_write(animation, fname)
    } 
    invisible(g)
}

plr = diffuser(datalist=dlist, gbase=glr, fname='diffuser/diffuser_lr.gif')
prl = diffuser(datalist=rev(dlist), gbase=grl, fname='diffuser/diffuser_rl.gif')


diffuser2p = function(datalist1, datalist2, gbase1, gbase2, bow_seq, alpha_seq, fps=5, fname=NULL) {
  img = image_graph(res = 96)
  for (i in 1:length(datalist1)){
    gbase = gbase1 + 
      geom_point(aes(color=I(bow_seq), alpha=I(alpha_seq)),  data=datalist1[[i]], show.legend=F)
    print(gbase1)
  }
  for (i in 1:length(datalist2)){
    gbase2 = gbase2 + 
      geom_point(aes(color=I(bow_seq), alpha=I(alpha_seq)),  data=datalist2[[i]], show.legend=F)
    print(gbase2)
  }
  dev.off()
  
  animation <- image_animate(img, fps = fps)
  if(!is.null(fname)) {
    image_write(animation, fname)
  } 
  invisible(g)
}


# diffuser pulse start with finished plot and redo