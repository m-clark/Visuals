library(ggplot2); library(lazerhawk); library(cowplot)
# run commented part for the heart
# set.seed(8675309)
# N = 10000
# 
# A = matrix(c(2,3,2,1), 2)*.3
# Suni = matrix(runif(2*N, min = -1), ncol = 2)
# Xuni = Suni %*% A
# 
# pc_res = psych::principal(Xuni, 2)$scores
# 
# qplot(pc_res[,1], pc_res[,2], color=I('#FF000005'), size=runif(N), show.legend=F) +
#   lims(x=c(-3,3), y=c(-3,3)) +
#   labs(x='', y='') +
#   scale_x_reverse() +
#   coord_polar(theta = 'x', start=pi, direction = 1) +
#   theme_void()
# 
# ggsave('heart/heart.png')

img1 <- png::readPNG('heart/heart.png')
img2 <- png::readPNG('heart/brat.png')

qplot() +
  lims(x=c(-3,3), y=c(-3,3)) +
  draw_image(img1, x=-1, y=-1, scale=10) +
  draw_image(img2, x=-1, y=-1.65) +
  theme_void()

ggsave('heart/heart_rat.svg')
