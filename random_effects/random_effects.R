# mixed models
library(tidyverse)

rescov <- function(model, data) {
  var.d <- crossprod(getME(model,"Lambdat"))
  Zt <- getME(model,"Zt")
  vr <- sigma(model)^2
  var.b <- vr*(t(Zt) %*% var.d %*% Zt)
  sI <- vr * Diagonal(nrow(data))
  var.y <- var.b + sI
  invisible(var.y)
}

library(lme4)
model_ri = lmer(Reaction ~ Days + (1|Subject), data=sleepstudy)
# summary(gpa_vis_cov)

rc_ri <- rescov(model_ri, sleepstudy)

library(plotly); library(viridis)

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  ticks = '',
  showgrid = FALSE
)

plot_ly(z =~ as.matrix(rc_ri[1:30,1:30]), 
        type='contour', 
        colors = viridis(n=3, option = 'magma'),
        showlegend=F, 
        showscale=F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed")) 

export(file='random_effects/model_ri_contour.png')

model_2re <- lmer(diameter ~ 1 + (1 | sample) + (1 | plate), data = Penicillin)
rc_2re <- rescov(model_2re, Penicillin)


plot_ly(z =~ as.matrix(rc_2re[1:30,1:30]), 
        type='contour', 
        colors = viridis(n=3, option = 'viridis'),
        showlegend=F, 
        showscale=F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed"))

export(file='random_effects/model_2re_contour.png')

plot_ly(z =~ as.matrix(rc_2re[1:30,1:30]), 
        type='heatmap', 
        colors = viridis(n=3, option = 'viridis'),
        showlegend=F, 
        showscale=F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed"))

export(file='random_effects/model_2re_heatmap.png')

plot_ly(z =~ as.matrix(rc_2re[1:12,1:12]), 
        type='contour', 
        colors = viridis(n=3, option = 'viridis'),
        showlegend=F, 
        showscale=F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed")) 

export(file='random_effects/model_2re_contour_zoom.png')


data(Oxide, package = "nlme")

model_nest <- lmer(Thickness ~ 1 + (1|Lot/Wafer), data = Oxide)
rc_nest <- rescov(model_nest, Oxide) %>% as.matrix()

plot_ly(z =~ rc_nest[1:9, 1:9], 
        type='contour', 
        colors = viridis(n=3, option = 'inferno'),
        showlegend=F, 
        showscale=F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed")) 

export(file='random_effects/model_nest_contour_zoom.png')


model_nonest <- lmer(Thickness ~ 1 + (1|Lot) +(1|Wafer), data = Oxide)
rc_nonest <- rescov(model_nonest, Oxide) %>% as.matrix()

plot_ly(z =~ rc_nonest^(1/2), # this is just to bring out the very small off diag element values
        type='contour', 
        colors = viridis(n=n_distinct(rc_nonest), option = 'viridis'),
        showlegend=F, 
        showscale=F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed")) 

export(file='random_effects/model_nonest_contour.png')

plot_ly(z =~ as.matrix(rc_nonest[1:12, 1:12]), 
        type='contour', 
        colors = viridis(n=3, option = 'inferno'),
        showlegend=F, 
        showscale=F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed")) 

export(file='random_effects/model_nonnest_contour_zoom.png')


library(nlme)
model_corAR <- lme(Reaction ~ 1, random= ~1|Subject, data = sleepstudy, correlation = corAR1(form=~Days))
phi = coef(model_corAR$modelStruct$corStruct, unconstrained = F)
init = corMatrix(Initialize(corAR1(phi, form = ~1|Subject), data=sleepstudy))
rc_ar = bdsmatrix::bdsmatrix(rep(10, 18), blocks = rep(init[[1]][lower.tri(init[[1]], diag = T)], 18)) %>% 
  as.matrix()


plot_ly(z =~ rc_ar[1:30, 1:30], 
        type='contour', 
        colors = viridis(n=n_distinct(rc_ar)-1, option = 'inferno'),
        showlegend=F, 
        showscale=F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed")) 
export(file='random_effects/model_ar_contour.png')

plot_ly(z =~ rc_ar[1:30, 1:30], 
        type='heatmap', 
        colors = viridis(n=n_distinct(rc_ar)-1, option = 'inferno'),
        showlegend=F, 
        showscale=F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed")) 

export(file='random_effects/model_ar_heat.png')
