library(tidyverse); library(plotly); library(viridis)

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  ticks = '',
  showgrid = FALSE
)



# Squared exponential -----------------------------------------------------



# The covariance function; here it is the squared exponential kernel.
# l is the horizontal scale, sigmaf is the vertical scale, sigman the noise.
# See ?covSEiso in the gpr package for example, which is also based on Rasmussen and
# Williams Matlab code (gpml Matlab library)

Kfn_se = function(x, l=1, sigmaf=1, sigman=.5){
    sigmaf * exp( -(1/(2*l^2)) * as.matrix(dist(x, upper=T, diag=T)^2) )
}


l = 1          
sigmaf = 1      
sigman = .25 

x = seq(-5, 5, .2)

Sigma = Kfn_se(x, l=l, sigmaf=sigmaf, sigman=sigman)

plot_ly(z =~ Sigma, 
        type='contour', 
        colors = viridis(n=3, option = 'inferno'),
        showlegend=F, 
        showscale=F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed")) 

export(file='random_effects/gp_se_1.png')

plot_ly(z =~ Sigma, 
        type='contour', 
        colors = viridis(n=n_distinct(Sigma)-1, option = 'inferno'),
        showlegend=F, 
        showscale=F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed")) 

export(file='random_effects/gp_se_2.png')


l = .5           # for l, sigmaf, sigman, see note at covariance function
sigmaf = 1
sigman = .25 
Sigma = Kfn_se(x, l=l, sigmaf=sigmaf, sigman=sigman)

plot_ly(z =~ Sigma, 
        type='contour', 
        colors = viridis(n=n_distinct(Sigma)-1, option = 'inferno'),
        showlegend=F, 
        showscale=F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed")) 

export(file='random_effects/gp_se_3_contour.png')

plot_ly(z =~ Sigma, 
        type='heatmap', 
        colors = viridis(n=n_distinct(Sigma)-1, option = 'inferno'),
        showlegend=F, 
        showscale=F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed")) 

export(file='random_effects/gp_se_3_heatmap.png')


l = 5           # for l, sigmaf, sigman, see note at covariance function
sigmaf = 1
sigman = .25 
Sigma = Kfn_se(x, l=l, sigmaf=sigmaf, sigman=sigman)

plot_ly(z =~ Sigma, 
        type='contour', 
        colors = viridis(n=n_distinct(Sigma)-1, option = 'inferno'),
        showlegend=F, 
        showscale=F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed")) 

export(file='random_effects/gp_se_4.png')

# Rational Quadratic ------------------------------------------------------

Kfn_rq <- function(x, alpha=.5, l=1, eta_sq=1, sigma_sq=1e-3) {
  
  N = length(x)
  Sigma = matrix(0, N, N)
  # off-diagonal elements for covariance matrix
  
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      Sigma[i,j] <- eta_sq * (1 + (1/(2*alpha)) * ((x[i] - x[j])/l)^2)^-alpha
      Sigma[j,i] <- Sigma[i,j]
    }
  }
  
  # diagonal elements
  diag(Sigma) <- eta_sq + sigma_sq # + jitter for pos def
  Sigma
}

alpha=.5
l=1
eta_sq=1
sigma_sq=.5

Sigma = Kfn_rq(x, alpha, l, eta_sq, sigma_sq)

plot_ly(z =~ Sigma, 
        type='contour', 
        colors = viridis(n=n_distinct(Sigma)-1, option = 'inferno'),
        showlegend=F, 
        showscale=F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed")) 

export(file='random_effects/gp_rq_1_contour.png')

plot_ly(z =~ Sigma, 
        type='heatmap', 
        colors = viridis(n=n_distinct(Sigma)-1, option = 'inferno'),
        showlegend=F, 
        showscale=F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed"))

export(file='random_effects/gp_rq_1_heat.png')

plot_ly(z =~ Sigma[1:10, 1:10], 
        type='heatmap', 
        colors = viridis(n=n_distinct(Sigma)-1, option = 'inferno'),
        showlegend=F, 
        showscale=F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed"))

export(file='random_effects/gp_rq_1_heat_zoom.png')

alpha = 1
l = 2.5
eta_sq = 1
sigma_sq = 0

Sigma = Kfn_rq(x, alpha, l, eta_sq, sigma_sq)

# plot_ly(z =~ Sigma,
#         type='contour',
#         colors = viridis(n=n_distinct(Sigma)-1, option = 'inferno'),
#         showlegend=F,
#         showscale=F) %>%
#   layout(xaxis = ax,
#          yaxis = c(ax, autorange = "reversed"))

plot_ly(z =~ Sigma,
        type = 'heatmap',
        colors = viridis(n = n_distinct(Sigma) - 1, option = 'inferno'),
        showlegend = F,
        showscale = F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed")) 

export(file='random_effects/gp_rq_2_heatmap.png')


# plot_ly(z =~ Sigma[1:10, 1:10], 
#         type='heatmap', 
#         colors = viridis(n=n_distinct(Sigma)-1, option = 'inferno'),
#         showlegend=F, 
#         showscale=F) %>%
#   layout(xaxis = ax,
#          yaxis = c(ax, autorange = "reversed")) 



# Factor Analytic ---------------------------------------------------------

Kfn_fa <- function(x, l=1, eta_sq=1, sigma_sq=.5, L) {
  N = length(x)
  Sigma = matrix(0, N, N)  
  nobs_within_clus = nrow(L)
  M = tcrossprod(L) + diag(l^-2, nobs_within_clus)
  # M = L %*% diag(2) %*% t(L) + diag(l^-2, nobs_within_clus)

  x_dist = as.matrix(dist(x, diag = T, upper = T))
  
  idx1 = seq(1, N, by = nobs_within_clus)
  idx2 = seq(nobs_within_clus, N, by = nobs_within_clus)
  
  for (i in 1:length(idx1)) {
    idx = idx1[i]:idx2[i]
    Sigma[idx, idx] = eta_sq * exp(-1/2 * t(x_dist[idx, idx]) %*% M %*% x_dist[idx, idx])
  }

  # diagonal elements
  diag(Sigma) = eta_sq + sigma_sq
  
  Sigma
}

x = seq(-5, 5, length.out = 60)
x = rep(c(-3,-2,-1,1,2,3), 10)

loads = matrix(c(.9,.9,.9,0,0,0,0,0,0,.3,.3,.3), ncol = 2)
loads = matrix(c(1,1,1,0,0,0,0,0,0,1,1,1), ncol = 2)
# loads = matrix(c(1,1,1,-1,-1,-1,-1,-1,-1,1,1,1), ncol = 2)

debugonce(Kfn_fa)
Sigma = Kfn_fa(x, L=loads, l = 1, eta_sq = 1,  sigma_sq =1)

plot_ly(z =~ Sigma,
        type = 'heatmap',
        colors = viridis(n = n_distinct(Sigma) - 1, option = 'inferno'),
        showlegend = F,
        showscale = F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed")) 

export(file='random_effects/gp_fa_1_heatmap.png')



Sigma = Kfn_fa(x, L=loads, l = 2, eta_sq = .5,  sigma_sq = .5) +
  Kfn_se(x, l=2, sigmaf=1, sigman=.5) +
  Kfn_rq(x, alpha=1, l=2.5, eta_sq=1, sigma_sq=0)

plot_ly(z =~ Sigma,
        type = 'heatmap',
        colors = viridis(n = n_distinct(Sigma) - 1, option = 'plasma'),
        showlegend = F,
        showscale = F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed")) 
export(file='random_effects/gp_mixed_heatmap.png')


plot_ly(z =~ Sigma,
        type = 'contour',
        colors = viridis(n = n_distinct(Sigma) - 1, option = 'plasma'),
        showlegend = F,
        showscale = F) %>%
  layout(xaxis = ax,
         yaxis = c(ax, autorange = "reversed")) 

export(file='random_effects/gp_mixed_contour.png')
