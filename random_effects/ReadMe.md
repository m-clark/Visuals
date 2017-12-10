
These are some visualizations of some covariance structures for random effects and gaussian process models.  By accident I found that plotly kind of does some unusual, but in my opinion quite nifty, things when using a contour plot to visualize a matrix.  The colorscales are from viridis.


### Mixed model covariance structures

- Standard random intercepts
![](model_ri_contour.png)

- An additional random effect
![](model_2re_contour.png)

- A 'nested' random effect
![](model_nest_contour_zoom.png)

- Non-nested
![](model_nonest_contour.png)

  - Zoomed
![](model_nonnest_contour_zoom.png)

- AR1 (also Toeplitz)
![](model_ar_contour.png)

- As a heatmap
![](model_ar_heat.png)

### Gaussian processes

- Squared Exponential Kernel

![](gp_se_2.png)

  - Different settings 
![](gp_se_3_heatmap.png)
  

- Rational Quadratic

![](gp_rq_1_contour.png)

  - Zoomed in as a heatmap
![](gp_rq_1_heat_zoom.png)

  - Different settings
    
![](gp_rq_2_heatmap.png)

- A combination of covariance structures

![](gp_mixed_contour.png)
