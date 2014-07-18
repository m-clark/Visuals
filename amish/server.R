library(dplyr)
library(maps)
library(scales)

amish <- read.csv('amish.csv') # from data sets repo

CD = amish$ChurchDistricts
amish$pointrange = (CD-min(CD))/(max(CD)-min(CD))*(2-.1) + .1

shinyServer(function(input, output) {
  map('state', col='gray50', lwd=.5, ylim=c(20,50))
  map('world', 'canada', col='gray50', lwd=.5, add=T)
  
  output$map <- renderPlot({
    amishNow <- amish %.%
      filter(YearFounded <= input$year) %.%
      select(YearFounded, ChurchDistricts, city1Lon, city1Lat, pointrange)

    map('state', col='gray50', lwd=.5, ylim=c(20,50))
    map('world', 'canada', col='gray50', lwd=.5, add=T)
    points(amishNow[,c('city1Lon','city1Lat')], col=alpha('#FF5500', .4), pch=19, cex=amishNow$pointrange)
  })
})