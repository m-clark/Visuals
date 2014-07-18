# ui.R

amish <- read.csv('amish.csv') # from data sets repo


shinyUI(fluidPage(
  titlePanel("Amish Settlements in the US"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId="year", label="Year Founded:", min = min(amish$YearFounded), 
                  max = max(amish$YearFounded),
                  value=min(amish$YearFounded), step = 1, 
                  format='####',
                  animate=animationOptions(interval=200, loop=FALSE)
                  )
      ),
    
    mainPanel(plotOutput("map"))
    )
  )
  )