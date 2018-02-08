# Load packages
library(shiny)
library(ggplot2)
library(plotly)

data <- read.table("./data/cereal.tsv", header = TRUE)

# Defines data that is shown based on inputs defined in the UI
# and turns it into an output plot
shinyServer(function(input, output) {
  
  output$scatterPlot <- renderPlotly({
    
    # a darker dot suggests more than one type of cereal fall
    # into that statistics
    ggplot(data, aes_string(input$x.var, input$y.var)) +
      geom_point(alpha = 0.2)
    
  })
})