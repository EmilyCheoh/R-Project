# Load packages
library(shiny)
library(plotly)

# my UI function
shinyUI(fluidPage(
  
  titlePanel("Cereal Information"),
  
  sidebarLayout(
    
    sidebarPanel(
      # side panel 1 that can change the x variable, 
      # the default value is "calories"
      selectInput(inputId = "x.var",
                  label = "x variable",
                  choices = c("calories", "protein", "fat", "sodium", "carbo", "sugars", "potass", "vitamins", 
                              "shelf", "weight", "cups", "rating"),
                  selected = "calories"),
      # side panel 2 that can change the y variable,
      # the default value is "fat"
      selectInput(inputId = "y.var",
                  label = "y variable",
                  choices = c("calories", "protein", "fat", "sodium", "carbo", "sugars", "potass", "vitamins", 
                              "shelf", "weight", "cups", "rating"),
                  selected = "fat"),
      
      "A darker the dot suggests there is an overlap of different brands of cereals that share the same statistics 
      (e.g. same calories and fat)."
    ),
    
    mainPanel(
      plotlyOutput('scatterPlot')
    )
  )
  
))