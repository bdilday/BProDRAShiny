#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bprodradata)
library(Lahman)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("BPro DRA Explorer"),

  fluidRow(

    column(3,
           selectInput("year", "year", c(2007, 2016), selected = NULL, multiple = FALSE,
                       selectize = TRUE, width = NULL, size = NULL),
           htmlOutput("selectUI")
    ),

    column(8,  plotOutput("distPlot"))
  ),

  fluidRow(
    column(4, h4("Pitcher random effect"),
           tableOutput("pitcher_components")),
    column(8, h4("Average secondary random effects"),
           tableOutput("model_ranef"))
  )
))
