
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)
library(plotly)
library(shinythemes)
library(leaflet)
options(shiny.sanitize.errors = FALSE)

shinyUI(fluidPage(theme=shinytheme("darkly"),
                  useShinyjs(),
                  #includeCSS("style.css"),
                  
  # Application title #everthing related to head will come here
  headerPanel("LokDhaba"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(tabPanel("GE", tagList(
        uiOutput("ge_uitype_selection"),
        uiOutput("ge_filter_selection")
      ),value="GE"),
                  tabPanel("AE", tagList(
                    uiOutput("state_selection"),
                    uiOutput("ae_uitype_selection"),
                    uiOutput("ae_filter_selection")
                  ),value="AE"),
                  id="electionType"
      )
    ),  # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot"),
      leafletOutput("mapPlot")
    )
  )
))
