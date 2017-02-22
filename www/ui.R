
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

 library(shiny)
 library(shinyjs)
 library(plotly)
 #library(shinythemes)
 library(shinydashboard)
 library(leaflet)
 source("utils/utils-ui.R")
options(shiny.sanitize.errors = FALSE)

shinyUI(navbarPage(title=div(img(src="myassets/logo.png")),windowTitle = "LokDhaba",  useShinyjs(),
                   selected="EDV",
                   tabPanel(tagList(br(),h4("Election Data Visualization")),
                            dashboardPage(dashboardHeader(disable = TRUE),
                                          dashboardSidebar(
                                            tabsetPanel(
                                                         tabPanel("General Elections", tagList(uiOutput("ge_uitype_selection"),
                                                                                uiOutput("ge_filter_selection")
                                                                                ),value="GE"),
                                                         tabPanel("Assembly Elections", tagList(uiOutput("state_selection"),
                                                                                uiOutput("ae_uitype_selection"),
                                                                                uiOutput("ae_filter_selection")
                                                                                ),value="AE"),
                                                         id="electionType"
                                                         ),width=300
                                            ),
                                          dashboardBody(
                                                   plotlyOutput("distPlot",height = 600),
                                                   leafletOutput("mapPlot",height = 600)
                                            )
                                          ),
                            value="EDV"),
                     #  tagList(sidebarLayout(
                     #        sidebarPanel(
                     #          tabsetPanel(
                     #            tabPanel("GE", tagList(uiOutput("ge_uitype_selection"),
                     #                                   uiOutput("ge_filter_selection")
                     #                                   ),value="GE"),
                     #            tabPanel("AE", tagList(uiOutput("state_selection"),
                     #                                   uiOutput("ae_uitype_selection"),
                     #                                   uiOutput("ae_filter_selection")
                     #                                   ),value="AE"),
                     #            id="electionType"
                     #            )
                     #      ),  # Show a plot of the generated distribution
                     #      mainPanel(
                     #        box(title="Plot Area", status="primary", 
                     #            background = "maroon",solidHeader = TRUE,collapsible = TRUE,
                     #            "Note there",br(),
                     #        plotlyOutput("distPlot",height = 400),
                     #        leafletOutput("mapPlot",height = 400))
                     #      )
                     # ))
                     # ),
  navbarMenu(tagList(br(),h4("2017 Assembly Elections")),
              tabPanel(h4("Uttar Pradesh"),
                       dashboardPage(dashboardHeader(disable = TRUE),
                                     dashboardSidebar(useShinyjs()),
                                     dashboardBody(useShinyjs(),
                                        getFixedUIHolders("Uttar Pradesh","UP")              
                                     )
                       ),value="UP"),
                       # tagList(leafletOutput("UP_vis1"),
                       #                         leafletOutput("UP_vis2"),
                       #                         leafletOutput("UP_vis3"),
                       #                         leafletOutput("UP_vis4")
                       #                         ),value="UP"),
              tabPanel(h4("Uttarakhand"),
                       dashboardPage(dashboardHeader(disable = TRUE),
                                     dashboardSidebar(useShinyjs()),
                                     dashboardBody(useShinyjs(),
                                       getFixedUIHolders("Uttarakhand","UK")
                                     )
                       ),value="UK"),
              #          tagList(leafletOutput("UK_vis1"),
              #                                  leafletOutput("UK_vis2"),
              #                                  leafletOutput("UK_vis3"),
              #                                  leafletOutput("UK_vis4")
              # ),value="UK"),
              tabPanel(h4("Punjab"),
                       dashboardPage(dashboardHeader(disable = TRUE),
                                     dashboardSidebar(useShinyjs()),
                                     dashboardBody(useShinyjs(),
                                       getFixedUIHolders("Punjab","PB")
                                       )
                       )
                       # tagList(leafletOutput("PB_vis1"),
                       #                         leafletOutput("PB_vis2"),
                       #                         leafletOutput("PB_vis3"),
                       #                         leafletOutput("PB_vis4")
              ,value="PB")
             )
  )
)
