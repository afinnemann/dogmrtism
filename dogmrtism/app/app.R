library(shiny)
library(tidyverse)
library(shinydashboard)



# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(
    title = "Twitter dogmatism Analytics",
    titleWidth = "100%"),


   ### Sidebar listing 3 pages
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction page", tabName = "theory",
               icon = icon("info-circle")),

      menuItem("Visualization", tabName = "Validation", badgeLabel = "Try me!",
               badgeColor = "blue", icon = icon("pencil-square-o")),

      menuItem("Simulation tests", tabName = "Twitter", badgeLabel = "Watch me!",
               badgeColor = "blue", icon = icon("pencil-square-o"))
    )
  ),

  ## mainbar containging introduction and developer info
  dashboardBody(
    tabItems(
      tabItem(


        #### First page
        tabName = "theory",
        fluidRow(
          column(7,
                 withMathJax(),
                 div(style = "font-size:125%",
                     "
                     tHIS APP DOES DOGMATISM ANALYTICS AND OTHER COOLS

                     ")

          ),
          column(5,

                 box(title=h4("Developed  by:"),
                     solidHeader = TRUE,
                     status = "info",
                     width = '12',

                     h4(img(src="https://i2.wp.com/tquant.eu/wp-content/uploads/2016/08/amsterdam_logo.png?resize=100%2C100&ssl=1", width = "50"),
                        "   Adam Finnemann")
                 )
          )
        )
      ),

      ##### Second Page
      tabItem(tabName = "Validation",
              fluidRow(
                column(12,
                       div(style = "font-size:125%",
                       "yeah so im not done")
                       )

                )
              ),
      ###### Third page
      tabItem(tabName = "Twitter",
              fluidRow(
                column(12,
                       div(style = "font-size:125%",
                       "yeah so im not done")
                       )
              )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


}

# Run the application
shinyApp(ui = ui, server = server)

