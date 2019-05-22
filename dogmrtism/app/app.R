library(shiny)
library(tidyverse)
library(shinydashboard)
library(dogmRtism)
library(twitteR)
library(ROAuth)
library(instaR)






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

      menuItem("Data input", tabName = "Validation", badgeLabel = "Try me!",
               badgeColor = "blue", icon = icon("pencil-square-o")),

      menuItem("Twitter input", tabName = "Twitter", badgeLabel = "Watch me!",
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
                     tHIS APP DOES DOGMATISM ANALYTICS AND OTHER COOLS.
Dogmaism is contextual, this measure levels of confidence. However, confidence can be more than warrented. I.e. a confidence analysis on
a R tutorial by Hadley Wickham on somethin developed by hadley Wickham, we would hope for low uncertainty!
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
                column(5,
                       radioButtons("show","Show:",
                                    choices = c(political_comments = "pol",
                                                vax_antivax = "vax",
                                                science_communication = "sc",
                                                own_data = "own"),
                                    choiceNames = c("Political news comments",
                                                    "Twiter anti-vax vs vax",
                                                    "Science Communication",
                                                    "Own Data input")
                                      ),
                       fileInput("file1", "Choose CSV File",
                                 multiple = TRUE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")
                                 ),
                       radioButtons("sep", "Separator:",
                                    choices = c(Comma = ",",
                                                Semicolon = ";",
                                                Tab = "\t"
                                                ),
                                    selected = ","
                                    )
                       ),
                column(7,
                       h3("Dogmatism ratio"),
                       plotOutput("dog_plot")
                       )
                )
              ),
      ###### Third page
      tabItem(tabName = "Twitter",
              fluidRow(
                column(5,
                       # textInput("consumer_key", h3("Input consumer_key"),
                       #           value = "Enter text..."),
                       # textInput("consumer_secret", h3("Input consumer_secret"),
                       #           value = "Enter text..."),
                       # textInput("access_token ", h3("Input access_token "),
                       #           value = "Enter text..."),
                       # textInput("access_secret", h3("Input access_secret"),
                       #           value = "Enter text..."),
                       textInput("hashtag", h3("Input hashtag"),
                                 value = "Enter text..."),
                       textInput("hashtag2", h3("Input another hashtag"),
                                 value = "Enter text..."),
                       sliderInput("n_slider", h3("Choose number of tweets"),
                                   min = 0, max = 200, value = 20),
                       actionButton("harvest", "Click to harvest Tweets")


                       ),
                column(7,
                       h3("Dogmatism ratio"),
                       plotOutput("twit_plot")
                )
              )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {



  df <- reactive({

    if (!is.null(input$file1$datapath)){

    df <- read.csv(input$file1$datapath, header = T,sep = input$sep)

    #print(head(df))

    dog_df <- df %>%
      mutate(txt = as.character(txt)) %>%
      dogmrtism()
    }
  })


  output$dog_plot <- renderPlot({

    df() %>%
      gather(key,value, close_mind, open_mind) %>%
      ggplot(aes(class, value)) +
      geom_boxplot() +
      facet_wrap(~key)

  })


  ################# Twitter computations
  #observeEvent(input$harvest, { print("im working")})

  dog_tweetdf <- eventReactive(input$harvest, {


      #setup_twitter_oauth(consumer_key = input$consumer_key,
      #                    consumer_secret = input$consumer_secret,
      #                    access_token = input$acces_token,
      #                    access_secret = input$access_secret)

      hashtag1 <- paste("#", input$hashtag, sep ="")
      hashtag2 <- paste("#", input$hashtag2, sep ="")
      print(hashtag1)
      print(hashtag2)
      print(input$n_slider)


      tweet1  <- searchTwitter(hashtag1, n = input$n_slider, retryOnRateLimit = 120)
      tweet2  <- searchTwitter(hashtag2, n = input$n_slider, retryOnRateLimit = 120)




      tweetdf <- rbind(twListToDF(tweet1),
                       twListToDF(tweet2))


      #colnames to txt
      colnames(tweetdf) <- c("txt",colnames(tweetdf)[-1])

      #adding hashtag
      tweetdf$hashtag <- c(rep(hashtag1, length(tweet1)),
                           rep(hashtag2, length(tweet2)))

      #dogmatism test
      dog_tweetdf <- dogmrtism(tweetdf)



  })

  output$twit_plot <- renderPlot({

       dog_tweetdf() %>%
         mutate(hashtag = as.factor(hashtag)) %>%
          gather(key,value, close_mind, open_mind) %>%
          ggplot(aes(hashtag, value)) +
          geom_boxplot() +
          facet_wrap(~key)

    })



}

# Run the application
shinyApp(ui = ui, server = server)
