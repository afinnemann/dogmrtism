library(shiny)
library(tidyverse)
library(shinydashboard)
library(dogmRtism)
library(twitteR)
library(ROAuth)
library(instaR)
library(plyr)
library(pacman)
library(stringr)

#p_load("stringr","tidyverse", quanteda, "tidytext", sentimentr, MuMIn, psych,  ModelMetrics, caret)
#p_load(boot, caret,pROC,finalfit)

library(kableExtra)


############ R - Twitter authentication


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(
    title = "Twitter dogmatism Analytics", #main header
    titleWidth = "100%"),


  ### creating 3 pages in App
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction page", tabName = "theory", #first page: theory
               icon = icon("info-circle")),

      menuItem("Data input", tabName = "Validation", badgeLabel = "Try me!", #second page: input own data
               badgeColor = "blue", icon = icon("pencil-square-o")),

      menuItem("Twitter input", tabName = "Twitter", badgeLabel = "Watch me!", #third page: scrabe directly from twitter
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
                     " This app allows the user to perform dogmatism analytics of text. Either through own inputs (see page 2), or
directly scrape and analyse Twitter data**. The analysis is based on the dogmRtism package, which utilizes the Dogmatism Quotient Dictionary
developed by Suitbert Ertel in the 1980. Which the dogmRtism package this dictionary has gotten a arrival can now be applied computationally.
The analysis is a simple dictionary match count of words related to open-mindedness and close-mindedness. By calculating this word-use ratio, we
can form impressions of the level of dogmatic thinking. The practical impact of difference in dogmatic ratios is also evaluated.
By providing source information the discriminability between groups is asses using 10 fold-cross validation of logisitc regression models
predicting the source from open-minded and close-mindedness.

(* A Twitter authentication must be done before live Twitter scrabe will work, i.e. the user has to run the setup_twitter_oauth() function with own identifications key, see: http://thinktostart.com/twitter-authentification-with-r/)
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
      tabItem(tabName = "Validation", #this consists of 1 row with two columns
              fluidRow(
                column(6,#column 1: user input
                       ## create button to choose one of 3 default data sets, or input own
                       radioButtons("show","Show:",
                                    choiceNames = c("Political news comments",
                                                    "Own Data input"),
                                    choiceValues = c("pol","own")
                       ),
                       #if own data chosen, this allow user to input csv formatted files
                       fileInput("file1", "Choose CSV File",
                                 multiple = TRUE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")
                       ),
                       #allows user to specify seperator
                       textInput(inputId = "txt_col","Column storing texts",
                                 value = "txt"),

                       radioButtons("sep", "Separator:",
                                    choices = c(Comma = ",",
                                                Semicolon = ";",
                                                Tab = "\t"),
                                    selected = ","),

                       radioButtons(inputId = "split",label = "Split analysis based on column?",
                                    choices = c("yes","no"),
                                    choiceNames = c("yes","no"),
                                    selected = "no"),

                       textInput(inputId = "split_col","Group analysis based on this column",
                                 value = "enter column.")

                ),
                column(6, #column two, visualizing results
                       h3("Dogmatism ratio"),
                       plotOutput("dog_plot")
                )
              ),
              fluidRow(
                column(6,
                       textOutput("dog_cross_text"),
                       uiOutput("dog_image")
                ),
                column(6,
                       h3("Cross validation results"),
                       plotOutput("dog_cross_plot")
                )


              ),
              ###### Third page
              tabItem(tabName = "Twitter",
                      fluidRow(
                        column(5,
                               textInput("hashtag", h3("Input hashtag"),
                                         value = "Enter text..."),
                               textInput("hashtag2", h3("Input another hashtag"),
                                         value = "Enter text..."),
                               sliderInput("n_slider", h3("Choose number of tweets"),
                                           min = 0, max = 2000, value = 200),
                               actionButton("harvest", "Click to harvest Tweets")


                        ),
                        column(7,
                               h3("Dogmatism ratio"), #descriptives of open- and close-minded words
                               plotOutput("twit_plot")
                        )
                      ),
                      fluidRow(
                        column(6,
                               textOutput("cross_text"), #max performance
                               uiOutput("image")
                        ),
                        column(6,
                               h3("Cross validation results"),
                               plotOutput("cross_plot") #plot of cross validation results
                        )
                      )
              )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  ################ updates for page 2: data analysis

  df <- reactive({ #loading exisiting or user's own data, extract dogmatism ratios.


    if (input$show == "pol"){
      df <- read.csv("shiny_pol_test.csv", header = T,sep = input$sep)
      dog_df <- df %>%
        mutate(txt = as.character(txt)) %>%
        dogmrtism("txt")

    }else if(input$show == "own"){
      df <- read.csv(input$file1$datapath, header = T,sep = input$sep)
      #df <- read.csv("r_pyt_test_df2.csv")

      dog_df <- df %>%
        mutate(txt = as.character(txt)) %>%
        dogmrtism(input$txt_col)
    }



  })


  output$dog_plot <- renderPlot({ #plot result
    df <- df()

    if (input$split == "yes") {
      long <- df %>%
        gather(variable,ratio, close_mind, open_mind)

      long %>%
        ggplot(aes(long[,input$split_col], ratio)) +
        geom_boxplot() +
        facet_wrap(~ variable)
    }else{
      df %>%
        gather(variable,ratio, close_mind, open_mind) %>%
        ggplot(aes(variable, ratio)) +
        geom_boxplot()

    }
  })



  #
  dog_cros_output <- reactive({ #plot result

    if (input$split == "yes") {
      df <- df()

      group <- levels(df[,input$split_col])

      cros_val <- df %>%
        dog_list_return(vars = c("close_mind","open_mind"),
                        outcome_var = input$split_col,
                        above = group[1],
                        below = group[2],
                        pos = group[1],
                        n_fold = 10) #return ggplot2 friendly list of cross validation result

    }
  })

  output$dog_cross_plot <- renderPlot({
    if (input$split == "yes") {
      #group <- levels(df[,input$split_col])
      cros_val <- dog_cros_output()

      cros_val %>%
        arrange(desc(mean_auc)) %>%
        dplyr::rename("In_domain" = "mean_auc","In_domainSD" = "sd_auc","predictors" = "var") %>%
        ggplot(aes(x=predictors, y= In_domain, label = round(In_domain,4))) +
        geom_errorbar(aes(ymin=In_domain-In_domainSD, ymax=In_domain+In_domainSD), width=.2, color = "blue") +
        geom_line()+
        geom_point() +
        #labs(title =  paste("Mean AUC (and 1 SD), ",group[1], " vs. ",group[2], sep =""), y = "In domain AUC")+
        geom_text(size = 4,hjust = 1.5) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 10, hjust = 1, size = 13))+
        scale_y_continuous(breaks = seq(0.5,1,0.01))
    }
  })

  output$dog_image <- renderUI({
    if (input$split == "yes") {
      cros_val <- dog_cros_output()

      if (is.null(max(cros_val$mean_auc))){
        NULL
      }else if (max(cros_val$mean_auc) > 0.8){
        setwd("~/GitHub/dogmrtism/dogmrtism/app")
        tags$img(src = "https://memegenerator.net/img/instances/69738487/farewell-my-work-here-is-done.jpg",width = "300", height = "200")
      }else if (max(cros_val$mean_auc) > 0.6){
        setwd("~/GitHub/dogmrtism/dogmrtism/app")
        tags$img(src = "https://memegenerator.net/img/instances/71134675/just-wanted-to-say-good-job.jpg",width = "300", height = "200")

      }else{
        tags$img(src = "https://i.kym-cdn.com/entries/icons/original/000/028/021/work.jpg",width = "300", height = "200")
      }
    }
  })

  output$dog_cross_text <- renderText({
    cros_val <- dog_cros_output()
    paste("Max performance of ",round(max(cros_val$mean_auc),4), "!?", sep = "")
  })





  ################# Twitter computations ######################
  #observeEvent(input$harvest, { print("im working")})

  dog_tweetdf <- eventReactive(input$harvest, {


    #setup_twitter_oauth(consumer_key = input$consumer_key,
    #                    consumer_secret = input$consumer_secret,
    #                    access_token = input$acces_token,
    #                    access_secret = input$access_secret)

    hashtag1 <- paste("#", input$hashtag, sep ="")
    hashtag2 <- paste("#", input$hashtag2, sep ="")



    tweet1  <- searchTwitter(hashtag1, n = input$n_slider, retryOnRateLimit = 120)
    print("first scrape done")
    tweet2  <- searchTwitter(hashtag2, n = input$n_slider, retryOnRateLimit = 120)
    print("second scrape done")

    tweetdf <- rbind(twListToDF(tweet1),
                     twListToDF(tweet2))

    #easy df for debugging
    #hashtag1 <- "R"
    #hashtag2 <- "python"
    #tweet1  <- searchTwitter(hashtag1, n = 800, retryOnRateLimit = 120)
    #tweet2  <- searchTwitter(hashtag2, n = 800, retryOnRateLimit = 120)
    #tweetdf <- rbind(twListToDF(tweet1),twListToDF(tweet2))



    #colnames to txt
    colnames(tweetdf) <- c("txt",colnames(tweetdf)[-1])

    #adding hashtag
    tweetdf$hashtag <- c(rep(hashtag1, length(tweet1)),
                         rep(hashtag2, length(tweet2)))

    tweetdf$txt <- as.character(tweetdf$txt)

    #dogmatism test
    dog_tweetdf <- dogmrtism(tweetdf,"txt")

  })

  output$twit_plot <- renderPlot({


    dog_tweetdf() %>%
      mutate(hashtag = as.factor(hashtag)) %>%
      gather(key,value, close_mind, open_mind) %>%
      ggplot(aes(hashtag, value)) +
      geom_boxplot() +
      facet_wrap(~key)

  })


  cross_output <- reactive({

    dog_tweetdf <- dog_tweetdf()

    dog_tweetdf$hashtag <- as.factor(dog_tweetdf$hashtag)

    cros_val <- dog_tweetdf %>%
      dog_list_return(vars = c("close_mind","open_mind"),
                      outcome_var = "hashtag",
                      above = paste("#", input$hashtag, sep =""),
                      below = paste("#", input$hashtag2, sep =""),
                      pos = paste("#", input$hashtag, sep =""),
                      n_fold = 10) #return ggplot2 friendly list of cross validation result
  })

  output$cross_plot <- renderPlot({

    cross_output() %>%
      arrange(desc(mean_auc)) %>%
      dplyr::rename("In_domain" = "mean_auc","In_domainSD" = "sd_auc","predictors" = "var") %>%
      ggplot(aes(x=predictors, y= In_domain, label = round(In_domain,4))) +
      geom_errorbar(aes(ymin=In_domain-In_domainSD, ymax=In_domain+In_domainSD), width=.2, color = "blue") +
      geom_line()+
      geom_point() +
      labs(title =  paste("Mean AUC (and 1 SD), ",input$hashtag, " vs. ",input$hashtag2, sep =""), y = "In domain AUC")+
      geom_text(size = 4,hjust = 1.5) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 10, hjust = 1, size = 13))+
      scale_y_continuous(breaks = seq(0.5,1,0.01))
  })

  output$image <- renderUI({
    cros_val <- cross_output()

    if (is.null(max(cros_val$mean_auc))){
      NULL
    }else if (max(cros_val$mean_auc) > 0.8){
      setwd("~/GitHub/dogmrtism/dogmrtism/app")
      tags$img(src = "https://memegenerator.net/img/instances/69738487/farewell-my-work-here-is-done.jpg",width = "300", height = "200")
    }else if (max(cros_val$mean_auc) > 0.6){
      setwd("~/GitHub/dogmrtism/dogmrtism/app")
      tags$img(src = "https://memegenerator.net/img/instances/71134675/just-wanted-to-say-good-job.jpg",width = "300", height = "200")

    }else{
      tags$img(src = "https://i.kym-cdn.com/entries/icons/original/000/028/021/work.jpg",width = "300", height = "200")
    }
  })

  output$cross_text <- renderText({
    cros_val <- cross_output()
    paste("Max performance of ",round(max(cros_val$mean_auc),4), "!?", sep = "")
  })


}

# Run the application
shinyApp(ui = ui, server = server)

