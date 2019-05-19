# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#use description
library("tidyverse")

## use devtools::load_all() instead
#source("pred_extract.R")


# .onLoad <- function(libname, pkgname) {
#   op <- options()
#   op.devtools <- list(
#     devtools.path = "~/R-dev",
#     devtools.install.args = "",
#     devtools.name = "Your name goes here",
#     devtools.desc.author = "First Last <first.last@example.com> [aut, cre]",
#     devtools.desc.license = "What license is it under?",
#     devtools.desc.suggests = NULL,
#     devtools.desc = list()
#   )
#   toset <- !(names(op.devtools) %in% names(op))
#   if(any(toset)) options(op.devtools[toset])
#
#   invisible()
# }



dogmrtism <- function(df,
                      language = "eng",
                      group = NA,
                      return = c("all","df","plot","table") ) {

  #check if string
  if (!is.character(df$txt)) stop("Error: input is not a string")
  #check if it contains words
  #if (  str_count(df$txt,"\\S+") < 0) stop("Error: no words found in input") or multiple rows?


  df <- predictors_extract(df)


#  return <- c(return)

 # if ((return %in% "all") | (return %in% "plot")) {
#    longdf <- df %>%
#      gather(type, value, close_mind,open_mind)
#
#    if (is.na(group)){
#      longdf %>%
#        ggplot(aes(x = type, y = value, color = value))+
#        geom_boxplot()
#
#    }else{
#      longdf %>%
#        ggplot(aes(x = type, y = value, color = value))+
#        geom_boxplot() +
#        ~facet_wrap(.~group)
#    }
#  }


 # switch(return,
#          all = return(df),
#          df = return(df),
#          plot = return(plot),
#          table = return(NA))
  return(df)
  }


#questions:
#
#load packages?
#return nice object - a table, with a list inside
#documentation how is it written.
#license, which one?

