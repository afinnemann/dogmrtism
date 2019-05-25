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




#' Analysis of Dogmatism QUotient
#'
#'This functiosn runs the core dogmatism analysis of text data. It is based on the dogmatism dictionary developed by Suirtbert Ertel.
#'This dictionary contains a list of words related to open-mindedness and close-minded less. The analysis consists of calculating the
#'proportion of these words
#'
#' @param df a dataframe
#' @param col the name of text column to be analysis, must be parsed as a string
#' @param language only english version is implemented, but a german version exists and will be added later
#'
#' @return returns the original df with three addional columns: open-minded, close-minded, n-words.
#' @export
#'
#' @examples
#' dog_df <- dogmrtism(test_df, "txt")
#'

#Change!
library(tidyverse)

dogmrtism <- function(df, #main object df
                      col, #col name w text
                      language = "eng"){#german version not implemented yet

  if (!is.character(df[[col]])) stop("Error: input is not a string")

  #change col name to txt, used in helper function.
  colnames(df)[colnames(df) == col] <- "txt"




  df <- predictors_extract(df)


  #fix the naming! pass column as
  colnames(df)[colnames(df) == "txt"] <- col


  return(df)
  }


#questions:
#
#load packages?
#return nice object - a table, with a list inside
#documentation how is it written.
#license, which one?

