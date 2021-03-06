% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dog_cross_val_fun.R
\name{dog_list_return}
\alias{dog_list_return}
\title{Dogmatism cross validation return}
\usage{
dog_list_return(da, vars, outcome_var = "hashtag", above, below, pos,
  n_fold = 10)
}
\arguments{
\item{da}{A data frame}

\item{vars}{a list of variables that will form the predictors in the cross-validation. The will create 1 model for each variable with one predictor. For dogmatism analysis this
will be close-minded and open-minded words}

\item{outcome_var}{the name of column storing the outcome variable. In case of tweets, "hashtag".}

\item{above}{For classification, which class will be assigned for high scores on predictor variables, this is a factor level from "outcome-var".}

\item{below}{The other factor level in the "outcome var"}

\item{pos}{What is counted as a positive in the confusion matrix}

\item{n_fold}{number of folds}
}
\description{
dog_list_return returns the results of the dogmatism cross validation in a ggplot2 friendly format. This function is designed to work with the Twitter data frames used
in the dogmatism analytics Shiny app.
}
