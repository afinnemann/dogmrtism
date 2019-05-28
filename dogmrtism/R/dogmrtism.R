#' Analysis of Dogmatism Quotient
#'
#'This functiosn runs the core dogmatism analysis of text data. It is based on the Dogmatism Quotient Dictionary developed by Suitbert Ertel.
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
dogmrtism <- function(df, #main object df
                      col, #col name w text
                      language = "eng"){#german version not implemented yet

  if (!is.data.frame(df)) stop("Error: input is not a data frame")
  if (!is.character(df[[col]])) stop("Error: input is not a string")

  #change col name to txt, used in helper function.
  colnames(df)[colnames(df) == col] <- "txt"

  #extracting open-minded and close-minded scores
  df <- predictors_extract(df)

  #reverting column name
  colnames(df)[colnames(df) == "txt"] <- col


  return(df)
}
