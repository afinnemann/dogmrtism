

#load dictionary files
data("savedf")

#mutate into string format
savedf %>%
  dplyr::mutate(non_dogma = as.character(non_dogma),
         dogma = as.character(dogma)) -> dic

#' helper function for dogmrtism
#'
#'Extracts close-minded, open-minded and total words for a txt file
#'
#' @param df takes a data frame with a column named "txt"
#'
#' @return
#' @export
#'
#' @examples
predictors_extract = function(df){

#extracts predictors
  df = df %>%
    dplyr::mutate(words = stringr::str_count(txt,"\\S+"),
                  close_mind = stringr::str_count(txt,dic$dogma)/words,
                  open_mind = stringr::str_count(txt,dic$non_dogma)/words

    )
  return(df)
}

