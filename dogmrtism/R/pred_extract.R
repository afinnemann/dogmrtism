


data("savedf")

savedf %>%
  mutate(non_dogma = as.character(non_dogma),
         dogma = as.character(dogma)) -> dic

#' helper function for dogmrtism
#'
#'Extracts close-minded, open-minded and total words for a txt file
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
predictors_extract = function(df){


  df = df %>%
    dplyr::mutate(words = str_count(txt,"\\S+"),
                  close_mind = str_count(txt,dic$dogma)/words,
                  open_mind = str_count(txt,dic$non_dogma)/words

    ) %>%
    dplyr::filter(words != 0)
  return(df)
}

