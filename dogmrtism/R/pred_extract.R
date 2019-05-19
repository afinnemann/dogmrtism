library(tidyverse)



#find better solution!
#setwd("~/GitHub/dogmrtism/dogmrtism/data")
data("savedf")


savedf %>%
  mutate(non_dogma = as.character(non_dogma),
         dogma = as.character(dogma)) -> dic

predictors_extract = function(df){


  df = df %>%
    dplyr::mutate(words = str_count(txt,"\\S+"),
                  close_mind = str_count(txt,dic$dogma)/words,
                  open_mind = str_count(txt,dic$non_dogma)/words
                  # certain_A = str_count(txt,certain_A_list)/words,
                  # certain_B = str_count(txt,certain_B_list)/words,
                  # degree_A = str_count(txt,degree_A_list)/words,
                  # degree_B = str_count(txt,degree_B_list)/words,
                  # exclu_A = str_count(txt,exclu_A_list)/words,
                  # exclu_B = str_count(txt,exclu_B_list)/words,
                  # freq_A = str_count(txt,freq_A_list)/words,
                  # freq_B = str_count(txt,freq_B_list)/words,
                  # necess_A = str_count(txt,necess_A_list)/words,
                  # necess_B = str_count(txt,necess_B_list)/words,
                  # quant_A = str_count(txt,quant_A_list)/words,
                  # quant_B = str_count(txt,quant_B_list)/words,
                  # non_dogma_ratio = open_mind / (open_mind + close_mind),
                  # dogma_quotient = ifelse(is.nan(non_dogma_ratio), 1,1 - non_dogma_ratio),
                  # Tentat_ratio = Tentat/(Tentat + Certain),
                  # Certain_quotient = ifelse(is.nan(Tentat_ratio),1,1-Tentat_ratio)
    ) %>%
    dplyr::filter(words != 0)
  return(df)
}

