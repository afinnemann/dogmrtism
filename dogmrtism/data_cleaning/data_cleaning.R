library(tidyverse)

library(devtools)
#DOTA predictors
setwd("~/GitHub/dogmrtism/dogmrtism/data_cleaning")
dic = read.csv("dota_dictionary.csv")
dic$word = as.character(dic$word)

dogma = dic %>%
  dplyr::filter(dogmatic == 1)


non_dogma = dic %>%
  dplyr::filter(dogmatic == 0)

lists = lapply(levels(dic$category), function(x){
  dic %>%
    dplyr::filter(category == x) -> temp
  dogma_list = paste(c(temp$word),collapse = "|")
  dogma_list = str_replace_all(dogma_list, "\\| ","|")
  dogma_list = str_replace_all(dogma_list, " \\|","|")

})


for (i in 1:12){
  Object = lists[[i]]

  assign(paste0(levels(dic$category)[i],"_list"), Object)
}

dogma_list = paste(c(dogma$word),collapse = "|")
dogma_list = str_replace_all(dogma_list, "\\| ","|")
dogma_list = str_replace_all(dogma_list, " \\|","|")

non_dogma_list = paste(c(non_dogma$word),collapse = "|")
non_dogma_list = str_replace_all(non_dogma_list, "\\| ","|")
non_dogma_list = str_replace_all(non_dogma_list, " \\|","|")

savedf <- data.frame("dogma" = dogma_list,
                     "non_dogma" = non_dogma_list)

savedf$dogma <- str_replace(savedf$dogma," always", "always")

use_data(savedf,overwrite = T)


#should it be publicly available.
