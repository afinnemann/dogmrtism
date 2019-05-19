

library(pacman)
p_load("stringr","tidyverse", "lubridate","lmerTest")
p_load(boot, caret,pROC, knitr, kableExtra)

setwd("~/UvA_ResMas/programming_next_step/dogmatism_data_analysis/fast_data/dogmatism-master/data")
pol = read.delim("nytimes.txt", header =F ) %>%
  dplyr::rename(txt = V1, dogmatism=V2) %>%
  mutate(txt = as.character(txt)) %>%
  filter(dogmatism <= 9 | dogmatism  >= 13) %>%
  mutate(class = ifelse(dogmatism <= 9, "non_dogmatic","dogmatic"),
         class = as.factor(class),
         post_id = as.factor(1:578))




setwd("~/GitHub/dogmrtism/dogmrtism/R")
source("dogmrtism.R")
df <- dogmrtism(df = pol,return = "all")



head(df)

df %>%
  gather(key,value, close_mind, open_mind) %>%
  ggplot(aes(class, value)) +
  geom_boxplot() +
  facet_wrap(~key)


df %>%
  ggplot(aes(class, close_mind)) +
  geom_boxplot()

dput(head(df))
