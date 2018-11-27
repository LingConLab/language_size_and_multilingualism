# носители малых языков многоязычнее, чем носители больших.

setwd("/home/agricolamz/work/abstracts/2019_Lion_Daghestan")
library(tidyverse)

options(scipen=999)
theme_set(theme_bw())

read_csv("finall.csv") %>% 
  mutate(residence = if_else(expedition == "Chuni, Tsukhta, Ubekimakhi" & residence == "0", "Ubekimakhi", residence),
         residence = if_else(expedition == "Karata, Tokita, Tad-Magitl', Tlibisho" & residence == "0", "Tad-Magitl'", residence),
         цезский = if_else(residence == "Kidero", 1, 0),
         decade = round(`year of birth`/10)*10) %>% 
  filter(expedition != "Richa, Chirag",
         expedition != "Arkhit, Kug, Laka, Khiv") ->
  df

residence_langs <- read_csv("residence_langs_ND.csv")


df %>% 
  left_join(residence_langs) %>%
  select(-36) %>%  # remove Russian
  mutate(sum_langs = rowSums(.[10:44]),
         sum_langs = if_else(expedition == "Karata, Tokita, Tad-Magitl', Tlibisho", sum_langs-1, sum_langs), # remove Avar
         sum_langs = if_else(expedition == "Hinuq, Kidero, Bezhta", sum_langs-1, sum_langs), # remove Avar
         multilingual = if_else(sum_langs > 1, "multilingual", "monolingual")) %>%
  select(residence, decade, langs, multilingual, sum_langs, status, expedition) %>% 
  count(residence, expedition, status, decade, multilingual) %>% 
  spread(multilingual, n, fill = 0) %>%
  rowwise() %>% 
  mutate(ratio = multilingual/sum(monolingual, multilingual),
         ci_min = binom.test(multilingual, sum(monolingual, multilingual))$conf.int[1],
         ci_max = binom.test(multilingual, sum(monolingual, multilingual))$conf.int[2]) %>% 
  filter(decade <= 1950,
         decade >= 1900,) %>% 
  ggplot(aes(decade, ratio, ymin = ci_min, ymax = ci_max, color = status))+
  geom_linerange()+
  geom_point()+
  facet_wrap(~expedition+residence)
  
df %>% 
  ggplot(aes(lang_type, sum, fill = lang_type))+
  ggpol::geom_boxjitter(alpha = 0.3, outlier.alpha = 0)


df %>% 
  ggplot(aes(`year of birth`, sum))+
  geom_point(alpha = 0.2)+
  facet_wrap(~lang_type)
