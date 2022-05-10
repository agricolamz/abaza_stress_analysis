setwd("/home/agricolamz/for_work/HSE/students/2022_Kuznetsova")
# load packages
library(tidyverse)
library(phonfieldwork)
theme_set(theme_bw())

# read data
read_from_folder("data/", type = "textgrid") %>% 
  filter(content != "") ->
  textgrids

textgrids %>% 
  filter(tier_name %in% c("labels", "translation")) %>% 
  select(-tier) %>% 
  pivot_wider(values_from = content, names_from = tier_name) %>% 
  mutate(id_label = 1:n()) %>% 
  group_by(source) %>% 
  mutate(pair_group = as.double(factor(tolower(labels)))) %>%
  group_by(source, pair_group) %>% 
  mutate(pair = as.double(factor(labels))) %>% 
  ungroup() ->
  labels

textgrids %>% 
  filter(tier_name %in% c("utterance", "vowel")) %>% 
  select(-tier) %>% 
  pivot_wider(values_from = content, names_from = tier_name) %>% 
  mutate(duration = time_end - time_start,
         utterance_n = str_extract(utterance, "u\\d"),
         utterance_n = str_remove(utterance_n, "u"),
         vowel_n = str_extract(utterance, "V\\d"),
         vowel_n = str_remove(vowel_n, "V"),
         stressed = vowel == toupper(vowel),
         vowel = tolower(vowel)) ->
  annotations

map(labels$id_label, function(i){
  annotations %>% 
    filter(source == labels$source[i],
           time_start >= labels$time_start[i],
           time_end <= labels$time_end[i]) %>%
    mutate(id_label = labels$id_label[i]) %>% 
    pull(id_label)
}) %>% 
  unlist() ->
  annotations$id_label

annotations %>% 
  select(-time_start, -time_end, -id) %>% 
  left_join(labels) %>% 
  select(vowel, utterance_n, vowel_n, pair, source, pair_group, duration) %>% 
  mutate(vowel = ifelse(is.na(vowel), "а", vowel),
         pair = str_c("duration_", pair)) %>% 
  pivot_wider(names_from = pair, values_from = duration) ->
  duration_df

annotations %>% 
  select(-time_start, -time_end, -id) %>% 
  left_join(labels) %>% 
  select(vowel,  utterance_n, vowel_n, pair, source, pair_group, stressed) %>% 
  mutate(vowel = ifelse(is.na(vowel), "а", vowel),
         pair = str_c("stressed_", pair)) %>% 
  pivot_wider(names_from = pair, values_from = stressed) ->
  stress_df

annotations %>% 
  select(-time_start, -time_end, -id) %>% 
  left_join(labels) %>% 
  select(vowel,  utterance_n, vowel_n, pair, source, pair_group, labels) %>% 
  mutate(vowel = ifelse(is.na(vowel), "а", vowel),
         pair = str_c("word_", pair)) %>% 
  pivot_wider(names_from = pair, values_from = labels) ->
  labels_df

duration_df %>% 
  bind_cols(stress_df[,c("stressed_1", "stressed_2")],
            labels_df[,c("word_1", "word_2")]) %>% 
  write_csv("intermediate_result.csv")

# analysis ----------------------------------------------------------------
df <- read_csv("intermediate_result.csv")

df %>% 
  mutate(words = str_c(word_1, "/", word_2),
         segment_id = 1:n()) %>%
  select(vowel, utterance_n, vowel_n, source, pair_group, words, duration_1, duration_2) %>% 
  pivot_longer(names_to = "duration_id", values_to = "duration", duration_1:duration_2) %>% 
  select(-duration_id) ->
  df_values

df %>% 
  mutate(words = str_c(word_1, "/", word_2),
         segment_id = 1:n()) %>%
  select(vowel, utterance_n, vowel_n, source, pair_group, words, stressed_1, stressed_2) %>% 
  pivot_longer(names_to = "duration_id", values_to = "stress", stressed_1:stressed_2) %>% 
  select(-duration_id) %>% 
  bind_cols(df_values[,"duration"]) %>% 
  filter(vowel == "а",
         utterance_n <= 4) %>% 
  mutate(vowel_n = str_c(vowel_n, ". syllable"),
         utterance_n = str_c(utterance_n, ". utterance"),
         stress = ifelse(stress, "stressed", "unstressed"),
         duration = duration*1000) %>% 
  na.omit() ->
  for_analysis

for_analysis %>%   
  ggplot(aes(stress, duration))+
  geom_violin()+
  facet_grid(vowel_n~utterance_n)+
  labs(x = "", y = "vowel duration (in ms)")

library(brms)
fit <- brm(duration~stress*vowel_n+(1|source)+ (1|words/utterance_n),
           data = for_analysis)

conditional_effects(fit, effects = c("vowel_n:stress"))
