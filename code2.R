setwd("/home/agricolamz/for_work/HSE/students/2022_Kuznetsova")
library(tidyverse)
theme_set(theme_bw())

df <- read_tsv("data/log.txt", col_names = FALSE)
colnames(df) <- c("speaker", "vowel", "utterance", "word", "translation", "f0", "f1", "f2", "f3", "intensity", "duration", "step")

df %>% 
  mutate_all(function(i){str_replace(i, "--undefined--", NA_character_)}) %>%
  mutate(across(f0:duration, as.double),
         stressed = ifelse(vowel == toupper(vowel), "stressed", "unstressed"),
         stressed = factor(stressed, levels = c("unstressed", "stressed")),
         vowel_n = str_extract(utterance, "V\\d"),
         vowel_n = as.double(str_remove(vowel_n, "V")),
         utterance = str_extract(utterance, "u\\d"),
         utterance = as.double(str_remove(utterance, "u"))) %>% 
  filter(str_detect(vowel, "[аА]"),
         utterance <= 4) %>% 
  select(-step) %>% 
  group_by(speaker, utterance, stressed, word, vowel_n) %>% 
  summarise(f0 = mean(f0, na.rm = TRUE),
            f1 = mean(f1, na.rm = TRUE),
            f2 = mean(f2, na.rm = TRUE),
            f3 = mean(f3, na.rm = TRUE),
            intensity = mean(intensity, na.rm = TRUE),
            duration = mean(duration, na.rm = TRUE)) %>% 
  mutate(vowel_n = str_c(vowel_n, ". syllable"),
         utterance = str_c(utterance, ". utterance"),
         duration = duration*1000) %>% 
  ungroup() %>% 
  group_by(speaker) %>% 
  mutate(word_pair = as.double(factor(tolower(word)))) %>%
  group_by(word_pair) %>% 
  mutate(word_merged = str_c(unique(str_c(word, "_")), collapse = ""),
         word_merged = str_remove(word_merged, "_$")) %>% 
  ungroup() ->
  mean_values

mean_values %>% 
  select(speaker, utterance, stressed, word_merged, vowel_n, duration) %>% 
  pivot_wider(names_from = stressed, values_from = duration) %>% 
  mutate(duration_differance = stressed-unstressed) %>% 
  ggplot(aes(duration_differance, fill = vowel_n))+
  geom_density(alpha = 0.4)+
  facet_grid(speaker~utterance, scales = "free")

library(brms)
mean_values %>% 
  select(speaker, utterance, stressed, word_merged, vowel_n, duration) %>% 
  brm(stressed ~ duration*vowel_n + (1|speaker) + (1|word_merged/utterance),
      family = bernoulli(),
      data = .) ->
  fit_duration

conditional_effects(fit_duration,
                    effects = c("duration:vowel_n"))

mean_values %>% 
  select(speaker, utterance, stressed, word_merged, vowel_n, f0) %>% 
  brm(stressed~f0*vowel_n + (1|speaker) + (vowel_n|word_merged/utterance),
      family = bernoulli(),
      data = .) ->
  fit_f0

conditional_effects(fit_f0,
                    effects = c("f0:vowel_n"))

mean_values %>% 
  select(speaker, utterance, stressed, word_merged, vowel_n, intensity) %>% 
  brm(stressed~intensity*vowel_n + (1|speaker) + (vowel_n|word_merged/utterance),
      family = bernoulli(),
      data = .) ->
  fit_intensity

conditional_effects(fit_intensity,
                    effects = c("intensity:vowel_n"))

beepr::beep()
