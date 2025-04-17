library(tidyverse)
library(timetk)
library(here)
library(rstatix)
library(fastDummies)
library(lme4)
library(lmerTest)
library(interactions)
# i_am(".here")

# preset
theme_update(text = element_text(size = 20),
             axis.text.x = element_text(size = 20, color = "black"), axis.title.x = element_text(size = 24),
             axis.text.y = element_text(size = 20,  color = "black"), axis.title.y = element_text(size = 24), 
             panel.background = element_blank(),panel.border = element_blank(), panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(), axis.line = element_blank(), 
             legend.key = element_rect(fill = "white")) 
# pos_levels <- c("Supine", "Prone", "Sitting", "Upright", "Held")
# pal <-  c("#E69F00","#56B4E9", "#009E73","#F0E442", "#0072B2") %>% 
  # set_names(pos_levels)

# load data
# synced_ppts <- read_csv("/Volumes/padlab/study_imu_long/code/project_status/project_dashboard.csv") %>% 
#   mutate(id_uni = str_glue("{id}/{session}")) %>% 
#   mutate(unique_id = paste0(id,"/",session)) %>% 
#   filter(infant_unr == 2) %>% 
#   filter (id != 104)

# load data from local
load("whole-model-prediction.RData")

# function
read_unrestrained <- function(temp_id) {
  position <- read_csv(str_glue("/Volumes/padlab/study_imu_long/data/{temp_id}/synced_data/restrained_predictions_infant.csv"),
                       show_col_types = FALSE) %>% 
    mutate(unrestrained = factor(pos, levels = c("Unrestrained","Restrained")))
  start_time <- position %>% slice_head %>% pull(time_start)
  end_time <- position %>% slice_tail %>% pull(time_start)
  position <- position %>% add_column(unique_id = temp_id, .before = 1) 
}

ds <- map_dfr(synced_ppts$unique_id, read_unrestrained)

id_info <- synced_ppts %>% select(unique_id, id, agemo, session, sitter, crawler, walker)

ds <- ds %>% left_join(id_info, by = "unique_id") %>% 
  mutate(crawler = factor(crawler, levels = c("Non-Crawler", "Crawler")),
         walker = factor(walker, levels = c("Non-Walker", "Walker")),
         sitter = factor(sitter, levels = c("Non-Sitter","Sitter")),
         age_group = factor(as.numeric(agemo < 9), levels = c(1,0), labels = c("Younger","Older")))

# calculate session-level data, and excluded unavailable time chunk
ds_sum <- ds %>% group_by(unique_id, agemo, walker, crawler, sitter, age_group) %>% 
  filter(exclude_period == 0, nap_period == 0) %>% 
  summarize(unrestrained_n = sum(as.numeric(unrestrained == "Unrestrained")),
            total_n = n()) %>% 
  mutate(unrestrained_prop = unrestrained_n/total_n)

# save the data frames so that don't have to download data from server again
# save(ds, ds_sum, synced_ppts, id_info, file = "whole-model-prediction.RData")

# unmatched/missing sessions
full_file_names <- dir('predict_4_pos', full.names = F)
session_loocv_have <- str_replace(unique(unlist(str_extract_all(full_file_names, "\\d+_\\d+"))), "_", "/")
session_wholemodel_have <- ds_sum$unique_id
setdiff(session_loocv_have, session_wholemodel_have)
setdiff(session_wholemodel_have, session_loocv_have)

# plot to see if any outliers
ggplot(ds_sum, aes(x = age_group, y = unrestrained_prop)) +
  geom_boxplot() + 
  geom_point(position = position_jitter(width = .1), size = 4, color = "red", shape = 1) + 
  scale_y_continuous(name = "Prop(unrestrained)", breaks = seq(0, 1, .25), limits = c(0,1))
# pick out outliers
# 178/3, younger min, 0.04
# 148/2, younger max, 0.85
# 181/3, older min, 0.25
outliers_list_older <- 
  DescTools::Outlier(ds_sum$unrestrained_prop[ds_sum$age_group=="Older"], method = "boxplot")

outliers_list_younger <- 
  DescTools::Outlier(ds_sum$unrestrained_prop[ds_sum$age_group=="Younger"], method = "boxplot")

ds_sum_excl <- 
  ds_sum[!(ds_sum$unrestrained_prop %in% outliers_list_older | ds_sum$unrestrained_prop %in% outliers_list_younger),]

# age-related change
ggplot(ds_sum_excl) +
  geom_point(aes(x = agemo, y = unrestrained_prop)) + 
  geom_smooth(aes(x = agemo, y = unrestrained_prop), method = "lm", se = T) + 
  scale_x_continuous(name = "Age (mo)", breaks = c(4,7,11,14), limits = c(3,15)) +
  scale_y_continuous(name = "Prop(unrestrained)", breaks = seq(0, 1, .25), limits = c(0,1))
summary(lm(unrestrained_prop ~ scale(agemo), data = ds_sum_excl %>% drop_na(crawler)))

ds_sum_excl %>% filter(agemo > 5.5, agemo < 12.5) %>% ggplot() +
  geom_point(aes(x = agemo, y = unrestrained_prop)) + 
  geom_smooth(aes(x = agemo, y = unrestrained_prop), method = "lm", se = T) + 
  scale_x_continuous(name = "Age (mo)", breaks = c(6,7,11,12), limits = c(5,13)) +
  scale_y_continuous(name = "Prop(unrestrained)", breaks = seq(0, 1, .25), limits = c(0,1))
summary(lm(unrestrained_prop ~ scale(agemo), data = ds_sum_excl %>% filter(agemo > 5.5, agemo < 12.5) %>% drop_na(crawler)))

ggplot(ds_sum_excl, aes(x = age_group, y = unrestrained_prop)) +
  geom_boxplot() + 
  geom_point(position = position_jitter(width = .1), size = 4, color = "red", shape = 1) + 
  scale_y_continuous(name = "Prop(unrestrained)", breaks = seq(0, 1, .25), limits = c(0,1))

# sit-related change
ggplot(drop_na(ds_sum_excl, sitter)) +
  geom_point(aes(x = agemo, y = unrestrained_prop, color = sitter)) + 
  geom_smooth(aes(x = agemo, y = unrestrained_prop, color = sitter), method = "lm", se = FALSE) + 
  scale_x_continuous(name = "Age (mo)", breaks = c(4,7,11,14), limits = c(3,15)) +
  scale_y_continuous(name = "Prop(unrestrained)", breaks = seq(0, 1, .25), limits = c(0,1))
summary(lm(unrestrained_prop ~ sitter, data = ds_sum_excl %>% drop_na(sitter)))
summary(lm(unrestrained_prop ~ sitter + scale(agemo), data = ds_sum_excl %>% drop_na(sitter)))

ggplot(drop_na(filter(ds_sum_excl, agemo < 10), sitter)) +
  geom_point(aes(x = agemo, y = unrestrained_prop, color = sitter)) + 
  geom_smooth(aes(x = agemo, y = unrestrained_prop, color = sitter), method = "lm", se = FALSE) + 
  scale_x_continuous(name = "Age (mo)", breaks = 4:7) +
  scale_y_continuous(name = "Prop(unrestrained)", breaks = seq(0, 1, .25), limits = c(0,1))
summary(lm(unrestrained_prop ~ sitter, data = ds_sum_excl %>% filter(agemo < 10) %>% drop_na(sitter)))
summary(lm(unrestrained_prop ~ sitter + scale(agemo), data = ds_sum_excl %>% filter(agemo < 10) %>% drop_na(sitter)))

# crawl-related change
ggplot(drop_na(ds_sum_excl, crawler), aes(x = crawler, y = unrestrained_prop)) +
  geom_boxplot() + 
  geom_point(position = position_jitter(width = .1), size = 4, color = "red", shape = 1) + 
  scale_y_continuous(name = "Prop(unrestrained)", breaks = seq(0, 1, .25), limits = c(0,1))
summary(lm(unrestrained_prop ~ crawler, data = ds_sum_excl %>% drop_na(crawler)))
summary(lm(unrestrained_prop ~ crawler + scale(agemo), data = ds_sum_excl %>% drop_na(crawler)))

ds_sum_excl %>% filter(agemo > 5.5, agemo < 12.5) %>% drop_na(crawler) %>% 
  ggplot(aes(x = crawler, y = unrestrained_prop)) +
  geom_boxplot() + 
  geom_point(position = position_jitter(width = .1), size = 4, color = "red", shape = 1) + 
  scale_y_continuous(name = "Prop(unrestrained)", breaks = seq(0, 1, .25), limits = c(0,1))
summary(lm(unrestrained_prop ~ crawler, data = ds_sum_excl %>% filter(agemo > 5.5, agemo < 12.5) %>% drop_na(crawler)))
summary(lm(unrestrained_prop ~ crawler + scale(agemo), data = ds_sum_excl %>% filter(agemo > 5.5, agemo < 12.5) %>% drop_na(crawler)))

# walk-related change
ggplot(drop_na(ds_sum_excl, walker)) +
  geom_point(aes(x = agemo, y = unrestrained_prop, color = walker)) + 
  geom_smooth(aes(x = agemo, y = unrestrained_prop, color = walker), method = "lm", se = FALSE) + 
  scale_x_continuous(name = "Age (mo)", breaks = c(4,7,11,14), limits = c(3,15)) +
  scale_y_continuous(name = "Prop(unrestrained)", breaks = seq(0, 1, .25), limits = c(0,1))
summary(lm(unrestrained_prop ~ walker, data = ds_sum_excl %>% drop_na(walker)))
summary(lm(unrestrained_prop ~ walker + scale(agemo), data = ds_sum_excl %>% drop_na(walker)))

ggplot(drop_na(filter(ds_sum_excl, agemo > 10), walker)) +
  geom_point(aes(x = agemo, y = unrestrained_prop, color = walker)) + 
  geom_smooth(aes(x = agemo, y = unrestrained_prop, color = walker), method = "lm", se = FALSE) + 
  scale_x_continuous(name = "Age (mo)", breaks = 4:7) +
  scale_y_continuous(name = "Prop(unrestrained)", breaks = seq(0, 1, .25), limits = c(0,1))
summary(lm(unrestrained_prop ~ walker, data = ds_sum_excl %>% filter(agemo > 10) %>% drop_na(walker)))
summary(lm(unrestrained_prop ~ walker + scale(agemo), data = ds_sum_excl %>% filter(agemo > 10) %>% drop_na(walker)))
