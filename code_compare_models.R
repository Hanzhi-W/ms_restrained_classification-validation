# load packages
library(tidyverse)

# load metrics from csv
ds_30_pos <- read_csv("metrics_restrained_30_pos.csv") %>% 
  mutate(id = ifelse(str_detect(id, "-"), format(as.Date(id, format="%d-%b"),"%m/%d"),id)) %>% 
  arrange(id)

ds_30_no_pos <- read_csv("metrics_restrained_30_no_pos.csv") %>% 
  mutate(id = ifelse(str_detect(id, "-"), format(as.Date(id, format="%d-%b"),"%m/%d"),id)) %>% 
  arrange(id)

ds_4_pos <- read_csv("metrics_restrained_4_pos.csv") %>% 
  mutate(id = ifelse(str_detect(id, "-"), format(as.Date(id, format="%d-%b"),"%m/%d"),id)) %>% 
  arrange(id)

ds_4_no_pos <- read_csv("metrics_restrained_4_no_pos.csv") %>% 
  mutate(id = ifelse(str_detect(id, "-"), format(as.Date(id, format="%d-%b"),"%m/%d"),id)) %>% 
  arrange(id)

# human coded 146 sessions. There are only 139 sessions for 30s models, and 126 sessions for 4s models.
# check which sessions are missing
# load human coding results
ds_human <- read_csv("Restrained human coding results.csv",
                     col_select = c(1,2,6,7))
colnames(ds_human) <- c("subj", "sess", "perc_agr","kappa_agr")
ds_human <- ds_human %>% 
  filter(perc_agr!="/") %>% 
  mutate(id = paste(subj, sess, sep = "/"))

# check missing sessions in 30s models
ds_ms_30 <- anti_join(ds_human, ds_30_pos, by = "id")

# check missing sessions in 4s models
ds_ms_4 <- anti_join(ds_human, ds_4_pos, by = "id")


# merge data frames
ds_merge <- reduce(list(ds_30_pos,ds_30_no_pos,ds_4_pos,ds_4_no_pos), full_join, by = "id")
colnames(ds_merge) <- c("id","ds_30_pos", "ds_30_no_pos", "ds_4_pos","ds_4_no_pos")

# descriptive analysis
ds_long <- ds_merge %>% 
  pivot_longer(cols = "ds_30_pos":"ds_4_no_pos", 
               names_to = "model", values_to = "acc", 
               values_drop_na = T)
ds_long %>% 
  group_by(model) %>% 
  summarize(
    mean=mean(acc,na.rm=T),
    sd=sd(acc,na.rm=T),
    min=min(acc,na.rm=T),
    max=max(acc,na.rm=T),
  )

boxplot(acc ~ model, data = ds_long)

ggplot(data=ds_long, aes(x=factor(model), y=acc))+
  geom_jitter(width=.25)


# t test
# results show (1) there is no difference whether including position or not, for either 30s- or 4s-model.
# (2) 30-window models perform significantly better than 4s-window ones.
t.test(ds_merge$ds_30_pos, ds_merge$ds_30_no_pos, paired = TRUE, alternative = "two.sided")
t.test(ds_merge$ds_4_pos, ds_merge$ds_4_no_pos, paired = TRUE, alternative = "two.sided")

ds_clean <- na.omit(ds_merge)  # Remove rows with any NA
t.test(ds_clean$ds_30_pos, ds_clean$ds_4_pos, paired = TRUE, alternative = "two.sided")
t.test(ds_clean$ds_30_no_pos, ds_clean$ds_4_no_pos, paired = TRUE, alternative = "two.sided")

# check if 30s model performs better in the sessions that 4s data is missing
# results show there is no difference between missing/non-missing sessions for 30s model.
ds_na1 <- ds_merge %>% 
  filter(is.na(ds_4_pos)) %>% 
  select(id, ds_30_pos, ds_30_no_pos)
ds_na0 <- ds_merge %>% 
  filter(!is.na(ds_4_pos)) %>% 
  select(id, ds_30_pos, ds_30_no_pos)
t.test(ds_na0$ds_30_pos, ds_na1$ds_30_pos, paired = F, alternative = "two.sided")
t.test(ds_na0$ds_30_no_pos, ds_na1$ds_30_no_pos, paired = F, alternative = "two.sided")

