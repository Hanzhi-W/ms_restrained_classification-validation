# load packages
library(tidyverse)
library(dplyr)
library(here)

# update theme
theme_update(text = element_text(size = 14),
             axis.text.x = element_text(size = 14, color = "black"),
             axis.title.x = element_text(size = 16),
             axis.text.y = element_text(size = 14,  color = "black"),
             axis.title.y = element_text(size = 16),
             panel.background = element_blank(),panel.border = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(), axis.line = element_blank(),
             legend.key = element_rect(fill = "white"))

# load metrics from csv
ds_30_pos <- read_csv("metrics_restrained_30_pos.csv") %>% 
  mutate(id = ifelse(str_detect(id, "-"), format(as.Date(id, format="%d-%b"),"%m/%d"),id)) %>% 
  arrange(id) %>% 
  rename(id=id, acc=overall_acc, sens=overall_sens, spec=overall_spec, posi=overall_posi_pred) %>% 
  mutate(sens = 1-sens, spec = 1-spec, posi = 1-posi)

ds_30_no_pos <- read_csv("metrics_restrained_30_no_pos.csv") %>% 
  mutate(id = ifelse(str_detect(id, "-"), format(as.Date(id, format="%d-%b"),"%m/%d"),id)) %>% 
  arrange(id)%>% 
  rename(id=id, acc=overall_acc, sens=overall_sens, spec=overall_spec, posi=overall_posi_pred) %>% 
  mutate(sens = 1-sens, spec = 1-spec, posi = 1-posi)

ds_4_pos <- read_csv("metrics_restrained_4_pos.csv") %>% 
  mutate(id = ifelse(str_detect(id, "-"), format(as.Date(id, format="%d-%b"),"%m/%d"),id)) %>% 
  arrange(id)%>% 
  rename(id=id, acc=overall_acc, sens=overall_sens, spec=overall_spec, posi=overall_posi_pred) %>% 
  mutate(sens = 1-sens, spec = 1-spec, posi = 1-posi)

ds_4_no_pos <- read_csv("metrics_restrained_4_no_pos.csv") %>% 
  mutate(id = ifelse(str_detect(id, "-"), format(as.Date(id, format="%d-%b"),"%m/%d"),id)) %>% 
  arrange(id)%>% 
  rename(id=id, acc=overall_acc, sens=overall_sens, spec=overall_spec, posi=overall_posi_pred) %>% 
  mutate(sens = 1-sens, spec = 1-spec, posi = 1-posi)

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
colnames(ds_merge) <- c("id","acc_30_pos", "sens_30_pos", "spec_30_pos", "posi_30_pos",
                        "acc_30_no_pos", "sens_30_no_pos", "spec_30_no_pos", "posi_30_no_pos",
                        "acc_4_pos", "sens_4_pos", "spec_4_pos", "posi_4_pos",
                        "acc_4_no_pos", "sens_4_no_pos", "spec_4_no_pos", "posi_4_no_pos")

ds_long <- bind_rows(list(ds30pos = ds_30_pos, ds30nopos = ds_30_no_pos, ds4pos = ds_4_pos, ds4nopos = ds_4_no_pos),
                     .id = "model") %>% 
  mutate(model = factor(model, levels=c("ds30pos","ds30nopos","ds4pos","ds4nopos"),
                        labels = c("ds30pos","ds30nopos","ds4pos","ds4nopos")))

ds_long2 <- ds_long %>% 
  pivot_longer(acc:posi,names_to="ind",values_to="value")

# descriptive analysis
ds_long2 %>% 
  group_by(model,ind) %>% 
  summarize(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE))

# t test
# accuracy
t.test(ds_merge$acc_30_pos, ds_merge$acc_30_no_pos, paired = TRUE, alternative = "two.sided", na.action = na.omit)
t.test(ds_merge$acc_4_pos, ds_merge$acc_4_no_pos, paired = TRUE, alternative = "two.sided", na.action = na.omit)
t.test(ds_merge$acc_30_pos, ds_merge$acc_4_pos, paired = TRUE, alternative = "two.sided", na.action = na.omit)
t.test(ds_merge$acc_30_no_pos, ds_merge$acc_4_no_pos, paired = TRUE, alternative = "two.sided", na.action = na.omit)
# sensitivity
t.test(ds_merge$sens_30_pos, ds_merge$sens_30_no_pos, paired = TRUE, alternative = "two.sided", na.action = na.omit)
t.test(ds_merge$sens_4_pos, ds_merge$sens_4_no_pos, paired = TRUE, alternative = "two.sided", na.action = na.omit)
t.test(ds_merge$sens_30_pos, ds_merge$sens_4_pos, paired = TRUE, alternative = "two.sided", na.action = na.omit)
t.test(ds_merge$sens_30_no_pos, ds_merge$sens_4_no_pos, paired = TRUE, alternative = "two.sided", na.action = na.omit)
# specificity
t.test(ds_merge$spec_30_pos, ds_merge$spec_30_no_pos, paired = TRUE, alternative = "two.sided", na.action = na.omit)
t.test(ds_merge$spec_4_pos, ds_merge$spec_4_no_pos, paired = TRUE, alternative = "two.sided", na.action = na.omit)
t.test(ds_merge$spec_30_pos, ds_merge$spec_4_pos, paired = TRUE, alternative = "two.sided", na.action = na.omit)
t.test(ds_merge$spec_30_no_pos, ds_merge$spec_4_no_pos, paired = TRUE, alternative = "two.sided", na.action = na.omit)
# positive predictive value
t.test(ds_merge$posi_30_pos, ds_merge$posi_30_no_pos, paired = TRUE, alternative = "two.sided", na.action = na.omit)
t.test(ds_merge$posi_4_pos, ds_merge$posi_4_no_pos, paired = TRUE, alternative = "two.sided", na.action = na.omit)
t.test(ds_merge$posi_30_pos, ds_merge$posi_4_pos, paired = TRUE, alternative = "two.sided", na.action = na.omit)
t.test(ds_merge$posi_30_no_pos, ds_merge$posi_4_no_pos, paired = TRUE, alternative = "two.sided", na.action = na.omit)

# ANOVA
# Should I use ANOVA to compare all models together?
# But I'm not equally interested in all paired comparisons.
# summary(aov(acc ~ model, data = ds_long))


# Plot
# scatter plot with line connecting same session

facet <- data_frame(id = ds_merge$id,facet = rep(1:6,length.out = nrow(ds_merge)))

ds_long <- left_join(ds_long, facet, by="id")

ggplot(data=ds_long, aes(x=model, y=acc, group=id, color=id))+
  geom_point()+
  geom_line()+
  geom_text(data = ds_long %>% filter(model=="ds4nopos"),
            aes(label = id), hjust = -0.2, size=1.5)+
  theme_update() +
  labs(x = "model", y = "acc") +
  theme(legend.position = "none")+
  facet_wrap(~ facet)
# ggsave("figure/acc_across_models.eps", width = 15, height = 20)


# for (ind in c("acc","sens","spec","posi")){
#   plot <- ggplot(data=ds_long, 
#                  aes_string(x="model", y=ind, group="id", color="id"))+
#           geom_point()+
#           geom_line()+
#           geom_text(data = ds_long %>% filter(model=="ds4nopos"),
#                     aes(label = id), hjust = -0.2)+
#           theme_minimal() +
#           labs(x = "model", y = ind, color = "id") +
#           theme(legend.position = "none")
#   ggsave(paste("figure/",ind,".eps",sep=""), width = 5, height = 10)
# }

# accuracy mean~range plot
ds_acc <- ds_long %>% 
  group_by(id) %>% 
  summarize(acc_mean = mean(acc),
            acc_min = min(acc),
            acc_max = max(acc)) %>% 
  mutate(id = reorder(factor(id), acc_mean))

ggplot(data = ds_acc, aes(x=id, y=acc_mean))+
  geom_errorbar(aes(ymin = acc_min, ymax = acc_max)) + 
  geom_point(size = 3, color = "red") +
  labs(x = "Mean of accuracy", y = "Session (Ordered by Range)") +
  theme_update()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figure/acc_variability.eps", width = 30, height = 10)

# bar plot
ds_long_error_bar <- ds_long2 %>% 
  group_by(model, ind) %>% 
  summarize(
    mean = mean(value, na.rm = T), 
    sd = sd(value, na.rm = T), 
    se = sd/sqrt(n()),
    ymin = mean - se,
    ymax = mean + se) %>% ungroup()

ggplot(data = ds_long_error_bar, aes(x=ind, y=mean, fill=model))+
      geom_bar(stat = "identity",position = position_dodge(width = 0.9))+
      geom_errorbar(aes(ymax = ymax, ymin = ymin), position = position_dodge(width = 0.9), width=0.2)+
      theme_update()+
      labs(x = "index", y = "value", color = "model")
ggsave(paste("figure/barplot.eps",sep=""), width = 10, height = 5)

# examine each bad session
# read in files
session <- "123_1"
good_model <- "predict_30_no_pos"
bad_model <- "predict_4_pos"
pred_good <- read_csv(paste(good_model,"/",session, "_predict.csv", sep=""))
pred_bad <- read_csv(paste(bad_model,"/",session, "_predict.csv", sep=""))
actual_good <- read_csv(paste(good_model,"/",session, "_actual.csv", sep="")) %>% 
  mutate(ref=ifelse(ref==1,2,1)) %>% 
  select(-pool)
actual_bad <- read_csv(paste(bad_model,"/",session, "_actual.csv", sep="")) %>% 
  mutate(ref=ifelse(ref==1,2,1)) %>% 
  select(-pool)
# re-calculate accuracy
accuracy_good <- mean(pred_good$ref == actual_good$ref)
accuracy_bad <- mean(pred_bad$ref == actual_bad$ref)
# plot bad model timeline
merge_bad <- bind_cols(pred_bad, actual_bad)
merge_bad$time = 1:nrow(merge_bad)
colnames(merge_bad) <- c("pool","predict","actual","time")
long_bad <- pivot_longer(data = merge_bad, cols=c(predict,actual), names_to = "source", values_to = "category")

ggplot(data = long_bad, aes(x=time, y=category, color = source)) +
  geom_line() +
  geom_point() +
  labs(x = "Time", y = "Categorization: 1=restrained", title = "Time Series Plot") +
  theme_update()
ggsave(paste("figure/bad_sessions_timeline/",session,".png",sep=""),width=10, height=5)  


