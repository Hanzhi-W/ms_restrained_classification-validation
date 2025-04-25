# load library
library(tidyverse)
library(here)
library(lubridate)
library(janitor)
library(scales)
library(hms)

# pre-set
theme_update(text = element_text(size = 14),
             plot.title = element_text(size = 12),
             axis.text.x = element_text(size = 14, color = "black"), axis.title.x = element_text(size = 16),
             axis.text.y = element_text(size = 12,  color = "black"), axis.title.y = element_text(size = 16), 
             panel.background = element_blank(),panel.border = element_blank(), panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(), axis.line = element_blank(), 
             legend.key = element_rect(fill = "white")) 
lims <- as_hms(c('09:00:00', '21:59:00'))
hour_breaks = as_hms(c('09:00:00', '10:00:00', '11:00:00', '12:00:00', '13:00:00', '14:00:00', '15:00:00', '16:00:00', '17:00:00', '18:00:00', '19:00:00', '20:00:00', '21:00:00'))
label_breaks = c("9am","","11am","","1pm","","3pm","","5pm","","7pm","","9pm")
pos_levels <- c("Supine", "Prone", "Sitting", "Upright", "Held")
pal <-  c("#E69F00","#56B4E9", "#009E73","#F0E442", "#0072B2") %>% 
  set_names(pos_levels)

loc_levels <- c("Crawling", "Walking")
loc_pal <-  c("brown","blue") %>% 
  set_names(loc_levels)

unrestrained_levels <- c("Yes", "No")
unrestrained_pal <-  c("purple","white") %>% 
  set_names(unrestrained_levels)

# load data
ds <- read_csv("!import/lena-imu-compiled.csv")

# data wrangling
ds$id_uni <- factor(ds$id*100+ds$session)
ds <- ds %>% select(-(roll.key:Bin.Mins))
ds <- ds %>% rename_with(janitor::make_clean_names)

ds$age_centered <- scale(ds$age, scale = F)
ds$age_group <- factor(as.numeric(ds$age > 9), levels = 0:1, labels = c("Younger","Older"))

ds <- ds %>% mutate(across(sit_time:restrained_time, ~ ifelse(nap_time > 0 | exclude_time > 0, 0, .x))) %>% 
  mutate(nap_time = ifelse(nap_time > 0 & exclude_time == 0, 1, NA),
         exclude_time = ifelse(exclude_time > 0, 1, NA)) # why?

ds_long <- ds %>% pivot_longer(cols = nap_time:upright_time, names_to = "position", values_to = "prop") %>% 
  mutate(Position = factor(position,
                           levels = c("exclude_time","nap_time","upright_time", "sit_time", "prone_time", "supine_time", "held_time"),
                           labels = c("Exclude","Nap","Upright", "Sitting", "Prone", "Supine", "Held"))) %>% 
  arrange(Position, age)
ds_long <- ds_long %>% mutate(prop = na_if(prop, 0))

# plot timeline function -- need to modify
timeline_rest_pos <- function(graph_id) {
  ds_exemplar <- ds_long %>% filter(id_uni == graph_id) %>% 
    mutate(clock_time_start = as_hms(with_tz(clock_time_start, "America/Los_Angeles"))) %>% 
    mutate(sil = rescale(sil, to = c(0,1)), adult_words = rescale(adult_word_cnt, to = c(0,1)),
           walk = ifelse(walk_time > .05, .25, NA), crawl = ifelse(crawl_time > .05, .25, NA))
  
  p1 <- ggplot(ds_exemplar) + 
    geom_bar(aes(x = clock_time_start, y = prop, fill = Position), stat = "identity") + 
    scale_fill_manual(values = c("Nap" = "gray","Exclude" = "white",pal), name = "") + ylab("") + 
    geom_line(aes(x = clock_time_start, y = unrestrained_time - 1.1), color = "turquoise") +
    geom_tile(aes(x = clock_time_start, y = crawl - 2), fill = "#56B4E9") +
    geom_tile(aes(x = clock_time_start, y = walk - 2.5),fill ="#F0E442") + 
    scale_x_time(breaks = hour_breaks, labels = label_breaks, name = "", limits = lims) + 
    geom_line(aes(x = clock_time_start, y = sil + 1.2), color = "lightblue") +
    geom_line(aes(x = clock_time_start, y = adult_words + 1.2), color = "darkred") + 
    ggtitle(str_glue("{ds_exemplar$id[1]}-{ds_exemplar$session[1]} ({round(ds_exemplar$age[1],1)} months) {ds_exemplar$sitter[1]}/{ds_exemplar$crawler[1]}/{ds_exemplar$walker[1]}")) + 
    scale_y_continuous(breaks = c(-1.5, -.5, .5, 1.5), labels = c("Crawl (Blue)\nWalk (Yellow)", "Unrestrained Time", "Position", "Speech (Red)\nSilence (Blue)"), limits = c(-3, 2.25)) +
    theme(legend.position = "bottom") 
  print(p1)
  return(p1)
}
p <- make_timeline(801)
p
#legend.margin=margin(c(5,5,5,5)),legend.box.margin=margin(c(-10,-10,-10,-10))

walk(sort(unique(ds_long$id_uni)), make_timeline)