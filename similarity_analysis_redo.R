rm(list=ls())

library(corrplot)
library(tidyverse)
library(purrr)
library(magrittr)
library(brms)
library(lme4)
library(lmerTest)
library(tidybayes)
library(bayesplot)
library(bayestestR)
library(ggridges)
library(MetBrewer)
library(patchwork)
library(faintr)

pp <- met.brewer(name = "Hiroshige")

## load dataset and select columns of interest: Participant ID, certainty rating
## condition assignment (1 = detective, 2 = friend of victim, 3 = friend of
## accused), and video
dat <- read.csv('/Users/tua37526/Dropbox/certainty/data/summary.csv')
dat <- dat %>% 
  filter(n >= 50)
dat2 <- dat %>% 
  dplyr::select(PID, meanRating, condName2, condName3, videoName2, secTiming, n) %>% 
  group_by(PID,videoName2) %>% dplyr::mutate(id = seq_len(n()))
dat3 <- dat2 %>% 
  group_by(videoName2)
k <- group_split(dat3)

## create list of video names
vid_Names <- c("broadchurchS1E3", "broadchurchS1E4", "fargoS1E2",
               "fargoS1E7", "mindhunterS1E3", "mindhunterS2E7",
               "mindhunterS2E9", "theNightOfS1E1", "theUndoingS1E1")
## create list of PIDs
PID_names <- c("101", "102", "103", "104", "105", "106", "107", "109", "110",
               "111", "112", "113", "114", "115", "116", "117", "118", "119",
               "120", "121", "122", "124", "125", "126", "127", "128", "129",
               "130", "131", "132", "133", "134", "135", "136", "137")
## assign mean length values per video (50 samples per second)
vidLength <- c(464, 281, 364, 230, 291, 418, 341, 201, 281)
mean(vidLength)
sd(vidLength)

for (i in 1:9) {
PID_vec <- (k[[i]]$PID)
index_vec <- (k[[i]]$id)
cond_vec <- (k[[i]]$condName2)
meanRating_vec<- (k[[i]]$meanRating)
assign(paste0("vid", i),
       data.frame(PID_vec, index_vec, cond_vec, meanRating_vec))
}

# yes, this is bad code -- but getting all of the videos to the same size is a massive pain
for (i in 1:9) {
  if (i == 1) {
    vid1 %>% 
      filter(index_vec <= vidLength[i]) -> vid1
  }
  if (i == 2) {
    vid2 %>% 
      filter(index_vec <= vidLength[i]) -> vid2
  }
  if (i == 3) {
    vid3 %>% 
      filter(index_vec <= vidLength[i]) -> vid3
  }
  if (i == 4) {
    vid4 %>% 
      filter(index_vec <= vidLength[i]) -> vid4
  }
  if (i == 5) {
    vid5 %>% 
      filter(index_vec <= vidLength[i]) -> vid5
  }
  if (i == 6) {
    vid6 %>% 
      filter(index_vec <= vidLength[i]) -> vid6
  }
  if (i == 7) {
    vid7 %>% 
      filter(index_vec <= vidLength[i]) -> vid7
  }
  if (i == 8) {
    vid8 %>% 
      filter(index_vec <= vidLength[i]) -> vid8
  }
  if (i == 9) {
    vid9 %>% 
      filter(index_vec <= vidLength[i]) -> vid9
  }
}

k1 <- split(vid1, vid1$PID_vec)
k2 <- split(vid2, vid2$PID_vec)
k3 <- split(vid3, vid3$PID_vec)
k4 <- split(vid4, vid4$PID_vec)
k5 <- split(vid5, vid5$PID_vec)
k6 <- split(vid6, vid6$PID_vec)
k7 <- split(vid7, vid7$PID_vec)
k8 <- split(vid8, vid8$PID_vec)
k9 <- split(vid9, vid9$PID_vec)

## create empty data frame to fill with correlations
corFrame1 <- data.frame()
for (i in 1:length(k1)) {
  for (j in 1:length(k1)) {
  m <- cor(k1[[i]][4], k1[[j]][4])
  corFrame1[i,j] <- m
  }
i <- i + 1
}

corFrame2 <- data.frame()
for (i in 1:length(k2)) {
  for (j in 1:length(k2)) {
    m <- cor(k2[[i]][4], k2[[j]][4])
    corFrame2[i,j] <- m
  }
  i <- i + 1
}

corFrame3 <- data.frame()
for (i in 1:length(k3)) {
  for (j in 1:length(k3)) {
    m <- cor(k3[[i]][4], k3[[j]][4])
    corFrame3[i,j] <- m
  }
  i <- i + 1
}

## remove 102 (no variance)
k4 <- within(k4, rm("102"))
k4 <- within(k4, rm("127"))
corFrame4 <- data.frame()
for (i in 1:length(k4)) {
  for (j in 1:length(k4)) {
    m <- cor(k4[[i]][4], k4[[j]][4])
    corFrame4[i,j] <- m
  }
  i <- i + 1
}

k5 <- within(k5, rm("112"))
corFrame5 <- data.frame()
for (i in 1:length(k5)) {
  for (j in 1:length(k5)) {
    m <- cor(k5[[i]][4], k5[[j]][4])
    corFrame5[i,j] <- m
  }
  i <- i + 1
}

k6 <- within(k6, rm("113"))
k6 <- within(k6, rm("134"))
corFrame6 <- data.frame()
for (i in 1:length(k6)) {
  for (j in 1:length(k6)) {
    m <- cor(k6[[i]][4], k6[[j]][4])
    corFrame6[i,j] <- m
  }
  i <- i + 1
}

k7 <- within(k7, rm("102"))
corFrame7 <- data.frame()
for (i in 1:length(k7)) {
  for (j in 1:length(k7)) {
    m <- cor(k7[[i]][4], k7[[j]][4])
    corFrame7[i,j] <- m
  }
  i <- i + 1
}

corFrame8 <- data.frame()
for (i in 1:length(k8)) {
  for (j in 1:length(k8)) {
    m <- cor(k8[[i]][4], k8[[j]][4])
    corFrame8[i,j] <- m
  }
  i <- i + 1
}

k9 <- within(k9, rm("122"))
corFrame9 <- data.frame()
for (i in 1:length(k9)) {
  for (j in 1:length(k9)) {
    m <- cor(k9[[i]][4], k9[[j]][4])
    corFrame9[i,j] <- m
  }
  i <- i + 1
}

## compute video-wise similarity

## video 1 similarity
id_list <- vid1 %>% 
  filter(index_vec == 1)
cond_list <- id_list$cond_vec
PID_list <- id_list$PID_vec
id_list$PID_cond <- paste(PID_list, cond_list, sep = "_")
colnames(corFrame1) <- id_list$PID_cond

## subset correlation dataset based on condition
corFrame1_det <- corFrame1 %>% 
  select(ends_with("ive"))
corFrame1_vic <- corFrame1 %>% 
  select(ends_with("tim"))
corFrame1_acc <- corFrame1 %>% 
  select(ends_with("sed"))

## compute overall timecourse similarity
det_sim <- rowMeans(corFrame1_det)
vic_sim <- rowMeans(corFrame1_vic)
acc_sim <- rowMeans(corFrame1_acc)

## combine those into omnibus dataframe
sim_check <- data.frame(PID_list, cond_list, det_sim, acc_sim, vic_sim)

## adjust to remove self-correlations
sim_det_adj <- 
  sim_check %>%
  filter(cond_list == "detective") %>% 
  mutate(det_sim = det_sim - (1/length(PID_list)))
sim_vic_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfVictim") %>% 
  mutate(vic_sim = vic_sim - (1/length(PID_list)))
sim_acc_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfAccused") %>% 
  mutate(acc_sim = acc_sim - (1/length(PID_list)))

# bind adjusted frames together, make condition a factor
sim_adj <- rbind(sim_det_adj, sim_vic_adj, sim_acc_adj)

# pivot the data from wide to long
v1_sim_adj_long <- sim_adj %>% 
  pivot_longer(
    cols = ends_with("sim"),
    names_to = "target_cond",
    values_to = "sim"
  )
v1_sim_adj_long$vidNum <- 1

# quick visualization check (remove for primetime)
test <- lmer(sim ~ target_cond * cond_list + (1|PID_list), data = v1_sim_adj_long)
summary(test)
library(effects)
plot(effect("target_cond:cond_list", test), grid = TRUE)


## video 2 similarity
id_list <- vid2 %>% 
  filter(index_vec == 1)
cond_list <- id_list$cond_vec
PID_list <- id_list$PID_vec
id_list$PID_cond <- paste(PID_list, cond_list, sep = "_")
colnames(corFrame2) <- id_list$PID_cond

## subset correlation dataset based on condition
corFrame2_det <- corFrame2 %>% 
  select(ends_with("ive"))
corFrame2_vic <- corFrame2 %>% 
  select(ends_with("tim"))
corFrame2_acc <- corFrame2 %>% 
  select(ends_with("sed"))

## compute overall timecourse similarity
det_sim <- rowMeans(corFrame2_det)
vic_sim <- rowMeans(corFrame2_vic)
acc_sim <- rowMeans(corFrame2_acc)

## combine those into omnibus dataframe
sim_check <- data.frame(PID_list, cond_list, det_sim, acc_sim, vic_sim)

## adjust to remove self-correlations
sim_det_adj <- 
  sim_check %>%
  filter(cond_list == "detective") %>% 
  mutate(det_sim = det_sim - (1/length(PID_list)))
sim_vic_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfVictim") %>% 
  mutate(vic_sim = vic_sim - (1/length(PID_list)))
sim_acc_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfAccused") %>% 
  mutate(acc_sim = acc_sim - (1/length(PID_list)))

# bind adjusted frames together, make condition a factor
sim_adj <- rbind(sim_det_adj, sim_vic_adj, sim_acc_adj)

# pivot the data from wide to long
v2_sim_adj_long <- sim_adj %>% 
  pivot_longer(
    cols = ends_with("sim"),
    names_to = "target_cond",
    values_to = "sim"
  )
v2_sim_adj_long$vidNum <- 2

# quick visualization check (remove for primetime)
test <- lmer(sim ~ target_cond * cond_list + (1|PID_list), data = v2_sim_adj_long)
summary(test)
library(effects)
plot(effect("target_cond:cond_list", test), grid = TRUE)

## video 3 similarity
id_list <- vid3 %>% 
  filter(index_vec == 1)
cond_list <- id_list$cond_vec
PID_list <- id_list$PID_vec
id_list$PID_cond <- paste(PID_list, cond_list, sep = "_")
colnames(corFrame3) <- id_list$PID_cond

## subset correlation dataset based on condition
corFrame3_det <- corFrame3 %>% 
  select(ends_with("ive"))
corFrame3_vic <- corFrame3 %>% 
  select(ends_with("tim"))
corFrame3_acc <- corFrame3 %>% 
  select(ends_with("sed"))

## compute overall timecourse similarity
det_sim <- rowMeans(corFrame3_det)
vic_sim <- rowMeans(corFrame3_vic)
acc_sim <- rowMeans(corFrame3_acc)

## combine those into omnibus dataframe
sim_check <- data.frame(PID_list, cond_list, det_sim, acc_sim, vic_sim)

## adjust to remove self-correlations
sim_det_adj <- 
  sim_check %>%
  filter(cond_list == "detective") %>% 
  mutate(det_sim = det_sim - (1/length(PID_list)))
sim_vic_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfVictim") %>% 
  mutate(vic_sim = vic_sim - (1/length(PID_list)))
sim_acc_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfAccused") %>% 
  mutate(acc_sim = acc_sim - (1/length(PID_list)))

# bind adjusted frames together, make condition a factor
sim_adj <- rbind(sim_det_adj, sim_vic_adj, sim_acc_adj)

# pivot the data from wide to long
v3_sim_adj_long <- sim_adj %>% 
  pivot_longer(
    cols = ends_with("sim"),
    names_to = "target_cond",
    values_to = "sim"
  )
v3_sim_adj_long$vidNum <- 3

# quick visualization check (remove for primetime)
test <- lmer(sim ~ target_cond * cond_list + (1|PID_list), data = v3_sim_adj_long)
summary(test)
library(effects)
plot(effect("target_cond:cond_list", test), grid = TRUE)

## video 4 similarity
id_list <- vid4 %>% 
  filter(index_vec == 1)
cond_list <- id_list$cond_vec
PID_list <- id_list$PID_vec
id_list$PID_cond <- paste(PID_list, cond_list, sep = "_")
colnames(corFrame4) <- id_list$PID_cond

## subset correlation dataset based on condition
corFrame4_det <- corFrame4 %>% 
  select(ends_with("ive"))
corFrame4_vic <- corFrame4 %>% 
  select(ends_with("tim"))
corFrame4_acc <- corFrame4 %>% 
  select(ends_with("sed"))

## compute overall timecourse similarity
det_sim <- rowMeans(corFrame4_det)
vic_sim <- rowMeans(corFrame4_vic)
acc_sim <- rowMeans(corFrame4_acc)

## combine those into omnibus dataframe
sim_check <- data.frame(PID_list, cond_list, det_sim, acc_sim, vic_sim)

## adjust to remove self-correlations
sim_det_adj <- 
  sim_check %>%
  filter(cond_list == "detective") %>% 
  mutate(det_sim = det_sim - (1/length(PID_list)))
sim_vic_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfVictim") %>% 
  mutate(vic_sim = vic_sim - (1/length(PID_list)))
sim_acc_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfAccused") %>% 
  mutate(acc_sim = acc_sim - (1/length(PID_list)))

# bind adjusted frames together, make condition a factor
sim_adj <- rbind(sim_det_adj, sim_vic_adj, sim_acc_adj)

# pivot the data from wide to long
v4_sim_adj_long <- sim_adj %>% 
  pivot_longer(
    cols = ends_with("sim"),
    names_to = "target_cond",
    values_to = "sim"
  )
v4_sim_adj_long$vidNum <- 4

# quick visualization check (remove for primetime)
test <- lmer(sim ~ target_cond * cond_list + (1|PID_list), data = v4_sim_adj_long)
summary(test)
library(effects)
plot(effect("target_cond:cond_list", test), grid = TRUE)

## video 5 similarity
id_list <- vid5 %>% 
  filter(index_vec == 1) %>% 
  filter(PID_list != 112)
cond_list <- id_list$cond_vec
PID_list <- id_list$PID_vec
id_list$PID_cond <- paste(PID_list, cond_list, sep = "_")
colnames(corFrame5) <- id_list$PID_cond

## subset correlation dataset based on condition
corFrame5_det <- corFrame5 %>% 
  select(ends_with("ive"))
corFrame5_vic <- corFrame5 %>% 
  select(ends_with("tim"))
corFrame5_acc <- corFrame5 %>% 
  select(ends_with("sed"))

## compute overall timecourse similarity
det_sim <- rowMeans(corFrame5_det)
vic_sim <- rowMeans(corFrame5_vic)
acc_sim <- rowMeans(corFrame5_acc)

## combine those into omnibus dataframe
sim_check <- data.frame(PID_list, cond_list, det_sim, acc_sim, vic_sim)

## adjust to remove self-correlations
sim_det_adj <- 
  sim_check %>%
  filter(cond_list == "detective") %>% 
  mutate(det_sim = det_sim - (1/length(PID_list)))
sim_vic_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfVictim") %>% 
  mutate(vic_sim = vic_sim - (1/length(PID_list)))
sim_acc_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfAccused") %>% 
  mutate(acc_sim = acc_sim - (1/length(PID_list)))

# bind adjusted frames together, make condition a factor
sim_adj <- rbind(sim_det_adj, sim_vic_adj, sim_acc_adj)

# pivot the data from wide to long
v5_sim_adj_long <- sim_adj %>% 
  pivot_longer(
    cols = ends_with("sim"),
    names_to = "target_cond",
    values_to = "sim"
  )
v5_sim_adj_long$vidNum <- 5

# quick visualization check (remove for primetime)
test <- lmer(sim ~ target_cond * cond_list + (1|PID_list), data = v5_sim_adj_long)
summary(test)
library(effects)
plot(effect("target_cond:cond_list", test), grid = TRUE)

## video 6 similarity
id_list <- vid6 %>% 
  filter(index_vec == 1)
cond_list <- id_list$cond_vec
PID_list <- id_list$PID_vec
id_list$PID_cond <- paste(PID_list, cond_list, sep = "_")
colnames(corFrame6) <- id_list$PID_cond

## subset correlation dataset based on condition
corFrame6_det <- corFrame6 %>% 
  select(ends_with("ive"))
corFrame6_vic <- corFrame6 %>% 
  select(ends_with("tim"))
corFrame6_acc <- corFrame6 %>% 
  select(ends_with("sed"))

## compute overall timecourse similarity
det_sim <- rowMeans(corFrame6_det)
vic_sim <- rowMeans(corFrame6_vic)
acc_sim <- rowMeans(corFrame6_acc)

## combine those into omnibus dataframe
sim_check <- data.frame(PID_list, cond_list, det_sim, acc_sim, vic_sim)

## adjust to remove self-correlations
sim_det_adj <- 
  sim_check %>%
  filter(cond_list == "detective") %>% 
  mutate(det_sim = det_sim - (1/length(PID_list)))
sim_vic_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfVictim") %>% 
  mutate(vic_sim = vic_sim - (1/length(PID_list)))
sim_acc_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfAccused") %>% 
  mutate(acc_sim = acc_sim - (1/length(PID_list)))

# bind adjusted frames together, make condition a factor
sim_adj <- rbind(sim_det_adj, sim_vic_adj, sim_acc_adj)

# pivot the data from wide to long
v6_sim_adj_long <- sim_adj %>% 
  pivot_longer(
    cols = ends_with("sim"),
    names_to = "target_cond",
    values_to = "sim"
  )
v6_sim_adj_long$vidNum <- 6

# quick visualization check (remove for primetime)
test <- lmer(sim ~ target_cond * cond_list + (1|PID_list), data = v6_sim_adj_long)
summary(test)
library(effects)
plot(effect("target_cond:cond_list", test), grid = TRUE)

## video 7 similarity
id_list <- vid7 %>% 
  filter(index_vec == 1)
cond_list <- id_list$cond_vec
PID_list <- id_list$PID_vec
id_list$PID_cond <- paste(PID_list, cond_list, sep = "_")
colnames(corFrame7) <- id_list$PID_cond

## subset correlation dataset based on condition
corFrame7_det <- corFrame7 %>% 
  select(ends_with("ive"))
corFrame7_vic <- corFrame7 %>% 
  select(ends_with("tim"))
corFrame7_acc <- corFrame7 %>% 
  select(ends_with("sed"))

## compute overall timecourse similarity
det_sim <- rowMeans(corFrame7_det)
vic_sim <- rowMeans(corFrame7_vic)
acc_sim <- rowMeans(corFrame7_acc)

## combine those into omnibus dataframe
sim_check <- data.frame(PID_list, cond_list, det_sim, acc_sim, vic_sim)

## adjust to remove self-correlations
sim_det_adj <- 
  sim_check %>%
  filter(cond_list == "detective") %>% 
  mutate(det_sim = det_sim - (1/length(PID_list)))
sim_vic_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfVictim") %>% 
  mutate(vic_sim = vic_sim - (1/length(PID_list)))
sim_acc_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfAccused") %>% 
  mutate(acc_sim = acc_sim - (1/length(PID_list)))

# bind adjusted frames together, make condition a factor
sim_adj <- rbind(sim_det_adj, sim_vic_adj, sim_acc_adj)

# pivot the data from wide to long
v7_sim_adj_long <- sim_adj %>% 
  pivot_longer(
    cols = ends_with("sim"),
    names_to = "target_cond",
    values_to = "sim"
  )
v7_sim_adj_long$vidNum <- 7

# quick visualization check (remove for primetime)
test <- lmer(sim ~ target_cond * cond_list + (1|PID_list), data = v7_sim_adj_long)
summary(test)
library(effects)
plot(effect("target_cond:cond_list", test), grid = TRUE)

## video 8 similarity
id_list <- vid8 %>% 
  filter(index_vec == 1)
cond_list <- id_list$cond_vec
PID_list <- id_list$PID_vec
id_list$PID_cond <- paste(PID_list, cond_list, sep = "_")
colnames(corFrame8) <- id_list$PID_cond

## subset correlation dataset based on condition
corFrame8_det <- corFrame8 %>% 
  select(ends_with("ive"))
corFrame8_vic <- corFrame8 %>% 
  select(ends_with("tim"))
corFrame8_acc <- corFrame8 %>% 
  select(ends_with("sed"))

## compute overall timecourse similarity
det_sim <- rowMeans(corFrame8_det)
vic_sim <- rowMeans(corFrame8_vic)
acc_sim <- rowMeans(corFrame8_acc)

## combine those into omnibus dataframe
sim_check <- data.frame(PID_list, cond_list, det_sim, acc_sim, vic_sim)

## adjust to remove self-correlations
sim_det_adj <- 
  sim_check %>%
  filter(cond_list == "detective") %>% 
  mutate(det_sim = det_sim - (1/length(PID_list)))
sim_vic_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfVictim") %>% 
  mutate(vic_sim = vic_sim - (1/length(PID_list)))
sim_acc_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfAccused") %>% 
  mutate(acc_sim = acc_sim - (1/length(PID_list)))

# bind adjusted frames together, make condition a factor
sim_adj <- rbind(sim_det_adj, sim_vic_adj, sim_acc_adj)

# pivot the data from wide to long
v8_sim_adj_long <- sim_adj %>% 
  pivot_longer(
    cols = ends_with("sim"),
    names_to = "target_cond",
    values_to = "sim"
  )
v8_sim_adj_long$vidNum <- 8

# quick visualization check (remove for primetime)
test <- lmer(sim ~ target_cond * cond_list + (1|PID_list), data = v8_sim_adj_long)
summary(test)
library(effects)
plot(effect("target_cond:cond_list", test), grid = TRUE)

## video 9 similarity
id_list <- vid9 %>% 
  filter(index_vec == 1) %>% 
  filter(PID_list != 122)
cond_list <- id_list$cond_vec
PID_list <- id_list$PID_vec
id_list$PID_cond <- paste(PID_list, cond_list, sep = "_")
colnames(corFrame9) <- id_list$PID_cond

## subset correlation dataset based on condition
corFrame9_det <- corFrame9 %>% 
  select(ends_with("ive"))
corFrame9_vic <- corFrame9 %>% 
  select(ends_with("tim"))
corFrame9_acc <- corFrame9 %>% 
  select(ends_with("sed"))

## compute overall timecourse similarity
det_sim <- rowMeans(corFrame9_det)
vic_sim <- rowMeans(corFrame9_vic)
acc_sim <- rowMeans(corFrame9_acc)

## combine those into omnibus dataframe
sim_check <- data.frame(PID_list, cond_list, det_sim, acc_sim, vic_sim)

## adjust to remove self-correlations
sim_det_adj <- 
  sim_check %>%
  filter(cond_list == "detective") %>% 
  mutate(det_sim = det_sim - (1/length(PID_list)))
sim_vic_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfVictim") %>% 
  mutate(vic_sim = vic_sim - (1/length(PID_list)))
sim_acc_adj <- 
  sim_check %>%
  filter(cond_list == "friendOfAccused") %>% 
  mutate(acc_sim = acc_sim - (1/length(PID_list)))

# bind adjusted frames together, make condition a factor
sim_adj <- rbind(sim_det_adj, sim_vic_adj, sim_acc_adj)

# pivot the data from wide to long
v9_sim_adj_long <- sim_adj %>% 
  pivot_longer(
    cols = ends_with("sim"),
    names_to = "target_cond",
    values_to = "sim"
  )
v9_sim_adj_long$vidNum <- 9

# quick visualization check (remove for primetime)
test <- lmer(sim ~ target_cond * cond_list + (1|PID_list), data = v9_sim_adj_long)
summary(test)
library(effects)
plot(effect("target_cond:cond_list", test), grid = TRUE)

total_sim <- rbind(v1_sim_adj_long, v2_sim_adj_long, v3_sim_adj_long, 
                   v4_sim_adj_long, v5_sim_adj_long, v6_sim_adj_long,
                   v7_sim_adj_long, v8_sim_adj_long, v9_sim_adj_long)
total_sim$cond_list <- as.factor(total_sim$cond_list)
total_sim$target_cond <- as.factor(total_sim$target_cond)

total_sim$cond_list <- relevel(total_sim$cond_list, ref = "detective")
total_sim$target_cond <- relevel(total_sim$target_cond, ref = "det_sim")

test <- lmer(sim ~ target_cond * cond_list + (1|PID_list) + (1|vidNum), data = total_sim)
summary(test)
plot(effect("target_cond:cond_list", test), grid = TRUE)
library(reghelper)
simple_slopes(test)


## brms model
prior1 <- c(prior(normal(0,.10), class = b),
                      prior(student_t(3, -12.4, 23.5), class = Intercept),
                      prior(student_t(3, 0, 23.5), class = sd),
                      prior(student_t(3, 0, 23.5), class = sigma)
)

bmod1 <- brm(
  sim ~ cond_list + (1|PID_list) + (1|vidNum),
  data = total_sim, family = gaussian(),
  prior = prior1,
  warmup = 3000, iter = 10000,
  control = list(adapt_delta = 0.99)
)
bmod1

plot(bmod1)
stanplot(bmod1, 
         type = "areas",
         prob = 0.95)
pairs(bmod1)
post <- posterior_samples(bmod1, add_chain = T)
mcmc_acf(post, pars = c("b_Intercept", 
                        "sd_PID_list__Intercept"), lags = 8)
bmod1 %>% 
  neff_ratio() %>% 
  mcmc_neff_hist(binwidth = .1) +
  yaxis_text()

## get group-level parameters for hierarchical model
post <-
  post %>% 
  as_tibble()
head(post)

## plot histograms
post_s <-
  post %>% 
  transmute(Detective = (b_Intercept),
            `Friend of Victim` = (b_Intercept + b_cond_listfriendOfVictim),
            `Friend of Accused` = (b_Intercept + b_cond_listfriendOfAccused)) %>% 
  mutate(`Detective - Friend of Victim` = Detective - `Friend of Victim`,
         `Detective - Friend of Accused` = Detective - `Friend of Accused`,
         `Friend of Victim - Friend of Accused` = `Friend of Victim` - `Friend of Accused`)
head(post_s)

make_histogram_d <- function(data, mapping, title, xlim, ...) {
  
  ggplot(data, mapping) +
    geom_histogram(fill = pp[1], color = pp[1], size = .2,
                   bins = 30) +
    stat_pointinterval(aes(y = 0), 
                       point_interval = mode_hdi, .width = .95, color = "gray30") +
    scale_y_continuous(NULL, breaks = NULL) +
    geom_vline(xintercept = 0, linetype = 2, color = "black") +
    labs(title = title,
         x     = expression(theta)) +
    coord_cartesian(xlim = xlim) +
    theme(legend.position = "none")
  
}

make_histogram_v <- function(data, mapping, title, xlim, ...) {
  
  ggplot(data, mapping) +
    geom_histogram(fill = pp[2], color = pp[2], size = .2,
                   bins = 30) +
    stat_pointinterval(aes(y = 0), 
                       point_interval = mode_hdi, .width = .95, color = "gray30") +
    scale_y_continuous(NULL, breaks = NULL) +
    geom_vline(xintercept = 0, linetype = 2, color = "black") +
    labs(title = title,
         x     = expression(theta)) +
    coord_cartesian(xlim = xlim) +
    theme(legend.position = "none")
  
}

make_histogram_a <- function(data, mapping, title, xlim, ...) {
  
  ggplot(data, mapping) +
    geom_histogram(fill = pp[9], color = pp[9], size = .2,
                   bins = 30) +
    stat_pointinterval(aes(y = 0), 
                       point_interval = mode_hdi, .width = .95, color = "gray30") +
    scale_y_continuous(NULL, breaks = NULL) +
    geom_vline(xintercept = 0, linetype = 2, color = "black") +
    labs(title = title,
         x     = expression(theta)) +
    coord_cartesian(xlim = xlim) +
    theme(legend.position = "none")
  
}

make_histogram2 <- function(data, mapping, title, xlim, ...) {
  
  ggplot(data, mapping) +
    geom_histogram(fill = pp[7], color = pp[7], size = .2,
                   bins = 30) +
    stat_pointinterval(aes(y = 0), 
                       point_interval = mode_hdi, .width = .95, color = "gray30") +
    scale_y_continuous(NULL, breaks = NULL) +
    geom_vline(xintercept = 0, linetype = 2, color = "black") +
    labs(title = title,
         x     = expression(theta)) +
    coord_cartesian(xlim = xlim) +
    theme(legend.position = "none")
  
}

make_point <- function(data, mapping, limits, ...) {
  
  ggplot(data, mapping) +
    geom_abline(color = pp[3]) +
    geom_point(color = pp[5], size = 1/10, alpha = 1/20) +
    coord_cartesian(xlim = limits,
                    ylim = limits)
  
}

##
p1 <-
  make_histogram_d(data = post_s,
                 aes(x = Detective), 
                 title = "Detective", 
                 xlim = c(-.5,1))
p1

p2 <-
  make_histogram_v(data = post_s,
                 aes(x = `Friend of Victim`), 
                 title = "Friend of Victim", 
                 xlim = c(-.5, 1))
p2

p3 <-
  make_histogram_a(data = post_s,
                 aes(x = `Friend of Accused`), 
                 title = "Friend of Accused", 
                 xlim = c(-.5, 1))
p3

p4 <-
  make_histogram2(data = post_s,
                 aes(x = `Detective - Friend of Victim`), 
                 title = "Detective - Friend of Victim", 
                 xlim = c(-.15, .15))
p4

p5 <-
  make_histogram2(data = post_s,
                 aes(x = `Detective - Friend of Accused`), 
                 title = "Detective - Friend of Accused", 
                 xlim = c(-.15, .25))
p5

p6 <-
  make_histogram2(data = post_s,
                 aes(x = `Friend of Victim - Friend of Accused`), 
                 title = "Friend of Victim - Friend of Accused", 
                 xlim = c(-.15, .25))
p6

p1 + p2 + p3 + p4 + p5 + p6

total_sim_plot <- total_sim

total_sim_plot <-
  total_sim %>% 
  mutate(Perspective = factor(cond_list,
                      levels = c("detective", "friendOfVictim", "friendOfAccused"),
                      ordered = T,
                      labels = c("Detective", "FoV", "FoA"))) %>% 
  mutate(Group_Similarity = factor(target_cond,
                                   levels = c("det_sim", "vic_sim", "acc_sim"),
                                   ordered = T,
                                   labels = c("Detectives", "FoVs", "FoAs")))
total_sim_plot %>% 
  group_by(Perspective, Group_Similarity) %>% 
  summarise(m_similarity = mean(sim),
            n = n()) %>% 
  ungroup() %>% 
  mutate(Group_Similarity = fct_reorder(Group_Similarity, m_similarity),
         Perspective = fct_reorder(Perspective, m_similarity)) %>% 
  
  ggplot(aes(x = Group_Similarity, y = Perspective, fill = m_similarity, label = round(m_similarity, digits = 2))) +
  geom_tile() +
  geom_text(size = 5) +
  scale_fill_gradient(low = pp[9], high = pp[1],
                      breaks = c(0, .99), 
                      labels = c("low corr", "high corr")) +
  scale_x_discrete("Group Similarity", expand = c(0, 0)) +
  scale_y_discrete("Perspective", expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        axis.text.y = element_text(hjust = 0),
        axis.ticks = element_blank())

get_posterior_beliefs_about_hypotheses(bmod1)
compare_groups(
  model = bmod1, 
  higher = list(cond_list = "detective"), 
  lower = list(cond_list = "friendOfAccused"))

compare_groups(
  model = bmod1, 
  higher = list(cond_list = "friendOfVictim"), 
  lower = list(cond_list = "detective"))

  
