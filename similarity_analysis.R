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

pp <- met.brewer(name = "Hiroshige")

## load dataset and select columns of interest: Participant ID, certainty rating
## condition assignment (1 = detective, 2 = friend of victim, 3 = friend of
## accused), and video
dat <- read.csv('/Users/tua37526/Dropbox/certainty/data/summary.csv')
dat <- dat %>% 
  filter(n >= 50)
dat2 <- dat %>% 
  dplyr::select(PID, meanRating, condName3, videoName2, secTiming, n) %>% 
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
cond_vec <- (k[[i]]$condName3)
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

## calculate similarity
v1sim <- rowMeans(corFrame1) - (1/35)
v2sim <- rowMeans(corFrame2) - (1/34)
v3sim <- rowMeans(corFrame3) - (1/34)
v4sim <- rowMeans(corFrame4) - (1/33)
v5sim <- rowMeans(corFrame5) - (1/34)
v6sim <- rowMeans(corFrame6) - (1/33)
v7sim <- rowMeans(corFrame7) - (1/34)
v8sim <- rowMeans(corFrame8) - (1/33)
v9sim <- rowMeans(corFrame9) - (1/32)

## video 2 similarity
v2sim_df <- data.frame(v2sim)
v2sim_df$cond <- NA
v1sim_df$PID <- NA
v2sim_df$vid <- NA
v2sim_df$vid <- "v2"
table(vid2$PID_vec, vid2$cond_vec)
cond_list <- c(2, 1, 3, 2, 1, 2, 3, 1, 2, 1, 1, 3, 
               2, 1, 1, 1, 1, 1, 2, 3, 2, 1, 2, 1, 
               1, 3, 1, 1, 3, 2, 3, 2, 1, 2)
PID_list_2 <- c("101", "102", "103", "104", "105", "106", "107", "109", "110",
              "111", "113", "114", "115", "116", "117", "118", "119",
              "120", "121", "122", "124", "125", "126", "127", "128", "129",
              "130", "131", "132", "133", "134", "135", "136", "137")
for (i in 1:length(cond_list)) {
  v2sim_df$cond[i] <- cond_list[i]
}
v2sim_df$cond <- as.factor(v2sim_df$cond)
for (i in 1:length(PID_list_2)) {
  v2sim_df$PID[i] <- PID_list_2[i]
}
test <- lm(v2sim ~ cond, data = v2sim_df)
summary(test)
plot(effect("cond", test), grid = TRUE)

## video 3 similarity
v3sim_df <- data.frame(v3sim)
v3sim_df$cond <- NA
v3sim_df$vid <- NA
v3sim_df$vid <- "v3"
table(vid3$PID_vec, vid3$cond_vec)
cond_list <- c(3, 3, 1, 1, 1, 3, 1, 3, 3, 1, 2, 3, 3, 
               2, 1, 2, 2, 1, 3, 1, 2, 3, 3, 2, 3, 2, 
               1, 2, 2, 1, 3, 1, 2, 3)
PID_list_3 <- c("101", "102", "103", "104", "105", "106", "107", "109", "110",
              "111", "113", "114", "115", "116", "117", "118", "119",
              "120", "121", "122", "124", "125", "126", "127", "128", "129",
              "130", "131", "132", "133", "134", "135", "136", "137")
for (i in 1:length(cond_list)) {
  v3sim_df$cond[i] <- cond_list[i]
}
v3sim_df$cond <- as.factor(v3sim_df$cond)
for (i in 1:length(PID_list_3)) {
  v3sim_df$PID[i] <- PID_list_3[i]
}
test <- lm(v3sim ~ cond, data = v3sim_df)
summary(test)
plot(effect("cond", test), grid = TRUE)

## video 4 similarity ## no 102, 127
v4sim_df <- data.frame(v4sim)
v4sim_df$cond <- NA
v4sim_df$vid <- NA
v4sim_df$vid <- "v4"
table(vid4$PID_vec, vid4$cond_vec)
cond_list <- c(2, 2, 2, 2, 1, 2, 3, 2, 2, 2, 1, 2, 1, 1, 
               3, 3, 1, 3, 1, 3, 3, 2, 3, 2, 2, 1, 1, 2, 
               3, 2, 1, 3, 1)
PID_list_4 <- c("101", "103", "104", "105", "106", "107", "109", "110",
                "111", "113", "114", "115", "116", "117", "118", "119",
                "120", "121", "122", "124", "125", "126", "128", "129",
                "130", "131", "132", "133", "134", "135", "136", "137")
for (i in 1:length(cond_list)) {
  v4sim_df$cond[i] <- cond_list[i]
}
v4sim_df$cond <- as.factor(v4sim_df$cond)
for (i in 1:length(PID_list_4)) {
  v4sim_df$PID[i] <- PID_list_4[i]
}
test <- lm(v4sim ~ cond, data = v4sim_df)
summary(test)
plot(effect("cond", test), grid = TRUE)

## video 5 similarity ## no 112
v5sim_df <- data.frame(v5sim)
v5sim_df$cond <- NA
v5sim_df$vid <- NA
v5sim_df$vid <- "v5"
table(vid5$PID_vec, vid5$cond_vec)
cond_list <- c(1, 2, 2, 3, 1, 3, 2, 2, 1, 2, 1, 1, 3, 3, 
               2, 2, 3, 3, 3, 2, 1, 1, 2, 3, 2, 1, 2, 1, 
               1, 1, 3, 2, 3, 1)
PID_list_5 <- c("101", "102", "103", "104", "105", "106", "107", "109", "110",
                "111", "113", "114", "115", "116", "117", "118", "119",
                "120", "121", "122", "124", "125", "126", "127", "128", "129",
                "130", "131", "132", "133", "134", "135", "136", "137")
for (i in 1:length(cond_list)) {
  v5sim_df$cond[i] <- cond_list[i]
}
v5sim_df$cond <- as.factor(v5sim_df$cond)
for (i in 1:length(PID_list_5)) {
  v5sim_df$PID[i] <- PID_list_5[i]
}
test <- lm(v5sim ~ cond, data = v5sim_df)
summary(test)
plot(effect("cond", test), grid = TRUE)

## video 6 similarity ## no 113, 134
v6sim_df <- data.frame(v6sim)
v6sim_df$cond <- NA
v6sim_df$vid <- NA
v6sim_df$vid <- "v6"
table(vid6$PID_vec, vid6$cond_vec)
cond_list <- c(3, 2, 1, 1, 3, 2, 2, 3, 3, 2, 3, 1, 1, 2, 
               3, 1, 3, 1, 2, 2, 3, 2, 3, 2, 2, 1, 3, 3, 
               2, 1, 1, 1, 1)
PID_list_6 <- c("101", "102", "103", "104", "105", "106", "107", "109", "110",
                "111", "115", "116", "117", "118", "119",
                "120", "121", "122", "124", "125", "126", "127", "128", "129",
                "130", "131", "132", "133", "134", "135", "136", "137")
for (i in 1:length(cond_list)) {
  v6sim_df$cond[i] <- cond_list[i]
}
v6sim_df$cond <- as.factor(v6sim_df$cond)
for (i in 1:length(PID_list_6)) {
  v6sim_df$PID[i] <- PID_list_6[i]
}
test <- lm(v6sim ~ cond, data = v6sim_df)
summary(test)
plot(effect("cond", test), grid = TRUE)

## video 7 similarity ## no 102
v7sim_df <- data.frame(v7sim)
v7sim_df$cond <- NA
v7sim_df$vid <- NA
v7sim_df$vid <- "v7"
table(vid7$PID_vec, vid7$cond_vec)
cond_list <- c(1, 2, 1, 3, 2, 3, 1, 2, 3, 3, 3, 3, 2, 
               2, 2, 3, 2, 2, 2, 3, 1, 2, 1, 2, 1, 3, 
               3, 2, 1, 2, 2, 3, 3, 3)
PID_list_7 <- c("101", "103", "104", "105", "106", "107", "109", "110",
                "111", "113", "114", "115", "116", "117", "118", "119",
                "120", "121", "122", "124", "125", "126", "127", "128", "129",
                "130", "131", "132", "133", "134", "135", "136", "137")
for (i in 1:length(cond_list)) {
  v7sim_df$cond[i] <- cond_list[i]
}
v7sim_df$cond <- as.factor(v7sim_df$cond)
test <- lm(v7sim ~ cond, data = v7sim_df)
for (i in 1:length(PID_list_7)) {
  v7sim_df$PID[i] <- PID_list_7[i]
}
summary(test)
plot(effect("cond", test), grid = TRUE)

## video 8 similarity
v8sim_df <- data.frame(v8sim)
v8sim_df$cond <- NA
v8sim_df$vid <- NA
v8sim_df$vid <- "v8"
table(vid8$PID_vec, vid8$cond_vec)
cond_list <- c(1, 1, 3, 3, 2, 1, 3, 2, 1, 3, 2, 1, 
               3, 3, 3, 2, 2, 1, 2, 1, 1, 1, 3, 3, 
               2, 2, 3, 3, 3, 1, 3, 2, 2)
PID_list_8 <- c("101", "102", "103", "104", "105", "106", "107", "109", "110",
                "111", "113", "114", "115", "116", "117", "118", "119",
                "120", "121", "122", "124", "125", "126", "127", "128", "129",
                "130", "131", "132", "133", "134", "135", "136", "137")
for (i in 1:length(cond_list)) {
  v8sim_df$cond[i] <- cond_list[i]
}
v8sim_df$cond <- as.factor(v8sim_df$cond)
for (i in 1:length(PID_list_8)) {
  v8sim_df$PID[i] <- PID_list_8[i]
}
test <- lm(v8sim ~ cond, data = v8sim_df)
summary(test)
plot(effect("cond", test), grid = TRUE)

## video 9 similarity ## no 122
v9sim_df <- data.frame(v9sim)
v9sim_df$cond <- NA
v9sim_df$vid <- NA
v9sim_df$vid <- "v9"
table(vid9$PID_vec, vid9$cond_vec)
cond_list <- c(3, 2, 1, 2, 2, 3, 1, 1, 3, 
               1, 1, 2, 1, 2, 1, 1, 2, 3, 
               1, 3, 3, 2, 3, 1, 3, 2, 3, 
               1, 3, 1, 2, 2, 2)
PID_list_9 <- c("101", "102", "103", "104", "105", "106", "107", "109", "110",
                "111", "113", "114", "115", "116", "117", "118", "119",
                "120", "121", "124", "125", "126", "127", "128", "129",
                "130", "131", "132", "133", "134", "135", "136", "137")
for (i in 1:length(cond_list)) {
  v9sim_df$cond[i] <- cond_list[i]
}
v9sim_df$cond <- as.factor(v9sim_df$cond)
for (i in 1:length(PID_list_9)) {
  v9sim_df$PID[i] <- PID_list_9[i]
}
test <- lm(v9sim ~ cond, data = v9sim_df)
summary(test)
plot(effect("cond", test), grid = TRUE)

## video 1 similarity
# v1sim_df <- data.frame(v1sim)
# v1sim_df$cond <- NA
# v1sim_df$vid <- NA
# v1sim_df$PID <- NA
# v1sim_df$vid <- "v1"
# table(vid1$PID_vec, vid1$cond_vec)
PID_list <- c("101", "102", "103", "104", "105", 
                 "106", "107", "109", "110", "111", 
                 "112", "113", "114", "115", "116", 
                 "117", "118", "119", "120", "121", 
                 "122", "124", "125", "126", "127", 
                 "128", "129", "130", "131", "132", 
                 "133", "134", "135", "136", "137")
cond_list <- c(2, 3, 3, 3, 3, 
                  1, 1, 2, 1, 3, 
                  1, 3, 2, 3, 3, 
                  1, 2, 3, 3, 1, 
                  1, 2, 3, 1, 1, 
                  3, 1, 3, 2, 3, 
                  2, 1, 3, 1, 3)
## subset correlation dataset based on condition
corFrame1_detectives <- corFrame1 %>% 
  select(V6, V7, V9, V11, V16, V19, V20, V23, V24, V27, V32, V34)
corFrame1_victim <- corFrame1 %>% 
  select(V1, V8, V13, V17, V22, V29, V31)
corFrame1_accused <- corFrame1 %>% 
  select(V2, V3, V4, V5, V10, V12, V14, V15, V18, V19, V23, V26, V28, V30, V33, V35)

## compute overall timecourse similarity
det_sim <- rowMeans(corFrame1_detectives)
vic_sim <- rowMeans(corFrame1_victim)
acc_sim <- rowMeans(corFrame1_accused)

## combine those into omnibus dataframe
vid1_sim_check <- data.frame(PID_list, cond_list, det_sim, acc_sim, vic_sim)

## adjust to remove self-correlations
sim_det_adj <- 
  vid1_sim_check %>%
  filter(cond_list == 1) %>% 
  mutate(det_sim = det_sim - (1/35))
sim_vic_adj <- 
  vid1_sim_check %>%
  filter(cond_list == 2) %>% 
  mutate(vic_sim = vic_sim - (1/35))
sim_acc_adj <- 
  vid1_sim_check %>%
  filter(cond_list == 3) %>% 
  mutate(acc_sim = acc_sim - (1/35))

# bind adjusted frames together, make condition a factor
v1_sim_adj <- rbind(sim_det_adj, sim_vic_adj, sim_acc_adj)
v1_sim_adj$cond_list <- as.factor(v1_sim_adj$cond_list)

# pivot the data from wide to long
v1_sim_adj_long <- v1_sim_adj %>% 
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
# v2sim_df <- data.frame(v2sim)
# v2sim_df$cond <- NA
# v2sim_df$vid <- NA
# v2sim_df$PID <- NA
# v2sim_df$vid <- "v1"
# table(vid2$PID_vec, vid2$cond_vec)
PID_list <- c("101", "102", "103", "104", "105", 
                 "106", "107", "109", "110", "111", 
                 "113", "114", "115", "116", "117", 
                 "118", "119", "120", "121", "122", 
                 "124", "125", "126", "127", "128", 
                 "129", "130", "131", "132", "133", 
                 "134", "135", "136", "137")
cond_list <- c(2, 1, 3, 2, 1, 
                  2, 3, 1, 2, 1, 
                  1, 3, 2, 1, 1, 
                  1, 1, 1, 2, 3, 
                  2, 1, 2, 1, 1, 
                  3, 1, 1, 3, 2, 
                  3, 2, 1, 2)
## subset correlation dataset based on condition
corFrame2_detectives <- corFrame2 %>% 
  select(V2, V5, V8, V10, V11, V14, V15, V16, V17, V18, V22, V24, V25, V27, V28, V33)
corFrame2_victim <- corFrame2 %>% 
  select(V1, V4, V9, V13, V19, V21, V23, V30, V32, V34)
corFrame2_accused <- corFrame2 %>% 
  select(V3, V7, V12, V20, V26, V29, V31)

## compute overall timecourse similarity
det_sim <- rowMeans(corFrame2_detectives)
vic_sim <- rowMeans(corFrame2_victim)
acc_sim <- rowMeans(corFrame2_accused)

## combine those into omnibus dataframe
vid2_sim_check <- data.frame(PID_list, cond_list, det_sim, acc_sim, vic_sim)

## adjust to remove self-correlations
sim_det_adj <- 
  vid2_sim_check %>%
  filter(cond_list == 1) %>% 
  mutate(det_sim = det_sim - (1/35))
sim_vic_adj <- 
  vid2_sim_check %>%
  filter(cond_list == 2) %>% 
  mutate(vic_sim = vic_sim - (1/35))
sim_acc_adj <- 
  vid2_sim_check %>%
  filter(cond_list == 3) %>% 
  mutate(acc_sim = acc_sim - (1/35))

# bind adjusted frames together, make condition a factor
v2_sim_adj <- rbind(sim_det_adj, sim_vic_adj, sim_acc_adj)
v2_sim_adj$cond_list <- as.factor(v2_sim_adj$cond_list)

# pivot the data from wide to long
v2_sim_adj_long <- v2_sim_adj %>% 
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

sim_adj_long <- rbind(v1_sim_adj_long, v2_sim_adj_long)

# quick visualization check (remove for primetime)
test <- lmer(sim ~ target_cond * cond_list + (1|PID_list) + (1|vidNum), data = sim_adj_long)
summary(test)
library(effects)
plot(effect("target_cond:cond_list", test), grid = TRUE)




for (i in 1:length(cond_list)) {
  v1sim_df$cond[i] <- cond_list[i]
}
v1sim_df$cond <- as.factor(v1sim_df$cond)
for (i in 1:length(PID_list_1)) {
  v1sim_df$PID[i] <- PID_list_1[i]
}
test <- lm(v1sim ~ cond, data = v1sim_df)
summary(test)
plot(effect("cond", test), grid = TRUE)



v1sim_df$sim <- v1sim_df$v1sim
v2sim_df$sim <- v2sim_df$v2sim
v3sim_df$sim <- v3sim_df$v3sim
v4sim_df$sim <- v4sim_df$v4sim
v5sim_df$sim <- v5sim_df$v5sim
v6sim_df$sim <- v6sim_df$v6sim
v7sim_df$sim <- v7sim_df$v7sim
v8sim_df$sim <- v8sim_df$v8sim
v9sim_df$sim <- v9sim_df$v9sim

v1sim_df$v1sim <- NULL
v2sim_df$v2sim <- NULL
v3sim_df$v3sim <- NULL
v4sim_df$v4sim <- NULL
v5sim_df$v5sim <- NULL
v6sim_df$v6sim <- NULL
v7sim_df$v7sim <- NULL
v8sim_df$v8sim <- NULL
v9sim_df$v9sim <- NULL

total_sim <- 
  rbind(
    v1sim_df, v2sim_df,
      v3sim_df, v4sim_df,
      v5sim_df, v6sim_df,
      v7sim_df, v8sim_df,
      v9sim_df
    )

## mlm model for check
test <- lmer(sim ~ cond + (1|PID) + (1|vid), data = total_sim)
summary(test)
plot(effect("cond", test), grid = TRUE)

total_sim$cond <- relevel(total_sim$cond, ref = "1")

prior1 <- c(prior(normal(0,.10), class = b),
                      prior(student_t(3, -12.4, 23.5), class = Intercept),
                      prior(student_t(3, 0, 23.5), class = sd),
                      prior(student_t(3, 0, 23.5), class = sigma)
)

bmod1 <- brm(
  sim ~ cond + (1|PID) + (1|vid),
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
                        "sd_PID__Intercept"), lags = 8)
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
  transmute(Detective = (b_Intercept + b_cond1),
            `Friend of Victim` = (b_Intercept),
            `Friend of Accused` = (b_Intercept + b_cond3)) %>% 
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


## set null model
prior_n <- get_prior(sim ~ cond,
                    data = total_sim, family = gaussian())

bmod_n <- brm(
  sim ~ cond,
  data = total_sim, family = gaussian(),
  prior = prior_n,
  warmup = 3000, iter = 10000,
  control = list(adapt_delta = 0.99)
)
bmod_n

prior_n1 <- get_prior(sim ~ cond + (1|PID),
                     data = total_sim, family = gaussian())

bmod_n1 <- brm(
  sim ~ cond + (1|PID),
  data = total_sim, family = gaussian(),
  prior = prior_n1,
  warmup = 3000, iter = 10000,
  control = list(adapt_delta = 0.99)
)
bmod_n1

## model comparison
mod.output <- LOO(bmod1, bmod_n, bmod_n1)


## heatmap for similarity

cond <- total_sim$cond
PID <- total_sim$PID
sim <- total_sim$PID
total_sim_mat <- total_sim
total_sim_mat$vid <- NULL
total_sim_mat$cond <- as.numeric(total_sim_mat$cond)
total_sim_mat$PID <- as.numeric(total_sim_mat$PID)
total_sim_mat <- as.matrix(total_sim_mat)


heatmap(total_sim_mat, scale = "column")



  
