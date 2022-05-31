rm(list=ls())

library(corrplot)
library(tidyverse)
library(purrr)
library(magrittr)
library(effects)

## load dataset and select columns of interest: Participant ID, certainty rating
## condition assignment (1 = detective, 2 = friend of victim, 3 = friend of
## accused), and video
dat <- read.csv('/Users/tua37526/Dropbox/certainty/data/summary.csv')
dat <- dat %>% 
  filter(n >= 50)
dat2 <- dat %>% 
  select(PID, meanRating, condName3, videoName2, secTiming, n) %>% 
  group_by(PID,videoName2) %>% dplyr::mutate(id = seq_len(n()))
dat3 <- dat2 %>% 
  group_by(videoName2)
k <- group_split(dat3)
k %>% unnest_wider(k)

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
v1sim <- rowMeans(corFrame1)
v2sim <- rowMeans(corFrame2)
v3sim <- rowMeans(corFrame3)
v4sim <- rowMeans(corFrame4)
v5sim <- rowMeans(corFrame5)
v6sim <- rowMeans(corFrame6)
v7sim <- rowMeans(corFrame7)
v8sim <- rowMeans(corFrame8)
v9sim <- rowMeans(corFrame9)


## video 1 similarity
v1sim_df <- data.frame(v1sim)
v1sim_df$cond <- NA
v1sim_df$vid <- NA
v1sim_df$PID <- NA
v1sim_df$vid <- "v1"
table(vid1$PID_vec, vid1$cond_vec)
PID_list <- c("101", "102", "103", "104", "105", "106", "107", "109", "110",
               "111", "112", "113", "114", "115", "116", "117", "118", "119",
               "120", "121", "122", "124", "125", "126", "127", "128", "129",
               "130", "131", "132", "133", "134", "135", "136", "137")
cond_list <- c(2, 3, 3, 3, 3, 1, 1, 2, 1, 3, 1, 3, 
               2, 3, 3, 1, 2, 3, 3, 1, 1, 2, 3, 1, 
               1, 3, 1, 3, 2, 3, 2, 1, 3, 1, 3)
for (i in 1:length(cond_list)) {
  v1sim_df$cond[i] <- cond_list[i]
}
v1sim_df$cond <- as.factor(v1sim_df$cond)
for (i in 1:length(PID_list)) {
  v1sim_df$PID[i] <- PID_list[i]
}
test <- lm(v1sim ~ cond, data = v1sim_df)
summary(test)
plot(effect("cond", test), grid = TRUE)

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
PID_list <- c("101", "102", "103", "104", "105", "106", "107", "109", "110",
              "111", "113", "114", "115", "116", "117", "118", "119",
              "120", "121", "122", "124", "125", "126", "127", "128", "129",
              "130", "131", "132", "133", "134", "135", "136", "137")
for (i in 1:length(cond_list)) {
  v2sim_df$cond[i] <- cond_list[i]
}
v2sim_df$cond <- as.factor(v2sim_df$cond)
for (i in 1:length(PID_list)) {
  v2sim_df$PID[i] <- PID_list[i]
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
for (i in 1:length(cond_list)) {
  v3sim_df$cond[i] <- cond_list[i]
}
v3sim_df$cond <- as.factor(v3sim_df$cond)
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
for (i in 1:length(cond_list)) {
  v4sim_df$cond[i] <- cond_list[i]
}
v4sim_df$cond <- as.factor(v4sim_df$cond)
test <- lm(v4sim ~ cond, data = v4sim_df)
summary(test)
plot(effect("cond", test), grid = TRUE)




  
