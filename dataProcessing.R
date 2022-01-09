rm(list=ls())

library(skimr)
library(dplyr)
library(tidyr)
library(splitstackshape)
library(ggplot2)
library(plotly)
library(tidyverse)
library(effects)
library(lme4)
library(lmerTest)
library(stringr)

log_data <- read.delim("/Users/tua37526/Dropbox/certainty/data/SAN117/SAN117_Certainty Task-Joy_2021_Sep_10_1208.txt")

## rename columns
log_data <- as.data.frame(log_data)
colNames <- c("Timing", "Type", "Stim")
colnames(log_data) <- colNames

## isolate stim order
showStim <- grep("filename=", log_data$Stim)
movieOrder <- log_data[showStim,3]
res <- str_match(movieOrder, "filename='(.*?)'")
stimOrder <- res[,2]
stimTiming <- log_data[showStim,1]
res2 <- grep("Set VidClip finished", log_data$Stim)
stimTimingEnd <- log_data[res2,1]

## isolate cond info
condStim <- grep("Imagine", log_data$Stim)
condOrder <- log_data[condStim,3]
condRes <- str_match(condOrder, "Imagine that you are a (.*?)'")
condSelect <- condRes[,2]
condSelect <- condSelect[-c(1,3,5,7,9,11,13,15,17)]

## extract certainty ratings
certainRatings <- grep("% certain", log_data$Stim)
certainOrder <- log_data[certainRatings,3]
ratingRes <- str_match(certainOrder, "'(.*?)%")
ratingResOrder <- ratingRes[,2]
ratingResOrder <- as.numeric(ratingResOrder)

## extract certainty timing
certainTiming <- log_data[certainRatings, 1]

## extract rating direction
directionRatings <- grep("pos = array", log_data$Stim)
directionOrder <- log_data[directionRatings,3]
directionRes <- str_match(directionOrder, ",(.*?)]")
directionResOrder <- directionRes[,2]
directionResOrder <- as.numeric(directionResOrder)

## extract direction timing
directionTiming <- log_data[directionRatings, 1]

##remove certainty duplicates
k <- duplicated(certainTiming)
duplicates <- which(k == 1)
certainTiming2 <- certainTiming[-c(duplicates)]

## delete first certain rating (false 0)
duplicates2 <- duplicates - 1
ratingResOrder2 <- ratingResOrder[-c(duplicates2)]

## create data frame
 df <- data.frame(certainTiming2, directionTiming, ratingResOrder2, directionResOrder)
 df$ratingDirection[df$directionResOrder < 0] <- -1
 df$ratingDirection[df$directionResOrder > 0] <- 1
 df$ratingDirection[df$directionResOrder == 0] <- 0
 df$ratingFinal <- df$ratingResOrder2 * df$ratingDirection
 
 ## collapse data frame in second intervals
 df$certainTiming2 <- format(round( df$certainTiming2, 0))
 df$directionTiming2 <- format(round( df$directionTiming, 0))
 df2 <- df %>% 
   dplyr::group_by(certainTiming2) %>% 
   dplyr::summarise(meanRating = mean(ratingFinal), n = n())
df2 <- cbind(secTiming = 1:nrow(df2), df2)
df2$certainTiming2 <- as.numeric(df2$certainTiming2)

## paste video numbers
df2$videoNum[df2$certainTiming2 >= stimTiming[1] & df2$certainTiming2 <= stimTimingEnd[1]] <- 1
df2$videoNum[df2$certainTiming2 >= stimTiming[2] & df2$certainTiming2 <= stimTimingEnd[2]] <- 2
df2$videoNum[df2$certainTiming2 >= stimTiming[3] & df2$certainTiming2 <= stimTimingEnd[3]] <- 3
df2$videoNum[df2$certainTiming2 >= stimTiming[4] & df2$certainTiming2 <= stimTimingEnd[4]] <- 4
df2$videoNum[df2$certainTiming2 >= stimTiming[5] & df2$certainTiming2 <= stimTimingEnd[5]] <- 5
df2$videoNum[df2$certainTiming2 >= stimTiming[6] & df2$certainTiming2 <= stimTimingEnd[6]] <- 6
df2$videoNum[df2$certainTiming2 >= stimTiming[7] & df2$certainTiming2 <= stimTimingEnd[7]] <- 7
df2$videoNum[df2$certainTiming2 >= stimTiming[8] & df2$certainTiming2 <= stimTimingEnd[8]] <- 8
df2$videoNum[df2$certainTiming2 >= stimTiming[9] & df2$certainTiming2 <= stimTimingEnd[9]] <- 9

## paste video name 
df2$videoName[df2$videoNum == 1] <- stimOrder[1]
df2$videoName[df2$videoNum == 2] <- stimOrder[2]
df2$videoName[df2$videoNum == 3] <- stimOrder[3]
df2$videoName[df2$videoNum == 4] <- stimOrder[4]
df2$videoName[df2$videoNum == 5] <- stimOrder[5]
df2$videoName[df2$videoNum == 6] <- stimOrder[6]
df2$videoName[df2$videoNum == 7] <- stimOrder[7]
df2$videoName[df2$videoNum == 8] <- stimOrder[8]
df2$videoName[df2$videoNum == 9] <- stimOrder[9]

## paste condition
df2$condName[df2$videoNum == 1] <- condSelect[1]
df2$condName[df2$videoNum == 2] <- condSelect[2]
df2$condName[df2$videoNum == 3] <- condSelect[3]
df2$condName[df2$videoNum == 4] <- condSelect[4]
df2$condName[df2$videoNum == 5] <- condSelect[5]
df2$condName[df2$videoNum == 6] <- condSelect[6]
df2$condName[df2$videoNum == 7] <- condSelect[7]
df2$condName[df2$videoNum == 8] <- condSelect[8]
df2$condName[df2$videoNum == 9] <- condSelect[9]
df2$condName <- as.factor(df2$condName)

## write .csv file
 write.csv(df2, "/Users/tua37526/Dropbox/certainty/data/SAN117/117log.csv")

vid1 <- subset(df2, videoNum == 1)
p <- vid1 %>%
   ggplot(aes(x=secTiming, y=meanRating)) +
   geom_area(fill="#69b3a2", alpha=0.5) +
   geom_line(color="#69b3a2") +
   ylab("certainty") +
   ylim(-100, 100) +
   theme_bw() +
  ggtitle(vid1$condName[1],
          subtitle = vid1$videoName[1])
 p
 
 


# 
# 
# 
# 
# 
# 
# dat101 <-
#   read.csv("/Users/tua37526/Dropbox/certainty/data/SAN101_Certainty Task-Joy_2021_Apr_26_1546.csv")
# FR_101 <- dat101$frameRate
# 
# dat102 <-
#   read.csv("/Users/tua37526/Dropbox/certainty/data/san102_Certainty Task-Joy_2021_Apr_27_1227.csv")
# FR_102 <- dat102$frameRate
# 
# dat103 <-
#   read.csv("/Users/tua37526/Dropbox/certainty/data/san103_Certainty Task-Joy_2021_Apr_28_1337.csv")
# FR_103 <- dat103$frameRate
# 
# dat104 <-
#   read.csv("/Users/tua37526/Dropbox/certainty/data/san104_Certainty Task-Joy_2021_May_03_0903.csv")
# FR_104 <- dat104$frameRate
# 
# dat105 <-
#   read.csv("/Users/tua37526/Dropbox/certainty/data/san105_Certainty Task-Joy_2021_May_03_1450.csv")
# FR_105 <- dat105$frameRate
# 
# dat106 <-
#   read.csv("/Users/tua37526/Dropbox/certainty/data/SAN106_Certainty Task-Joy_2021_May_12_0918.csv")
# FR_106 <- dat106$frameRate
# 
# dat107 <-
#   read.csv("/Users/tua37526/Dropbox/certainty/data/san107_Certainty Task-Joy_2021_Jul_06_1304.csv")
# FR_107 <- dat107$frameRate
# 
# 
# 
# 
# 
# 
# 
# dat1 <- dat1[-c(1:6),]
# dat1$InstText <- NULL
# 
# dat2 <- dat1 %>% 
#   dplyr::mutate(index = row_number()) %>% 
#   cSplit('Total.Certainty.Ratings', sep=",", type.convert=TRUE) %>% 
#   group_by(index)
# 
# dat3 <- dat1 %>% 
#   dplyr::mutate(index = row_number()) %>% 
#   cSplit('Certainty.Status', sep=",", type.convert=TRUE) %>% 
#   group_by(index)
# 
# ## STOP HERE
# group <- dat2[,c(42:27953)]
# 
# group2 <- t(group)
# group2 <- group2[,colSums(is.na(group2))<nrow(group2)]
# group2 <- as.data.frame(group2)
# 
# group2$V1 <-gsub("'", "", group2$V1)
# group2$V2 <-gsub("'", "", group2$V2)
# group2$V3 <-gsub("'", "", group2$V3)
# group2$V4 <-gsub("'", "", group2$V4)
# group2$V5 <-gsub("'", "", group2$V5)
# group2$V6 <-gsub("'", "", group2$V6)
# group2$V7 <-gsub("'", "", group2$V7)
# group2$V8 <-gsub("'", "", group2$V8)
# group2$V9 <-gsub("'", "", group2$V9)
# 
# k <- colSums(!is.na(group2))
# k[1]
# 
# group2[1,] <- 0
# 
# ## STOP HERE
# print(group2$V1[k[1]])
# group2$V1[k[1]] <- -77
# 
# print(group2$V2[k[2]])
# group2$V2[k[2]] <- -50
# 
# print(group2$V3[k[3]])
# group2$V3[k[3]] <- 9
# 
# print(group2$V4[k[4]])
# group2$V4[k[4]] <- -13
# 
# print(group2$V5[k[5]])
# group2$V5[k[5]] <- 4
# 
# print(group2$V6[k[6]])
# group2$V6[k[6]] <- -88
# 
# print(group2$V7[k[7]])
# group2$V7[k[7]] <- 17
# 
# print(group2$V8[k[8]])
# group2$V8[k[8]] <- -39
# 
# print(group2$V9[k[9]])
# group2$V9[k[9]] <- -5
# 
# group2$V1 <- as.numeric(group2$V1)
# group2$V2 <- as.numeric(group2$V2)
# group2$V3 <- as.numeric(group2$V3)
# group2$V4 <- as.numeric(group2$V4)
# group2$V5 <- as.numeric(group2$V5)
# group2$V6 <- as.numeric(group2$V6)
# group2$V7 <- as.numeric(group2$V7)
# group2$V8 <- as.numeric(group2$V8)
# group2$V9 <- as.numeric(group2$V9)
# 
# group2 <- group2 %>% mutate(time=1:n())
# 
# m <- dat1$VideoClip[1]
# p1 <- group2 %>%
#   ggplot( aes(x=time, y=V1)) +
#   geom_area(fill="#69b3a2", alpha=0.5) +
#   geom_line(color="#69b3a2") +
#   ylab("certainty") +
#   xlim(0,k[1]) +
#   ylim(-100, 100) +
#   theme_bw() +
#   ggtitle(dat1$VideoClip[1],
#           subtitle = dat1$RoleText[1])
# p1
# ggsave("/Users/tua37526/Dropbox/certainty/data/SAN129/V1.jpg", p1)
# 
# p2 <- group2 %>%
#   ggplot( aes(x=time, y=V2)) +
#   geom_area(fill="#69b3a2", alpha=0.5) +
#   geom_line(color="#69b3a2") +
#   ylab("certainty") +
#   xlim(0,k[2]) +
#   ylim(-100, 100) +
#   theme_bw() +
#   ggtitle(dat1$VideoClip[21],
#           subtitle = dat1$RoleText[15])
# p2
# ggsave("/Users/tua37526/Dropbox/certainty/data/SAN129/V2.jpg", p2)
# 
# p3 <- group2 %>%
#   ggplot( aes(x=time, y=V3)) +
#   geom_area(fill="#69b3a2", alpha=0.5) +
#   geom_line(color="#69b3a2") +
#   ylab("certainty") +
#   xlim(0,k[3]) +
#   ylim(-100, 100) +
#   theme_bw() +
#   ggtitle(dat1$VideoClip[31],
#           subtitle = dat1$RoleText[29])
# p3
# ggsave("/Users/tua37526/Dropbox/certainty/data/SAN129/V3.jpg", p3)
# 
# p4 <- group2 %>%
#   ggplot( aes(x=time, y=V4)) +
#   geom_area(fill="#69b3a2", alpha=0.5) +
#   geom_line(color="#69b3a2") +
#   ylab("certainty") +
#   xlim(0,k[4]) +
#   ylim(-100, 100) +
#   theme_bw() +
#   ggtitle(dat1$VideoClip[43],
#           subtitle = dat1$RoleText[43])
# p4
# ggsave("/Users/tua37526/Dropbox/certainty/data/SAN129/V4.jpg", p4)
# 
# p5 <- group2 %>%
#   ggplot( aes(x=time, y=V5)) +
#   geom_area(fill="#69b3a2", alpha=0.5) +
#   geom_line(color="#69b3a2") +
#   ylab("certainty") +
#   xlim(0,k[5]) +
#   ylim(-100, 100) +
#   theme_bw() +
#   ggtitle(dat1$VideoClip[58],
#           subtitle = dat1$RoleText[57])
# p5
# ggsave("/Users/tua37526/Dropbox/certainty/data/SAN129/V5.jpg", p5)
# 
# p6 <- group2 %>%
#   ggplot( aes(x=time, y=V6)) +
#   geom_area(fill="#69b3a2", alpha=0.5) +
#   geom_line(color="#69b3a2") +
#   ylab("certainty") +
#   xlim(0,k[6]) +
#   ylim(-100, 100) +
#   theme_bw() +
#   ggtitle(dat1$VideoClip[73],
#           subtitle = dat1$RoleText[71])
# p6
# ggsave("/Users/tua37526/Dropbox/certainty/data/SAN129/V6.jpg", p6)
# 
# p7 <- group2 %>%
#   ggplot( aes(x=time, y=V7)) +
#   geom_area(fill="#69b3a2", alpha=0.5) +
#   geom_line(color="#69b3a2") +
#   ylab("certainty") +
#   xlim(0,k[7]) +
#   ylim(-100, 100) +
#   theme_bw() +
#   ggtitle(dat1$VideoClip[85],
#           subtitle = dat1$RoleText[85])
# p7
# ggsave("/Users/tua37526/Dropbox/certainty/data/SAN129/V7.jpg", p7)
# 
# p8 <- group2 %>%
#   ggplot( aes(x=time, y=V8)) +
#   geom_area(fill="#69b3a2", alpha=0.5) +
#   geom_line(color="#69b3a2") +
#   ylab("certainty") +
#   xlim(0,k[8]) +
#   ylim(-100, 100) +
#   theme_bw() +
#   ggtitle(dat1$VideoClip[100],
#           subtitle = dat1$RoleText[99])
# p8
# ggsave("/Users/tua37526/Dropbox/certainty/data/SAN129/V8.jpg", p8)
# 
# p9 <- group2 %>%
#   ggplot( aes(x=time, y=V9)) +
#   geom_area(fill="#69b3a2", alpha=0.5) +
#   geom_line(color="#69b3a2") +
#   ylab("certainty") +
#   xlim(0,k[9]) +
#   ylim(-100, 100) +
#   theme_bw() +
#   ggtitle(dat1$VideoClip[115],
#           subtitle = dat1$RoleText[113])
# p9
# ggsave("/Users/tua37526/Dropbox/certainty/data/SAN129/V9.jpg", p9)
# 
# columnNames <- c(dat1$VideoClip[1], dat1$VideoClip[21], dat1$VideoClip[31],
#                  dat1$VideoClip[43], dat1$VideoClip[58], dat1$VideoClip[73],
#                  dat1$VideoClip[85], dat1$VideoClip[100], dat1$VideoClip[115],
#                  "Time")
# colnames(group2) <- columnNames
# 
# summaryDF <- group2 %>% 
#   dplyr::summarise_all(mean, na.rm = TRUE)
# summaryDF <- as.data.frame(summaryDF)
# condNames <- c(dat1$RoleText[1], dat1$RoleText[15], dat1$RoleText[29], dat1$RoleText[43],
#                dat1$RoleText[57], dat1$RoleText[71], dat1$RoleText[85], dat1$RoleText[99],
#                dat1$RoleText[113], "Time")
# summaryDF[2,] <- condNames
# 
# ##STOP HERE
# summaryDF[3,] <- 129
# summaryDF <- t(summaryDF)
# summaryDF <- as.data.frame(summaryDF)
# summaryDF <- rownames_to_column(summaryDF, var = "movie")
# columnNames <- c("movie", "meanCertainty", "Condition", "PID")
# colnames(summaryDF) <- columnNames
# summaryDF <- summaryDF[-c(10),]
# 
# write.csv(group2, "/Users/tua37526/Dropbox/certainty/data/SAN129/SAN129.csv")
# write.csv(summaryDF, "/Users/tua37526/Dropbox/certainty/data/SAN129/SAN129_summary.csv")
# 
# rm(list=ls())
# 
# summ <- read.csv("/Users/tua37526/Dropbox/certainty/data/Subs_summary.csv")
# summ$Condition <- as.factor(summ$Condition)
# 
# test <- lmer(meanCertainty ~ Condition + promptType + (1|PID) + (1|movie), data = summ)
# summary(test)
# plot(effect("Condition", test), grid = TRUE)