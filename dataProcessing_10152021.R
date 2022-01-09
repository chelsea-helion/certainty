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

log_data <- read.delim("/Users/tua37526/Dropbox/certainty/data/SAN129/SAN129.txt")
PID <- 129

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
for (i in 1:9) {
  (df2$videoNum[df2$certainTiming2 >= stimTiming[i] & df2$certainTiming2 <= stimTimingEnd[i]] <- i)
  print(i)
}

## paste video name
for (i in 1:9) {
  (df2$videoName[df2$videoNum == i] <- stimOrder[i])
  print(i)
}

## paste condition
for (i in 1:9) {
  (df2$condName[df2$videoNum == i] <- condSelect[i])
  print(i)
}

## write .csv file
df2$PID <- PID
write.csv(df2, "/Users/tua37526/Dropbox/certainty/data/csvProcessed/129log.csv")

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