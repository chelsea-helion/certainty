rm(list=ls())

library(corrplot)
library(tidyverse)
library(purrr)
library(magrittr)

## load dataset and select columns of interest: Participant ID, certainty rating
## condition assignment (1 = detective, 2 = friend of victim, 3 = friend of
## accused), and video
dat <- read.csv('/Users/tua37526/Dropbox/certainty/data/summary.csv')
dat <- dat %>% 
  filter(n >= 50)
dat2 <- dat %>% 
  select(PID, meanRating, condName3, videoName2, secTiming, n) %>% 
  group_by(PID,videoName2) %>% mutate(id = seq_len(n()))
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
               "120", "121", "122", "124", "125", "126", "127", "128", "129")
## assign mean length values per video (50 samples per second)
vidLength <- c(464, 281, 364, 230, 291, 418, 342, 201, 281)

for (i in 1:9) {
PID_vec <- (k[[i]]$PID)
index_vec <- (k[[i]]$id)
cond_vec <- (k[[i]]$condName3)
meanRating_vec<- (k[[i]]$meanRating)
assign(paste0("vid", i),
       data.frame(PID_vec, index_vec, cond_vec, meanRating_vec))
}

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







#create separate dataframes for each PID
for (i in seq_along(PID_names)) {
  for (j in seq_along(vid_Names)) {
    assign(paste0("PID",i, "vid", j),
           filter(k, PID == PID_names[i] & videoName2 == vid_Names[j]))
  }
}


for (i in seq_along(PID_names)) {
k <- PID[i]vid1[1:vidLength[1],]
assign(paste0("PID",i, "vid", j),
       filter(dat2, PID == PID_names[i] & videoName2 == vid_Names[j]))
}




## subset based on condition
detective <- dat2 %>% 
  group_by(PID, videoName2) %>% 
  filter(condName3 == 1) %>% 
  #pivot_wider(names_from = condName3, values_from = meanRating)


victim <- dat2 %>% 
  group_by(PID) %>% 
  filter(condName3 == 2) %>% 
  pivot_wider(names_from = videoName2, values_from = meanRating)
accused <- dat2 %>% 
  group_by(PID) %>% 
  filter(condName3 == 3) %>% 
  pivot_wider(names_from = videoName2, values_from = meanRating)

mindhunter_s1E3_D <- compact(detective$mindhunterS1E3)
mindhunter_s1E3_D[[5]] <- mindhunter_s1E3_D[[5]][1:293]
mindhunter_s1E3_D[[6]] <- mindhunter_s1E3_D[[6]][1:293]
mindhunter_s1E3_D[[7]] <- mindhunter_s1E3_D[[7]][1:293]
mindhunter_s1E3_V <- compact(victim$mindhunterS1E3)
mindhunter_s1E3_V[[1]] <- mindhunter_s1E3_V[[1]][1:293]
mindhunter_s1E3_A <- compact(accused$mindhunterS1E3)

m <- cor(as.data.frame(mindhunter_s1E3_A))
corrplot(m, method = 'color', tl.pos='n',order = 'hclust')

mindhunter_s2E9_D <- compact(detective$mindhunterS2E9)
mindhunter_s2E9_D[[2]] <- NULL
mindhunter_s2E9_V <- compact(victim$mindhunterS2E9)
mindhunter_s2E9_V[[1]] <- mindhunter_s2E9_V[[1]][1:343]
mindhunter_s2E9_V[[2]] <- mindhunter_s2E9_V[[2]][1:343]
mindhunter_s2E9_V[[3]] <- mindhunter_s2E9_V[[3]][1:343]
mindhunter_s2E9_V[[4]] <- mindhunter_s2E9_V[[4]][1:343]
mindhunter_s2E9_V[[5]] <- mindhunter_s2E9_V[[5]][1:343]
mindhunter_s2E9_V[[6]] <- mindhunter_s2E9_V[[6]][1:343]
mindhunter_s2E9_V[[4]] <- mindhunter_s2E9_V[[4]][1:343]
mindhunter_s2E9_V[[7]] <- mindhunter_s2E9_V[[7]][1:343]
mindhunter_s2E9_V[[8]] <- mindhunter_s2E9_V[[8]][1:343]
mindhunter_s2E9_V[[10]] <- mindhunter_s2E9_V[[10]][1:343]
mindhunter_s2E9_V[[11]] <- mindhunter_s2E9_V[[11]][1:343]
mindhunter_s2E9_A <- compact(accused$mindhunterS2E9)
mindhunter_s2E9_A[[6]] <- mindhunter_s2E9_A[[6]][1:344]
m <- cor(as.data.frame(mindhunter_s2E9_V))
corrplot(m, method = 'color', tl.pos='n', order = 'hclust')


  

dat4 <- dat3 %>% 
  select[,1:3]
dat4$mindhunterS1E3<-compact(dat4$mindhunterS1E3)

for (i in 1:30) {
  for (j in 2:30) {
  if (dat3$mindhunterS1E3[[i]][1] > -1 & dat3$mindhunterS1E3[[j]][1] > -1)
    cor(dat3$mindhunterS1E3[[i]], dat3$mindhunterS1E3[[j]])
  else {
    j = j + 1
    }
  }
}



starwars %>% group_by(gender) %>% filter(mass > mean(mass, na.rm = TRUE))

## create list based on video name to extract ratings from
list_video <- split(dat2, dat$videoName2)

  
  
  
# PID_names <- c("P101", "P102", "P103", "P104", "P105", "P106", "P107", "P109", "P110",
#                "P111", "P112", "P113", "P114", "P115", "P116", "P117", "P118", "P119",
#                "P120", "P121", "P122", "P124", "P125", "P126", "P127", "P128", "P129")
# for (i in 1:length(list_PID)) {
#   assign(PID_names[i], list_PID[[i]])
# }

# subset_data <- function(PID1) {
#   i = 1:9
#   k<- list(split(PID1, PID1$videoName2))
#   assign(paste(PID1) , k[[1]][i], envir = globalenv()) 
# }

vid_Names <- c("broadchurchS1E3", "broadchurchS1E4", "fargoS1E2",
               "fargoS1E7", "mindhunterS1E3", "mindhunterS2E7",
               "mindhunterS2E9", "theNightOfS1E1", "theUndoingS1E1")

save_vids <- function(PID1) {
  i = PID1$PID[1]
  j = 1:9
  v <- 
  assign(paste("vid",j, i, sep = "_") , v[j], envir = globalenv()) 
  # v2 <- broadchurchS1E4$meanRating
  # assign(paste("vid2", i, sep = "_") , v2, envir = globalenv())  
  # v3 <- fargoS1E2$meanRating
  # assign(paste("vid3", i, sep = "_") , v3, envir = globalenv())
  # v4 <- fargoS1E7$meanRating
  # assign(paste("vid4", i, sep = "_") , v4, envir = globalenv()) 
  # v5 <- mindhunterS1E3$meanRating
  # assign(paste("vid5", i, sep = "_") , v5, envir = globalenv())
  # v6 <- mindhunterS2E7$meanRating
  # assign(paste("vid6", i, sep = "_") , v6, envir = globalenv())
  # v7 <- mindhunterS2E9$meanRating
  # assign(paste("vid7", i, sep = "_") , v7, envir = globalenv())
  # v8 <- theNightOfS1E1$meanRating
  # assign(paste("vid8", i, sep = "_") , v8, envir = globalenv())
  # v9 <- theUndoingS1E1$meanRating
  # assign(paste("vid9", i, sep = "_") , v9, envir = globalenv())
}

for (i in 1:27) {
subset_data(list_PID[[i]])
#save_vids(list_PID[[i]])
}




vid1 <- data.frame(vid1_101[1:465],
                   vid1_102[1:465],
                   vid1_103[1:465],
                   vid1_104[1:465],
                   vid1_105[1:465],
                   vid1_106[1:465],
                   vid1_107[1:465],
                   vid1_109[1:465],
                   vid1_110[1:465],
                   vid1_111[1:465],
                   vid1_112[1:465],
                   vid1_113[1:465],
                   vid1_114[1:465],
                   vid1_115[1:465],
                   vid1_116[1:465],
                   vid1_117[1:465],
                   vid1_118[1:465],
                   vid1_119[1:465],
                   vid1_120[1:465],
                   vid1_121[1:465],
                   vid1_122[1:465],
                   vid1_124[1:465],
                   vid1_125[1:465],
                   vid1_126[1:465],
                   vid1_127[1:465],
                   vid1_128[1:465],
                   vid1_129[1:465])
m_vid1 <- cor(vid1)
corrplot(m_vid1)

vid2 <- data.frame(vid2_101[1:284],
                   vid2_102[1:284],
                   vid2_103[1:284],
                   vid2_104[1:284],
                   vid2_105[1:284],
                   vid2_106[1:284],
                   vid2_107[1:284],
                   vid2_109[1:284],
                   vid2_110[1:284],
                   vid2_111[1:284],
                   #vid1_112[1:465],
                   vid2_113[1:284],
                   vid2_114[1:284],
                   vid2_115[1:284],
                   vid2_116[1:284],
                   vid2_117[1:284],
                   vid2_118[1:284],
                   vid2_119[1:284],
                   vid2_120[1:284],
                   vid2_121[1:284],
                   vid2_122[1:284],
                   vid2_124[1:284],
                   vid2_125[1:284],
                   vid2_126[1:284],
                   vid2_127[1:284],
                   vid2_128[1:284],
                   vid2_129[1:284])
m_vid2 <- cor(vid2)
corrplot(m_vid2)

vid3 <- data.frame(vid3_101[1:366],
                   vid3_102[1:366],
                   vid3_103[1:366],
                   vid3_104[1:366],
                   vid3_105[1:366],
                   vid3_106[1:366],
                   vid3_107[1:366],
                   vid3_109[1:366],
                   vid3_110[1:366],
                   vid3_111[1:366],
                   #vid1_112[1:465],
                   vid3_113[1:366],
                   vid3_114[1:366],
                   vid3_115[1:366],
                   vid3_116[1:366],
                   vid3_117[1:366],
                   vid3_118[1:366],
                   vid3_119[1:366],
                   vid3_120[1:366],
                   vid3_121[1:366],
                   vid3_122[1:366],
                   vid3_124[1:366],
                   vid3_125[1:366],
                   vid3_126[1:366],
                   vid3_127[1:366],
                   vid3_128[1:366],
                   vid3_129[1:366])
m_vid3 <- cor(vid3)
corrplot(m_vid3)


vid4 <- data.frame(vid4_101[1:231],
                   #vid4_102[1:231],
                   vid4_103[1:231],
                   vid4_104[1:231],
                   vid4_105[1:231],
                   vid4_106[1:231],
                   vid4_107[1:231],
                   vid4_109[1:231],
                   vid4_110[1:231],
                   vid4_111[1:231],
                   #vid1_112[1:465],
                   vid4_113[1:231],
                   vid4_114[1:231],
                   vid4_115[1:231],
                   vid4_116[1:231],
                   vid4_117[1:231],
                   vid4_118[1:231],
                   vid4_119[1:231],
                   vid4_120[1:231],
                   vid4_121[1:231],
                   vid4_122[1:231],
                   vid4_124[1:231],
                   vid4_125[1:231],
                   vid4_126[1:231],
                   #vid4_127[1:231],
                   vid4_128[1:231],
                   vid4_129[1:231])
m_vid4 <- cor(vid4)
corrplot(m_vid4)

vid5 <- data.frame(vid5_101[1:293],
                   vid5_102[1:293],
                   vid5_103[1:293],
                   vid5_104[1:293],
                   vid5_105[1:293],
                   vid5_106[1:293],
                   vid5_107[1:293],
                   vid5_109[1:293],
                   vid5_110[1:293],
                   vid5_111[1:293],
                   #vid5_112[1:293],
                   vid5_113[1:293],
                   vid5_114[1:293],
                   vid5_115[1:293],
                   vid5_116[1:293],
                   vid5_117[1:293],
                   vid5_118[1:293],
                   vid5_119[1:293],
                   vid5_120[1:293],
                   vid5_121[1:293],
                   vid5_122[1:293],
                   vid5_124[1:293],
                   vid5_125[1:293],
                   vid5_126[1:293],
                   vid5_127[1:293],
                   vid5_128[1:293],
                   vid5_129[1:293])
m_vid5 <- cor(vid5)
corrplot(m_vid5)

vid6 <- data.frame(vid6_101[1:421],
                   vid6_102[1:421],
                   vid6_103[1:421],
                   vid6_104[1:421],
                   vid6_105[1:421],
                   vid6_106[1:421],
                   vid6_107[1:421],
                   vid6_109[1:421],
                   vid6_110[1:421],
                   vid6_111[1:421],
                   vid6_112[1:421],
                   #vid6_113[1:421],
                   vid6_114[1:421],
                   vid6_115[1:421],
                   vid6_116[1:421],
                   vid6_117[1:421],
                   vid6_118[1:421],
                   vid6_119[1:421],
                   vid6_120[1:421],
                   vid6_121[1:421],
                   vid6_122[1:421],
                   vid6_124[1:421],
                   vid6_125[1:421],
                   vid6_126[1:421],
                   vid6_127[1:421],
                   vid6_128[1:421],
                   vid6_129[1:421])
m_vid6 <- cor(vid6)
corrplot(m_vid6)

vid7 <- data.frame(vid7_101[1:343],
                   #vid7_102[1:343],
                   vid7_103[1:343],
                   vid7_104[1:343],
                   vid7_105[1:343],
                   vid7_106[1:343],
                   vid7_107[1:343],
                   vid7_109[1:343],
                   vid7_110[1:343],
                   vid7_111[1:343],
                   vid7_112[1:343],
                   vid7_113[1:343],
                   vid7_114[1:343],
                   vid7_115[1:343],
                   vid7_116[1:343],
                   vid7_117[1:343],
                   vid7_118[1:343],
                   vid7_119[1:343],
                   vid7_120[1:343],
                   vid7_121[1:343],
                   vid7_122[1:343],
                   vid7_124[1:343],
                   vid7_125[1:343],
                   vid7_126[1:343],
                   vid7_127[1:343],
                   vid7_128[1:343],
                   vid7_129[1:343])
m_vid7 <- cor(vid7)
corrplot(m_vid7)

vid8 <- data.frame(vid8_101[1:204],
                   vid8_102[1:204],
                   vid8_103[1:204],
                   vid8_104[1:204],
                   vid8_105[1:204],
                   vid8_106[1:204],
                   vid8_107[1:204],
                   vid8_109[1:204],
                   vid8_110[1:204],
                   vid8_111[1:204],
                   #vid8_112[1:204],
                   #vid8_113[1:204],
                   vid8_114[1:204],
                   vid8_115[1:204],
                   vid8_116[1:204],
                   vid8_117[1:204],
                   vid8_118[1:204],
                   vid8_119[1:204],
                   vid8_120[1:204],
                   vid8_121[1:204],
                   vid8_122[1:204],
                   vid8_124[1:204],
                   vid8_125[1:204],
                   vid8_126[1:204],
                   vid8_127[1:204],
                   vid8_128[1:204],
                   vid8_129[1:204])
m_vid8 <- cor(vid8)
corrplot(m_vid8)

vid9 <- data.frame(vid9_101[1:283],
                   vid9_102[1:283],
                   vid9_103[1:283],
                   vid9_104[1:283],
                   vid9_105[1:283],
                   vid9_106[1:283],
                   vid9_107[1:283],
                   vid9_109[1:283],
                   vid9_110[1:283],
                   vid9_111[1:283],
                   #vid9_112[1:283],
                   #vid9_113[1:283],
                   vid9_114[1:283],
                   vid9_115[1:283],
                   vid9_116[1:283],
                   vid9_117[1:283],
                   vid9_118[1:283],
                   vid9_119[1:283],
                   vid9_120[1:283],
                   vid9_121[1:283],
                   #vid9_122[1:283],
                   vid9_124[1:283],
                   vid9_125[1:283],
                   vid9_126[1:283],
                   vid9_127[1:283],
                   vid9_128[1:283],
                   vid9_129[1:283])
m_vid9 <- cor(vid9)
corrplot(m_vid9, method = 'color')

condName <- subset(P105, videoName2 == 'theUndoingS1E1')

m1_vid9 <- as.data.frame(m_vid9)

m1_vid9$A_P101 <- m1_vid9$vid9_101.1.283. 1
m1_vid9$V_P102 <- m1_vid9$vid9_102.1.283. 2
m1_vid9$D_P103 <- m1_vid9$vid9_103.1.283. 3
m1_vid9$V_P104 <- m1_vid9$vid9_104.1.283. 4
m1_vid9$V_P105 <- m1_vid9$vid9_105.1.283. 5
m1_vid9$A_P106 <- m1_vid9$vid9_106.1.283. 6
m1_vid9$D_P107 <- m1_vid9$vid9_107.1.283. 7
m1_vid9$D_P109 <- m1_vid9$vid9_109.1.283. 8
m1_vid9$A_P110 <- m1_vid9$vid9_110.1.283. 9
m1_vid9$D_P111 <- m1_vid9$vid9_111.1.283. 10
m1_vid9$D_P114 <- m1_vid9$vid9_114.1.283. 11
m1_vid9$V_P115 <- m1_vid9$vid9_115.1.283. 12
m1_vid9$D_P116 <- m1_vid9$vid9_116.1.283. 13
m1_vid9$V_P117 <- m1_vid9$vid9_117.1.283. 14
m1_vid9$D_P118 <- m1_vid9$vid9_118.1.283. 15
m1_vid9$D_P119 <- m1_vid9$vid9_119.1.283. 16
m1_vid9$V_P120 <- m1_vid9$vid9_120.1.283. 17
m1_vid9$A_P121 <- m1_vid9$vid9_121.1.283. 18
m1_vid9$A_P124 <- m1_vid9$vid9_124.1.283. 19
m1_vid9$A_P125 <- m1_vid9$vid9_125.1.283. 20
m1_vid9$V_P126 <- m1_vid9$vid9_126.1.283. 21
m1_vid9$A_P127 <- m1_vid9$vid9_127.1.283. 22
m1_vid9$D_P128 <- m1_vid9$vid9_128.1.283. 23
m1_vid9$A_P129 <- m1_vid9$vid9_129.1.283. 24

m1_vid9[,1:24] <- NULL

m <- cor(m1_vid9)
corrplot(m, method = 'color', order = 'hclust')

A_avg_101 <- (m1_vid9[6,1] + m1_vid9[9,1] + m1_vid9[18,1] + m1_vid9[19,1] + m1_vid9[20,1] + m1_vid9[22,1] + m1_vid9[24,1])/7
D_avg_101 <- (m1_vid9[3,1] + m1_vid9[7,1] + m1_vid9[8,1] + m1_vid9[10,1] + m1_vid9[11,1] + m1_vid9[13,1] + m1_vid9[15,1] + m1_vid9[16,1] + m1_vid9[23,1])/9
V_avg_101 <- (m1_vid9[2,1] + m1_vid9[4,1] + m1_vid9[5,1] + m1_vid9[12,1] + m1_vid9[14,1] + m1_vid9[17,1] + m1_vid9[21,1])/7
A_avg_102 <- (m1_vid9[1,2] + m1_vid9[6,2] + m1_vid9[9,2] + m1_vid9[18,2] + m1_vid9[19,2] + m1_vid9[20,2] + m1_vid9[22,2] + m1_vid9[24,2])/8
D_avg_102 <- (m1_vid9[3,2] + m1_vid9[7,2] + m1_vid9[8,2] + m1_vid9[10,2] + m1_vid9[11,2] + m1_vid9[13,2] + m1_vid9[15,2] + m1_vid9[16,2] + m1_vid9[23,2])/9
V_avg_102 <- (m1_vid9[4,2] + m1_vid9[5,2] + m1_vid9[12,2] + m1_vid9[14,2] + m1_vid9[17,2] + m1_vid9[21,2])/6
A_avg_103 <- (m1_vid9[1,3] + m1_vid9[6,3] + m1_vid9[9,3] + m1_vid9[18,3] + m1_vid9[19,3] + m1_vid9[20,3] + m1_vid9[22,3] + m1_vid9[24,3])/8
D_avg_103 <- (m1_vid9[7,3] + m1_vid9[8,3] + m1_vid9[10,3] + m1_vid9[11,3] + m1_vid9[13,3] + m1_vid9[15,3] + m1_vid9[16,3] + m1_vid9[23,3])/8
V_avg_103 <- (m1_vid9[2,3] + m1_vid9[4,3] + m1_vid9[5,3] + m1_vid9[12,3] + m1_vid9[14,3] + m1_vid9[17,3] + m1_vid9[21,3])/7
A_avg_104 <- (m1_vid9[1,4] + m1_vid9[6,4] + m1_vid9[9,4] + m1_vid9[18,4] + m1_vid9[19,4] + m1_vid9[20,4] + m1_vid9[22,4] + m1_vid9[24,4])/8
D_avg_104 <- (m1_vid9[3,4] + m1_vid9[7,4] + m1_vid9[8,4] + m1_vid9[10,4] + m1_vid9[11,4] + m1_vid9[13,4] + m1_vid9[15,4] + m1_vid9[16,4] + m1_vid9[23,4])/9
V_avg_104 <- (m1_vid9[2,4] + m1_vid9[5,4] + m1_vid9[12,4] + m1_vid9[14,4] + m1_vid9[17,4] + m1_vid9[21,4])/6
A_avg_105 <- (m1_vid9[1,5] + m1_vid9[6,5] + m1_vid9[9,5] + m1_vid9[18,5] + m1_vid9[19,5] + m1_vid9[20,5] + m1_vid9[22,5] + m1_vid9[24,5])/8
D_avg_105 <- (m1_vid9[3,5] + m1_vid9[7,5] + m1_vid9[8,5] + m1_vid9[10,5] + m1_vid9[11,5] + m1_vid9[13,5] + m1_vid9[15,5] + m1_vid9[16,5] + m1_vid9[23,5])/9
V_avg_105 <- (m1_vid9[2,5] + m1_vid9[4,5] + m1_vid9[12,5] + m1_vid9[14,5] + m1_vid9[17,5] + m1_vid9[21,5])/6
A_avg_106 <- (m1_vid9[1,6] + m1_vid9[9,6] + m1_vid9[18,6] + m1_vid9[19,6] + m1_vid9[20,6] + m1_vid9[22,6] + m1_vid9[24,6])/7
D_avg_106 <- (m1_vid9[3,6] + m1_vid9[7,6] + m1_vid9[8,6] + m1_vid9[10,6] + m1_vid9[11,6] + m1_vid9[13,6] + m1_vid9[15,6] + m1_vid9[16,6] + m1_vid9[23,6])/9
V_avg_106 <- (m1_vid9[2,6] + m1_vid9[4,6] + m1_vid9[5,6] + m1_vid9[12,6] + m1_vid9[14,6] + m1_vid9[17,6] + m1_vid9[21,6])/7
A_avg_107 <- (m1_vid9[1,7] + m1_vid9[6,7] + m1_vid9[9,7] + m1_vid9[18,7] + m1_vid9[19,7] + m1_vid9[20,7] + m1_vid9[22,7] + m1_vid9[24,7])/8
D_avg_107 <- (m1_vid9[3,7] + m1_vid9[8,7] + m1_vid9[10,7] + m1_vid9[11,7] + m1_vid9[13,7] + m1_vid9[15,7] + m1_vid9[16,7] + m1_vid9[23,7])/8
V_avg_107 <- (m1_vid9[2,7] + m1_vid9[4,7] + m1_vid9[5,7] + m1_vid9[12,7] + m1_vid9[14,7] + m1_vid9[17,7] + m1_vid9[21,7])/7
A_avg_109 <- (m1_vid9[1,8] + m1_vid9[6,8] + m1_vid9[9,8] + m1_vid9[18,8] + m1_vid9[19,8] + m1_vid9[20,8] + m1_vid9[22,8] + m1_vid9[24,8])/8
D_avg_109 <- (m1_vid9[3,8] + m1_vid9[7,8] + m1_vid9[10,8] + m1_vid9[11,8] + m1_vid9[13,8] + m1_vid9[15,8] + m1_vid9[16,8] + m1_vid9[23,8])/8
V_avg_109 <- (m1_vid9[2,8] + m1_vid9[4,8] + m1_vid9[5,8] + m1_vid9[12,8] + m1_vid9[14,8] + m1_vid9[17,8] + m1_vid9[21,8])/7
A_avg_110 <- (m1_vid9[6,9] + m1_vid9[1,9] + m1_vid9[18,9] + m1_vid9[19,9] + m1_vid9[20,9] + m1_vid9[22,9] + m1_vid9[24,9])/7
D_avg_110 <- (m1_vid9[3,9] + m1_vid9[7,9] + m1_vid9[8,9] + m1_vid9[10,9] + m1_vid9[11,9] + m1_vid9[13,9] + m1_vid9[15,9] + m1_vid9[16,9] + m1_vid9[23,9])/9
V_avg_110 <- (m1_vid9[2,9] + m1_vid9[4,9] + m1_vid9[5,9] + m1_vid9[12,9] + m1_vid9[14,9] + m1_vid9[17,9] + m1_vid9[21,9])/7

AA_cov <- mean(A_avg_101, A_avg_106, A_avg_110)
VV_cov <- mean(V_avg_102, V_avg_104, V_avg_105)
DD_cov <- mean(D_avg_103, D_avg_107, D_avg_109)
AV_cov <- mean(V_avg_101, V_avg_106, V_avg_110)
AD_cov <- mean(D_avg_101, D_avg_106, D_avg_110)
VA_cov <- mean(A_avg_102, A_avg_104, A_avg_105)
VD_cov <- mean(D_avg_102, D_avg_104, D_avg_105)
DA_cov <- mean(A_avg_103, A_avg_107, A_avg_109)
DV_cov <- mean(V_avg_103, V_avg_107, V_avg_109)




