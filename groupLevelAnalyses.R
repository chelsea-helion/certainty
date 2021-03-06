library(plyr)
library(corrplot)
library(readr)
library(lme4)
library(lmerTest)
library(data.table)
library(ggplot2)

## load all csvs
setwd("/Users/tua37526/Dropbox/certainty/data/csvProcessed")
dataFiles <- list.files(pattern = "*log.csv", full.names = TRUE)
dat_csv <- ldply(dataFiles, read_csv)
dat_csv$condName2 <-as.factor(dat_csv$condName)
dat_csv$videoName2 <-as.factor(dat_csv$videoName)

## rename variables to be consistent
levels(dat_csv$condName2) <- list(detective = "detective",
                                 detective = "detective and will be called to testify",
                                 friendOfVictim = "friend of the victim",
                                 friendOfVictim = "friend of the victim and will be called to testify in support of the victim",
                                 friendOfAccused = "friend of the accused",
                                 friendOfAccused = "friend of the accused and will be called to testify in support of the accused")

levels(dat_csv$videoName2) <- list(broadchurchS1E3 = "Broadchurch_S01E03_1_1.mp4",
                                  broadchurchS1E3 = "Stim.Broadchurch.S01E03.mp4",
                                  broadchurchS1E4 = "Broadchurch_S01E04_1.mp4",
                                  broadchurchS1E4 = "Stim.Broadchurch.S01E04.mp4",
                                  fargoS1E2 = "FargoS1E2_1.mp4",
                                  fargoS1E2 = "Stim.Fargo.S01E02.mp4",
                                  fargoS1E7 = "FargoS1E7_1.mp4",
                                  fargoS1E7 = "Stim.Fargo.S01E07.mp4",
                                  mindhunterS1E3 = "MindhunterS1E3_1.mp4",
                                  mindhunterS1E3 = "Stim.Mindhunter.S01E03.mp4",
                                  mindhunterS2E7 = "MindhunterS2E7_1.mp4",
                                  mindhunterS2E7 = "Stim.Mindhunter.S02E07.mp4",
                                  mindhunterS2E9 = "MindhunterS2E9_1_1.mp4",
                                  mindhunterS2E9 = "Stim.Mindhunter.S02E09.mp4",
                                  theNightOfS1E1 = "Stim.TheNightOf.S01E01.mp4",
                                  theNightOfS1E1 = "TheNightOf_1_1.mp4",
                                  theUndoingS1E1 = "Stim.TheUndoing.S01E01.mp4",
                                  theUndoingS1E1 = "TheUndoing_1_1.mp4")

# set condname to numerical value for indexing in python
dat_csv$condName3 <- as.numeric(dat_csv$condName2)
dat_csv$videoName3 <- as.numeric(dat_csv$videoName2)

# remove NAs
dat_csv2 <- na.omit(dat_csv)

#write summary file to folder
write.csv(dat_csv2, "/Users/tua37526/Dropbox/certainty/data/summary.csv")

tu_S1E1 <- subset(dat_csv, videoName2 == "theUndoingS1E1")
tu_S1E1  <- tu_S1E1  %>%
  dplyr::group_by(PID) %>%
  dplyr::mutate(NewTime = row_number())

p <- tu_S1E1 %>%
  ggplot(aes(x=NewTime, y=meanRating, color = condName2)) +
  #geom_area() +
  geom_smooth() +
  ylab("certainty") +
  ylim(-100, 100) +
  theme_bw() +
  ggtitle(tu_S1E1$videoName[1]) +
  labs(x = "Time")
p



acf(tu_S1E1$meanRating)

f_S1E7 <- subset(dat_csv, videoName2 == "fargoS1E7")
f_S1E7  <- f_S1E7  %>%
  dplyr::group_by(PID) %>%
  dplyr::mutate(NewTime = row_number())

p <- f_S1E7 %>%
  ggplot(aes(x=NewTime, y=meanRating, color = condName2)) +
  #geom_area() +
  geom_smooth() +
  ylab("certainty") +
  ylim(-100, 100) +
  theme_bw() +
  ggtitle(f_S1E7$videoName[1]) +
  labs(x = "Time")
p

f_S1E2 <- subset(dat_csv, videoName2 == "fargoS1E2")
f_S1E2  <- f_S1E2  %>%
  dplyr::group_by(PID) %>%
  dplyr::mutate(NewTime = row_number())

p <- f_S1E2 %>%
  ggplot(aes(x=NewTime, y=meanRating, color = condName2)) +
  #geom_area() +
  geom_smooth() +
  ylab("certainty") +
  ylim(-100, 100) +
  theme_bw() +
  ggtitle(f_S1E2$videoName[1]) +
  labs(x = "Time")
p

no_S1E1 <- subset(dat_csv, videoName2 == "theNightOfS1E1")
no_S1E1  <- no_S1E1  %>%
  dplyr::group_by(PID) %>%
  dplyr::mutate(NewTime = row_number())

p <- no_S1E1 %>%
  ggplot(aes(x=NewTime, y=meanRating, color = condName2)) +
  #geom_area() +
  geom_smooth() +
  ylab("certainty") +
  ylim(-100, 100) +
  theme_bw() +
  ggtitle(no_S1E1$videoName[1]) +
  labs(x = "Time")
p

bc_S1E4 <- subset(dat_csv, videoName2 == "broadchurchS1E4")
bc_S1E4 <- bc_S1E4 %>%
  dplyr::group_by(PID) %>%
  dplyr::mutate(NewTime = row_number())


p <- bc_S1E4 %>%
  ggplot(aes(x=NewTime, y=meanRating, color = condName2)) +
  #geom_area() +
  geom_smooth() +
  ylab("certainty") +
  ylim(-100, 100) +
  theme_bw() +
  ggtitle(bc_S1E4$videoName[1]) +
  labs(x = "Time")
p


bc_S1E3 <- subset(dat_csv, videoName2 == "broadchurchS1E3")
bc_S1E3 <- bc_S1E3 %>%
  dplyr::group_by(PID) %>%
  dplyr::mutate(NewTime = row_number())


p <- bc_S1E3 %>%
  ggplot(aes(x=NewTime, y=meanRating, color = condName2)) +
  #geom_area() +
  geom_smooth() +
  ylab("certainty") +
  ylim(-100, 100) +
  theme_bw() +
  ggtitle(bc_S1E3$videoName[1]) +
  labs(x = "Time")
p
                                 
test <- lmer(meanRating ~ condName2 + (1|PID) + (1|videoName), data = dat_csv)
summary(test)
#confint(test)
plot(effect("condName2", test), grid = TRUE)

library(MetBrewer)
library(ggridges)
pp <- met.brewer(name = "Egypt")

dat_csv <- na.omit(dat_csv)

dat_csv %>% 
  group_by(condName2) %>% 
  mutate(group_mean = mean(meanRating)) %>% 
  ungroup() %>% 
  mutate(condNumber = fct_reorder(condName2, group_mean)) %>% 
  
  ggplot(aes(x = meanRating, y = condNumber, fill = group_mean)) +
  geom_density_ridges(scale = 3/2, size = .2, color = pp[4]) +
  scale_fill_gradient(low = pp[5], high = pp[2]) +
  #scale_x_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(-120, 100)) +
  scale_y_discrete(NULL, expand = expansion(mult = c(0, 0.4))) +
  theme(axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank(),
        legend.position = "none")

library(brms)

(mean_y <- mean(dat_csv$meanRating))
(sd_y <- sd(dat_csv$meanRating))

omega <- sd_y / 2
sigma <- 2 * sd_y

gamma_a_b_from_omega_sigma <- function(mode, sd) {
  
  if (mode <= 0) stop("mode must be > 0")
  if (sd   <= 0) stop("sd must be > 0")
  rate <- (mode + sqrt(mode^2 + 4 * sd^2)) / (2 * sd^2)
  shape <- 1 + mode * rate
  return(list(shape = shape, rate = rate))
  
}

(s_r <- gamma_a_b_from_omega_sigma(mode = omega, sd = sigma))

stanvars <- 
  stanvar(mean_y,    name = "mean_y") + 
  stanvar(sd_y,      name = "sd_y") +
  stanvar(s_r$shape, name = "alpha") +
  stanvar(s_r$rate,  name = "beta")

fit19.1 <-
  brm(data = dat_csv,
      family = gaussian,
      meanRating ~ condName2 + (1|videoName3) + (1|PID),
      prior = c(prior(normal(mean_y, sd_y * 5), class = Intercept),
                prior(gamma(alpha, beta), class = sd),
                prior(cauchy(0, sd_y), class = sigma)),
      iter = 4000, warmup = 1000, chains = 1, cores = 1,
      seed = 19,
      control = list(adapt_delta = 0.99),
      stanvars = stanvars)
