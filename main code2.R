games <- read.csv(file.choose())
gamegps <- read.csv(file.choose())
rpe <- read.csv(file.choose())
wellness <- read.csv(file.choose())
###
library(tidyverse)
library(ggplot2)
library(dplyr)

wellness$Date <- as.Date(wellness$Date)
games$Date <- as.Date(games$Date)
rpe$Date <- as.Date(rpe$Date)

well <- wellness %>%
  arrange(Date) %>%
  group_by(PlayerID) %>%
  arrange(PlayerID) %>%
  mutate(Date=Date-1) %>%
  mutate(stdFatigue = (Fatigue - mean(Fatigue)) / sd(Fatigue), 
         stdSoreness = (Soreness - mean(Soreness)) / sd(Soreness),
         stdDesire = (Desire - mean(Desire)) / sd(Desire),
         stdIrritability = (Irritability - mean(Irritability)) / sd(Irritability),
         stdSleepQuality = (SleepQuality - mean(SleepQuality)) / sd(SleepQuality))
  

##k <- games %>%
##  inner_join(gps, by = "GameID") %>%
##  inner_join(well, by = c("PlayerID","Date")) %>%
##  inner_join(rpe, by = c("PlayerID","Date"))

  
PlayerinGame <- gps %>%
  mutate(Energy = Speed * AccelImpulse) %>%
  inner_join(games, by = "GameID") %>%
  group_by(PlayerID, Date) %>%
  summarise(stdEnergy = sum(Energy), stdacLoad = sum(AccelLoad)) %>%
  ungroup() %>%
  mutate(stdacLoad = (stdacLoad - mean(stdacLoad))/sd(stdacLoad),
         stdEnergy = (stdEnergy - mean(stdEnergy))/sd(stdEnergy)) %>%
  inner_join(well, by = c("PlayerID", "Date"))
  
#wellness %>% 
  #na.omit() %>%
  #inner_join(na.omit(rpe), by = c("PlayerID","Date")) %>%
  #filter(SessionType == "Game") %>%
  #ggplot(aes(y=SleepHours,x=Fatigue)) +
  #geom_point()

#model1 <- lm(filter(k,SessionType == "Skills")$AcuteChronicRatio~filter(k,SessionType == "Skills")$USG)

#check.linear.relationships <- function(response, explanatory, dataset, f=FALSE) {
#  dataset <- na.omit(dataset)
#  p.val.success <- FALSE
#  lm_model <- NULL
#  y <- dataset[response]
#  x <- dataset[explanatory]
#  if (f == FALSE) {
#    lm_model <- lm(y~x)
#  }
#  pval <- summary(lm_model)$coefficients[2,4]
#  if (pval < 0.05) {
#    p.val.success <- TRUE
#    print(paste(response, explanatory, "has relationship"))
#  }
#}

#check.linear.relationships(response = "RPE", explanatory = "Fatigue", dataset = k)






# PCA for measureing fatigue:
#Subjective Score:
PlayerinGame <- ungroup(PlayerinGame)
X <- select(PlayerinGame,"stdFatigue","stdSoreness","stdDesire","stdIrritability","stdSleepQuality")
PCA <- prcomp(X)
summary(PCA)
biplot(PCA$x,PCA$rotation[,1:2])
X1 <- mutate(X,Subjective_Score=as.matrix(X)%*%PCA$rotation[,1])
X1 <- mutate(X1,Subjective_Score_2=as.matrix(X)%*%PCA$rotation[,2])
X1$SleepHours <- PlayerinGame$SleepHours
PlayerinGame$Sscore <- X1$Subjective_Score
PlayerinGame$Sscore2 <- X1$Subjective_Score_2
Weights_for_Subjective <- PCA$rotation[,1]

#Objective Score:
X2 <- select(PlayerinGame,"stdEnergy","stdacLoad")
PCA2 <- prcomp(X2)
summary(PCA2)
biplot(PCA2$x,PCA2$rotation[,1:2])
X3 <- mutate(X2,Objective_Score=as.matrix(X2)%*%PCA2$rotation[,1])
X1$SleepHours <- PlayerinGame$SleepHours
PlayerinGame$Oscore <- X3$Objective_Score
Weights_for_Objective <- PCA2$rotation[,1]


# Perfomance of each player in each game
allasone <- PlayerinGame %>%
  inner_join(rpe, by = c("PlayerID", "Date"))

# ranking
ranking <- c("Australia","New Zealand","France","Canada","USA","Russia","Spain","England","Fiji","Ireland","Japan","South Africa","Brazil","Kenya")

score_weight <- function(netscore, opponent, win, scaling=2, place=4, c=0.2) {
  weighted_score <- rep(1, length(win))
  for (i in 1:length(win)) {
    if (win[i] == "W") {
      weighted_score[i] <- netscore[i] * scaling * exp(c*(place-which(ranking == opponent[i])))/(1+exp(c*(place-which(ranking == opponent[i]))))
    } else if (win[i] == "L") {
      weighted_score[i] <- netscore[i] * (-scaling) * exp(c*(which(ranking == opponent[i])-place))/(1+exp(c*(which(ranking == opponent[i])-place)))
    }
  }
  weighted_score 
}

WeightedGames <- games %>%
  mutate(WeightedScore = score_weight(abs(TeamPoints-TeamPointsAllowed), Opponent, Outcome))