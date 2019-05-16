games <- read.csv("~/Desktop/datafest2019/games.csv")
gps <- read.csv("~/Desktop/datafest2019/gps.csv")
rpe <- read.csv("~/Desktop/datafest2019/rpe.csv")
wellness <- read.csv("~/Desktop/datafest2019/wellness.csv")
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

#####Checking Our Subjective_score is better than the Monitoring_score:
rpefilter <- rpe %>%
  filter(!is.na(BestOutOfMyself))
  
ANOVA <- PlayerinGame %>%
  inner_join(rpefilter, by=c('Date','PlayerID')) %>%
  distinct(PlayerID,Date, .keep_all = T)

#Two-sample t-test for the MonitoringScore based on two levels
var.test(ANOVA$MonitoringScore[ANOVA$BestOutOfMyself=="Not at all"],
         ANOVA$MonitoringScore[ANOVA$BestOutOfMyself=="Absolutely"])
#bigger than 0.1, evidence that variance is the same.
t.test(ANOVA$MonitoringScore[ANOVA$BestOutOfMyself=="Not at all"],
       ANOVA$MonitoringScore[ANOVA$BestOutOfMyself=="Absolutely"],var.equal = T)
#p-value=0.3323, evidence that they are the same. 



#Two-simple t-test for Sscore based on two levels
#we only check for two level as we only have one obs for "somewhat"
var.test(ANOVA$Sscore[ANOVA$BestOutOfMyself=="Not at all"],
         ANOVA$Sscore[ANOVA$BestOutOfMyself=="Absolutely"])
#bigger than 0.1, evidenve that variance is the same.
t.test(ANOVA$Sscore[ANOVA$BestOutOfMyself=="Not at all"],
       ANOVA$Sscore[ANOVA$BestOutOfMyself=="Absolutely"],var.equal = T)
#p-value=0.09286, evidence that they are not the same based on 10% significance level.

ANOVA2 <- ANOVA %>%
  filter(BestOutOfMyself != 'Somewhat')


a=ANOVA$Sscore[ANOVA$BestOutOfMyself=="Not at all"]
b=ANOVA$Sscore[ANOVA$BestOutOfMyself=="Absolutely"]
ggplot(ANOVA2, aes(x=BestOutOfMyself, y=Sscore)) + 
  geom_boxplot(aes(color = BestOutOfMyself)) + 
  geom_jitter(width = 0.2) +
  labs(title = "Subjective Score on athletes") +
  theme(legend.position = c(0.77, 0.85),
        legend.title =  element_text(size = 7)) +
  ylab(label = 'Our subjective score') + xlab(label = "Atheletes' response")

ggplot(ANOVA2, aes(sample=Sscore, colour = 'red')) +
  stat_qq() +
  stat_qq_line()


c=ANOVA$MonitoringScore[ANOVA$BestOutOfMyself=="Not at all"]
d=ANOVA$MonitoringScore[ANOVA$BestOutOfMyself=="Absolutely"]
ggplot(ANOVA2, aes(x=BestOutOfMyself, y=MonitoringScore)) + 
  geom_boxplot(aes(color = BestOutOfMyself), outlier.colour = "red", outlier.shape = 5) +
  geom_jitter(width = 0.2) +
  labs(title = "Monitoring Score on athletes") +
  theme(legend.position = c(0.77, 0.85),
        legend.title =  element_text(size = 7)) +
  ylab(label = 'Monitoring Score') + xlab(label = "Atheletes' response")

ggplot(ANOVA2, aes(sample=MonitoringScore, colour = 'red')) +
  stat_qq() +
  stat_qq_line()










#Objective Score:
X2 <- select(PlayerinGame,"stdEnergy","stdacLoad")
PCA2 <- prcomp(X2)
summary(PCA2)
biplot(PCA2$x,PCA2$rotation[,1:2])
X3 <- mutate(X2,Objective_Score=as.matrix(X2)%*%PCA2$rotation[,1])
X1$SleepHours <- PlayerinGame$SleepHours
PlayerinGame$Oscore <- X3$Objective_Score
Weights_for_Objective <- PCA2$rotation[,1]


dailyrpe <- rpe %>%
  filter(SessionType == "Game") %>%
  group_by(PlayerID, Date) %>%
  mutate(DailyLoad = sum(SessionLoad), Duration = sum(Duration), RPE = mean(RPE)) %>%
  distinct(Date,.keep_all=T) %>%
  select(Date, PlayerID, Training, Duration, RPE, DailyLoad)

# Perfomance of each player in each game
allasone <- PlayerinGame %>%
  inner_join(dailyrpe, by = c("Date","PlayerID")) %>%
  group_by(PlayerID) %>%
  mutate(stdDailyLoad = (DailyLoad - mean(DailyLoad)) / sd(DailyLoad),
         stdRPE = (RPE - mean(RPE)) / sd(RPE))

# ranking
ranking <- c("Australia","New Zealand","France","Canada","USA","Russia","Spain","England","Fiji","Ireland","Japan","South Africa","Brazil","Kenya")

score_weight <- function(netscore, opponent, win, scaling=2, place=4, c=0.2) {
  weighted_score <- rep(0, length(win))
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

M <- PlayerinGame %>%
  group_by(Date) %>%
  summarise(Msscore = mean(Sscore), Msscore2 = mean(Sscore2), Moscore = mean(Oscore)) 

K <- M %>%
  inner_join(WeightedGames, by="Date") %>%
  group_by(Date) %>%
  summarise(Mweightedscore = mean(WeightedScore)) %>%
  inner_join(M, by="Date")

WeightedGamesM <- M %>%
  inner_join(WeightedGames, by='Date')





