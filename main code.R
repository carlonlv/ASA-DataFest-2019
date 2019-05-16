games <- read.csv("~/Desktop/datafest2019/games.csv")
gps <- read.csv("~/Desktop/datafest2019/gps.csv")
rpe <- read.csv("~/Desktop/datafest2019/rpe.csv")
wellness <- read.csv("~/Desktop/datafest2019/wellness.csv")
###

wellness$Date <- as.Date(wellness$Date)
games$Date <- as.Date(games$Date)
rpe$Date <- as.Date(rpe$Date)

well <- wellness %>%
  arrange(Date) %>%
  group_by(PlayerID) %>%
  arrange(PlayerID) %>%
  mutate(Date=Date-1)

##k <- games %>%
##  inner_join(gps, by = "GameID") %>%
##  inner_join(well, by = c("PlayerID","Date")) %>%
##  inner_join(rpe, by = c("PlayerID","Date"))

  
PlayerinGame <- gps %>%
  inner_join(games, by = "GameID") %>%
  group_by(PlayerID, Date) %>%
  summarise(Mspeed = mean((Speed)**2), Macimpulse = mean(abs(AccelImpulse)), Macload = mean(abs(AccelLoad))) %>%
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
PlayerinGame <- ungroup(PlayerinGame)
X <- select(PlayerinGame,"Fatigue","Soreness","Desire","Irritability","SleepQuality","Mspeed","Macimpulse","Macload")
PCA <- prcomp(X)
summary(PCA)
biplot(PCA$x,PCA$rotation[,1:5])
X1 <- mutate(X,PC1=as.matrix(X)%*%PCA$rotation[,1])
X1 <- mutate(X1,PC2=as.matrix(X)%*%PCA$rotation[,2])
X1$SleepHours <- PlayerinGame$SleepHours
PlayerinGame$OurMeasure <- X1$PC1
