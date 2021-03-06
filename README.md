# ASA-DataFest-2019

## Introduction 

We participated in this ASA DataFest held by University of Toronto Statistical Sciences and University of Toronto Scarborough Computer and Mathematical Sciences in May of 2019. We won the Honorable Mention award. Big thanks to my teammates: Ziang Zhang, Xiaotong Liu and Haoye Wang.  

You can find more information about this event at [their official website @UofT](https://utorontodatafest.wordpress.com/).

## Setup

The Canadian National Women's Rugby Team seeks advice on the role of workload and fatigue in Rugby 7s. Rugby 7s is a fast-paced, physically demanding sport that pushes the limits of athelete speed, endurance and toughness. Rugby 7s players may play in up to three games in a day, resulting in tremendous amout of athletic exertion. Substantial exertion results in fatigue, which may lead to physiological deficits(e.g., dehydration), reduced athletic performance, and greater risk of injury.  

You can watch the introduction video to this event at [ASA Datafest CSI Pacific video link](https://www.youtube.com/watch?v=ie32B8MldyE&feature=youtu.be).

## Problem

Very little is known about managing player fatigue in professional athletics, and many training decisions are based on “gut feel.” Fatigue is typically estimated by asking players how they feel in wellness surveys. There is no agreed-upon standard of defining fatigue so the relationship between workload and fatigue is unclear.  

In this challenge, we were encouraged to explore new ways of measuring fatigue and examine its effects on players’ performance and physical wellness. The datasets provide a number of observations that we believe will be useful to measure fatigue in players of the Canadian National Women's Rugby Team in the 2017-2018 season.  

Remember that training load is not the same as fatigue, and one question to explore is whether you can find evidence that some measures of training load are better than others. 

## DataSet
The data were collected during the 2017-2018 season. There are five files that given different aspects of the games. The data themselves were collected through a variety of means.  

Player level data are provided by the individual athletes themselves and by IMU/GPS devices worn on their vests during games. GPS data  
may not be available if players are out of range of the satellites. Players are uniquely identified by the PlayerID variable in all data files. Note that players numbered 18-21 did not play in any of the games in the datasets, and so they can be removed from the analysis.  
**Due to the requirement of the Organizer, we are not allowed to distribute the datasets online.**

### Games.csv
Tells you when, where, opponent, and high-level outcomes and events in the game("box scores").  

**How were data collected:** information comes from this website: [https://en.wikipedia.org/wiki/2017%E2%80%9318_World_Rugby_Sevens_Series](https://en.wikipedia.org/wiki/2017%E2%80%9318_World_Rugby_Sevens_Series)  
**How to use:** high-level game information.  
**Links to other dataset:s** GameID link to gps. Date links to wellness, GPS, Rate of Perceived Effort(RPE).  

### Wellness.csv
Self-reported health and wellness for each player.  

**How were data collected:** self-reported by each athlete. In principle, reported every morning before 8:30 a.m. All values are subjective, but Urine Specific Gravity(USG) is recorded through a sensor. Each athelete may have a different sense of what "typical" means for them.  
**How to use:** provides subjective sense of energy levels. USG can provide evidence of dehydration.  
**Links to other datasets:** Date links to wellness, GPS, RPS, games. PlayerID links to RPE, GPS.  

### RPE.csv
Rate of Percieved Effort. Self-reported workloads for each "session". A session can be a workout(focusing on a particular objective) or a game.

**How were data collected:** In theory, each player rates herself after each session and/or game. It is easy, however, for players to neglect this when playing back-to-back games. Note that each day there can be multiple "sessions", and that a "session" can be a recovery period, a game, strength and conditioning, etc. There is no way to associate a particular rating with a particular game on days in which multiple games were played.  
**How to use:** Can be used to provide a subjective sense of fatigue. Note that what one player rates "4" for RPE another might rate "7" or any other number. For many sports analysts, a ratio of acute/chronic training load > 1.2 indicates that the athlete is currently in "high" traning load and at an increased risk for injury. A ratio < 0.8 indicates that they are "de-training" or recovering. These cut-off are based on Australlian Football League players.   
**Links to other datasets:** Date links to wellness, GPS, games. PlayerID links to wellness and GPS. 

### gps.csv
Position data for each player during a game.  

**How were data collected:** Data collected from sensors worn by players. Originally, data were collected at 100 Hz(100 times per second), but have been collapsed to 10 Hz. Thus, each second, there are 10 "frames" that provide information on player location and acceleration.  
Note that we do not know the location of the ball, or the orientation of the playing field. The "z" acceleration is in the up-ward direction, x is back-front, y is side-to-side.  
**How to use:** with caution. Note that making plots of location is unlikely to help you understand the role of fatigue unless you first think carefully about aspects of location that might be affected by fatigue.  
**Links to other datasets:** GameID link to games. Date links to wellness, GPS, RPE. PlayerID links to wellness, RPE.
