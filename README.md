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
### Games.csv
Tells you when, where, opponent, and high-level outcomes and events in the game("box scores").  

**How were data collected:** information comes from this website: [https://en.wikipedia.org/wiki/2017%E2%80%9318_World_Rugby_Sevens_Series](https://en.wikipedia.org/wiki/2017%E2%80%9318_World_Rugby_Sevens_Series)  
**How to use:** high-level game information.  
**Links to other dataset:** GameID link to gps. Date links to wellness, GPS, Rate of Perceived Effort(RPE).  

### Wellness.csv
Self-reported health and wellness for each player.  

**How were data collected:** self-reported by each athlete. In principle, reported every morning before 8:30 a.m. All values are subjective, but Urine Specific Gravity(USG) is recorded through a sensor. Each athelete may have a different sense of what "typical" means for them.  
**How to use:** provides subjective sense of energy levels. USG can provide evidence of dehydration.  
**Links to other dataset:** Date links to wellness, GPS, RPS, games. PlayerID links to RPE, GPS.  
