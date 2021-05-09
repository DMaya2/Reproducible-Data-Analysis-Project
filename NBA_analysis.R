#Packages
#These are the packages to be used
library(tidyverse)
library(broom)
library(ggcorrplot)

#Datasets
#Reading the datasets into R.

player_salaries <- read_csv("2018-19_nba_player-salaries.csv")
player_stats <- read_csv("2018-19_nba_player-statistics.csv")
team_stats_1 <- read_csv("2018-19_nba_team-statistics_1.csv")
team_stats_2 <- read_csv("2018-19_nba_team-statistics_2.csv")
team_payroll <- read_csv("2019-20_nba_team-payroll.csv")

#Datasets description 
#Two datasets are used
#2018-19_nba_player-statistics.csv: This data file provides total statistics for 
#Individual NBA players during the 2018-19 season. The variables consist:

#2018-19_nba_player-salaries.csv: This data file contains the salary for 
#individual players during the 2018-19 NBA season. The variables consist:


#Data pre-processing
#looking at the two datasets only
head(player_salaries)
tail(player_salaries)
str(player_salaries)
dim(player_salaries)

head(player_stats)
tail(player_stats)
str(player_stats)
dim(player_stats)


#Missing values Handling 
player_salaries %>% summarise(sum(is.na(.)))

#The player salaries data has no missing values

player_stats %>% summarise(sum(is.na(.)))

#The player statistics data has 117 missing values

#columns containing missing values
colnames(is.na(player_stats))

#we can see that 3P%, 2P%, FT% are columns with missing values
colSums(is.na(player_stats)) %>%  as.data.frame() 

#Seeks like these missing values are from players who played less games 
#therefore we will drop them

#dropping missing values 
player_stats_clean <- player_stats %>% drop_na()


#Data tidying 
#Merging player salaries data and player statistics clean data.

player_data <- full_join(player_salaries, player_stats_clean, by = "player_name")

#Duplicates handling 
#handling duplicates 
player_data$player_name[duplicated(player_data$player_id)] %>% as.data.frame()
unique(player_data$player_name[duplicated(player_data$player_id)]) %>% as.data.frame()

#There are alot of players duplicated probably due to season trading

#Removing duplicates from the data
player_data <- player_data[!duplicated(player_data$player_id), ]

colSums(is.na(player_data))
#we can see that there are missing values introduced after the 
#two datasets are merged.
#The missing values are mostly from players with no defined position
#so we will also drop them 

player_data_tidy <- player_data %>% drop_na()

#saving the tidy data
write.csv(player_data_tidy, "tidy_data.csv")


#Data exploration
#Salaries distribution
ggplot(data = player_data_tidy, aes(x = salary)) + 
  geom_histogram( color = "black", fill = "blue") + ggtitle("salary distribution")

#from the salary distribution plot, the histogram is skewed to the right.
#We can see that most of the players salary are less than a million

#salary summary
summary(player_data_tidy$salary)

#The median salary is $3258539
#The mean salary is $6967454 

#Does players with higher game points have higher salaries?
#Does playing positions affects player's salary?


#points against salary

ggplot(data = player_data_tidy, aes(x = PTS, y = salary, color = Pos)) +
  geom_point() + ggtitle("player points against salary")
  
#According to this graph, players that have higher points tend to have high salaries
#regardless of the position they player in.

#Bar plot of positions
ggplot(data = player_data_tidy, aes(x = Pos, fill = Pos)) + geom_bar() +
  ggtitle("Number of players for each position in the season 2018-19")

#There are a high number of Shooting guard players 
#in 2018-19 season compared to the other positions.
#Center position has the least number of players

#Games started by a player against points
#looking if games started by a player increases player's points.

ggplot(data = player_data_tidy, aes(x = GS, y = PTS)) + geom_point() +
  ggtitle("Games started by a player against Points") + geom_smooth(method = lm)

#This does not give a proper picture. 
#Because there are players that started fewer games but they still have high
#points


#Minutes played
#A player who played more minutes have higher points?

ggplot(data = player_data_tidy, aes(x = MP, y = PTS)) + geom_point() +
  ggtitle("Minutes played by a player vs Points") +geom_smooth(method = lm)

#This plot tell us that the more a player is given more minutes in playing, 
#that player can produce more points per game but not clear as 
#there are players who played moore minutes but still have low points.

#variable correlation
#removing variables that are not numeric for orrelation 
cor_data <- player_data_tidy %>% select(-c(player_name, Pos, Tm))
str(cor_data)

round(x = cor(cor_data), digits = 3)
ggcorrplot(cor(cor_data)) +ggtitle("Correlation headmap of the variables")

#We can see that salary is correlated with the other variables but not highly.

#Points though are highly correlated with Turnovers,Steals,Free Throw Attempts,
#Free Throws,2-point Field Goal Attempts,2-Point Field Goals,3-Point Field Goal Attempts,
#3-Point Field Goals,Field Goals,minutes Played,Games Started, Total Rebounds,Defensive Rebounds.

#Player's Age has no affects on points but has on salary.
#we can also see a high correlation among some of the variables.

#Features  extraction 
#extracting the variables to be used for the modeling
model_data <- player_data_tidy %>%
  select(c(player_name,salary,Pos,Age,Tm,G,GS,MP,FG,FGA,"3P","3PA","2P","2PA",FT,FTA,DRB,TRB,TOV,PTS))


#Data Modeling
#testing if players points explains salary
model1 <- lm(salary~PTS, data = model_data)

summary(model1)
tidy(model1, conf.int = TRUE)

#looking at the model summary, player's point does not explain salary well.
#makes a bit of sense since scoring high points are contributed to by other factors. 
#forexample, if a player played more minutes, there is a chance he can score more points. 
#so points only does not give a clear explanations,


#looking at other variables
#whether players points can be explain by other variables
model_data2 <- player_data_tidy %>%
  select(c(Pos,Age,G,GS,MP,FG,FGA,FT,FTA,DRB,TRB,TOV,PTS))

model2 <- lm(PTS~., data = model_data2)
summary(model2)
tidy(model2, conf.int = TRUE)

#By looking at this model, we can see that actually other variables 
#for example Minutes played contribute to a player having a high points.
#as this model has achieved an R-squared of 0.9979.
#we can say that if a player has a better statistics, the player
#tend to have high points. 

#Players points from highest to lowest. 
Rec_data <- player_data_tidy %>%
  select(c(player_name,salary,Pos,G,GS,MP,PTS)) %>% arrange(desc(PTS))

#Finding players by positions
#Conditions used to filter players: a player must atleast have played a minimum of 50 games, 
#1000 minute played, salary between 1 Million and 10 Million and minimum points of 1000

#Point guard players: 

pos_pg <- Rec_data %>% filter(Pos == "PG") %>% 
  filter(G >=50, MP>=1000, salary>=1000000, salary<=10000000, PTS>=1000) 
pos_pg

#Points per minute played (pmp)
pos_pg %>% mutate(pmp = PTS/MP) %>% 
  arrange(desc(pmp))



#The player to buy for the  position of Point Guard is 
#D'Angelo Russell: salary 7019698, games 81, games started 81, minute played 2448,
#points 1712 and points per minute played ratio 0.699. 
#He could be a value for money player for this position. He has been undervalued 
#in the market but with great points to minute played ratio.

#The other alternative player to consider for the position is 
#Spencer Dinwiddie, salary 1656092, games 68, game started 4, minute played 1914, 
#points 1143 and points per minute played 0.597. He cost less compared to other players
#whose stats are not as good as his. 

#Shooting guard
pos_sg <- Rec_data %>% filter(Pos == "SG") %>% 
  filter(G >=50, MP>=1000, salary>=1000000, salary<=10000000, PTS>=1000) 
pos_sg
  
#Points per minute played (pgmp)
  pos_sg%>% mutate(pmp = PTS/MP) %>% 
  arrange(desc(pmp))

#The good player to buy for the position of shooting guard is,
# Devin Booker: salary 3314365, games 64, games started 64, minute played 2242,
# points 1700 and points to minute played ratio 0.758. He is undervalued 
#in the market.


#small forward
pos_sf <- Rec_data %>% filter(Pos == "SF") %>% 
    filter(G >=50, MP>=1000, salary>=1000000, salary<=10000000, PTS>=1000) 
  pos_sf
  
#Points per minute played (pgmp)
  pos_sf %>% mutate(pmp = PTS/MP) %>% 
    arrange(desc(pmp))

#The good player to buy for the position of small forward is,
#Jayson Tatum: salary 6700800, games 79, games started 79, minute played 2455,
#points 1243 and points to minute played ratio 0.506. 
  
#power forward
  pos_pf <- Rec_data %>% filter(Pos == "PF") %>% 
    filter(G >=50, MP>=1000, salary>=1000000, salary<=10000000, PTS>=1000) 
  pos_pf
  
#Points per minute played (pgmp)
  pos_pf %>% mutate(pmp = PTS/MP) %>% 
    arrange(desc(pmp))

#player to buy for the position of power forward is,
#Julius Randle: salary 8641000, games 73, games started 49, minute played 2232  
#points 1565 and points per minute played ratio 0.701.

#Alternatively we can buy a cheaper option
#John Collins: salary 2299080, games 61, games started 59, minute played 1829 ,
#points 1188 and points per minute played ratio 0.650.
  
#center
 pos_c <- Rec_data %>% filter(Pos == "C") %>% 
    filter(G >=50, MP>=1000, salary>=1000000, salary<=10000000, PTS>=1000) 
  pos_c
  
#Points per minute played (pgmp)
  pos_c %>% mutate(pmp = PTS/MP) %>% 
    arrange(desc(pmp))

#Player to buy for the position of center is 
#Karl-Anthony Towns: salary 7839435, games 77, games started 77, minute played 2545,
#points 1880 and points to minute played ratio 0.739.
  

#Recommendations 
#Top Five starting players for Chicago to buy
  
recomend<- rbind(pos_c, pos_pf)
recomended <- rbind(recomend, pos_pg)    
recomended_pl <- rbind(recomended,pos_sf)
recomended_players <- rbind(recomended_pl, pos_sg) %>% mutate(pmp = PTS/MP) 
R <- recomended_players[c(1,6,12,19,21),] %>% arrange(desc(pmp))

118000000-sum(R$salary)

#D'Angelo Russell
#Karl-Anthony Towns
#Julius Randle
#Jayson Tatum
#Devin Booker



