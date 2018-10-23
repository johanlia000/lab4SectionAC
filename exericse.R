# Exercise 7: DPLYR practice with NBA data
# install.packages("dplyr")
library(dplyr)

# Read in the NBA team data of the 2016-2017 season from the data directory  
# into a variable called `team.data` using `read.csv`
team.data <- read.csv("data/teams.csv", stringsAsFactors = FALSE)


# The data.frame team.data should now be accessible to you. 
# View it, and get some basic information about the number of rows/columns. 
# Note the "X" preceding some of the column titles as well as the "*" 
# following the names of teams that made it to the playoffs that year.
View(team.data)


# Add a column that gives the turnovers to steals ratio (TOV / STL) for each team
team.data$stealsRatio <- (team.data$TOV / team.data$STL)

## I accidently made another column but not sure how to delete it....

# Sort the teams from lowest turnover/steal ratio to highest
team.data %>% 
  arrange(stealsRatio)

#Find the average BLK and STL for teams having a TOV greater than the average TOV of all teams

## use this inside the filter: 
  # avg_tov_allteams <- mean(team.data$TOV) # the average of TOV of all teams

teams_greater_avg_TOV <- team.data %>% # find teams that have greater TOV than average
  filter(TOV > mean(team.data$TOV))

avg_blk <- mean(teams_greater_avg_TOV$BLK)
avg_stl <- mean(teams_greater_avg_TOV$STL)

# Get the team that had the highest Total Rebounds (TRB) only with the columns 
# Team and TRB  *using one line of code*
TRB_best_teams <- team.data %>% 
  select(Team, TRB) %>% 
  arrange(desc(TRB)) %>% 
  mutate(place = row_number()) %>%
  filter(place == 1) %>% 
  head()

# Print only the name of the team that had the highest total rebounds
# (that also happens to be the greatest team of all time)
paste(TRB_best_teams$Team)

## Let's change gears!

# Read in the Pokemon data from the data directory  
# into a variable called `pokemon` using `read.csv`. Remember to not read strings in as factors.
pokemon <- read.csv("data/pokemon.csv", stringsAsFactors = FALSE)

# First, View() the data set to see what info you have to work with 
View(pokemon)

# Find all the Pokemon that are "Water" or "Ghost" Type 1 and have a speed higher than 50
pokemon %>% 
  filter((Type.1 == "Water" | Type.1 == "Ghost") & Speed > 50) %>% 
  head()

# Find the average HP, median HP, min HP and max HP for each of the type of generations
pokemon %>% 
  group_by(Generation) %>% 
  summarize(mean = mean(HP), median = median(HP), max = max(HP))

#FIND THE NUMBER OF POKEMONS THAT ARE LEGENDARY AND THE NUMBER OF POKEMONS THAT ARE NOT LEGENDARY 
#THAT HAVE A HIGHER ATTACK THAN DEFENSE VALUE
#NOTE: IT SHOULD BE DONE IN ONE RUN OF DPLYR
#HINT : Look into count() function by dplyr
pokemon %>% 
  group_by(Legendary) %>% 
  count()

#Find the generation that has the most number of pokemons with Type.1 as "Fire"
pokemon %>% 
  group_by(Generation) %>% 
  filter(Type.1 == "Fire") %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  filter()

# WHICH Type 1 pokemon has the overall within group max value of SUM of HP, Attack, Defense, Sp..Att, Sp.Def, Speed
#HINT : LOOK AT THE DATASET! - THERE IS A SUPER EASY WAY TO DO THIS.

