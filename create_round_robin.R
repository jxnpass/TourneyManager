library(googlesheets4)
library(tidyverse)

source("functions.R")

# set stack to TRUE to print all games/pools on one sheet
# set stack to FALSE for printing each pool per sheet
# set stack to "BOTH" to have both stacked and pool specific
stack <- "BOTH" # SET HERE

# copy/paste the link to the "Teams List" sheet
# SET LINK HERE
link <- 'https://docs.google.com/spreadsheets/d/1eCC9_43aua6a_YkqlNo_lh17zN3fFBxsDAmNnN1jnK0/edit?gid=96325893#gid=96325893'

# authentication required, follow instructions given
tourney_info <- read_sheet(link, sheet = "Teams List", range = "A2:D") 
pools <- unique(tourney_info$Pool)
max_games <- as.numeric(colnames(tourney_info)[4])

matchups <- list()
for (pool in pools){
  
  # pull teams in selected pool
  teams <- tourney_info %>% 
    filter(Pool == pool) %>% 
    pull(`Team Name`)
  
  # create round robin matchups
  rr <- round_robin(teams = teams, alphabetical = F)
  
  # convert to df
  rr_df <- to_df(rr) 
  
  # filter to max_games only
  rr_df_lim <- rr_df %>% 
    mutate(Round_Int = str_extract(Round, "\\d")) %>% 
    filter(Round_Int <= max_games) %>% 
    select(-Round_Int)
  
  # save results
  matchups[[pool]] <- rr_df_lim
}

print(matchups[[pools[1]]]) # check if it looks good

# write to google sheet
to_sheets(matchups, link, stack)



