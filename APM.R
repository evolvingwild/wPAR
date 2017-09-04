#################################################################################
#####     Adjusted Plus/Minus Testing       ||          09/02/17            #####
#################################################################################

setwd("~/RStudio/Hockey")

require(dplyr); require(ggplot2); require(ridge); require(car); require(glmnet)
require(caret); require(tibble); require(stringr)
options(scipen=999)




######    Extras    ######
fun.position <- function() {
  player_position <- read.csv("player_position.csv", stringsAsFactors = FALSE)
  pos_upd <- data.frame(matrix(nrow = 2, ncol = 2)); names(pos_upd) <- c("player", "position")
  pos_upd[1, 1] <- c("FRÉDÉRIC.ST-DENIS"); pos_upd[1, 2] <- 1
  pos_upd[2, 1] <- c("MICHAËL.BOURNIVAL"); pos_upd[2, 2] <- 1
  player_position <- player_position %>% rbind(., pos_upd) %>% arrange(player)
  return(player_position)
}
player_position <- fun.position()
scoreadj <- read.csv("ScoreAdj.csv")

## Objects - UPDATED event_types to match HTM names throughout (including functions below)
st.shot_events <- c("SHOT", "GOAL")
st.fenwick_events <- c("SHOT", "GOAL", "MISS")
st.corsi_events <- c("SHOT", "GOAL", "MISS", "BLOCK")
st.strength_states <- c("3v3", "5v5", "4v4", "5v4", "4v5", 
                        "5v3", "3v5", "4v3", "3v4", "5vE", 
                        "Ev5", "4vE", "Ev4", "3vE", "Ev3") %>% 
  as.factor()


# LOAD
setwd("~/RStudio/Hockey/pbp New HTM scrape files")
pbp1617 <- readRDS("pbp1617x.rds")
games_5v5_1617 <- readRDS("games_5v5_1617.rds")
setwd("~/RStudio/Hockey")



# The Setup 
################################

# Find qualified players from games data
fun.qualified <- function(data) {
  Qualified <- data %>% 
    ungroup() %>% group_by(player) %>% 
    summarise(TOI_5v5 = round(sum(TOI_5v5), 2)) %>% 
    left_join(., player_position) %>% 
    mutate(qual = ifelse(TOI_5v5 > 337 & position == 1, 1, 
                         ifelse(TOI_5v5 > 410 & position == 2, 1, 0))) %>% 
    select(player, qual)
  return(Qualified)
}
Qualified <- fun.qualified(games_5v5_1617) 

# Prepare pbp (initial)
fun.pbp_prepare <- function(data) {
  pbp_part <- data %>% filter(game_strength_state == "5v5") %>% 
    mutate(scradj = home_score - away_score, 
           home_lead = ifelse(scradj >= 3, 3, ifelse(scradj <= -3, -3, scradj)), 
           home_lead = home_lead + 4, 
           event_length = ifelse(is.na(event_length), 0, event_length), 
           event_team = ifelse(is.na(event_team), 0, event_team)
    )
  
  pbp_part <- pbp_part %>% 
    select(home_on_1:away_on_6, 
           home_goalie, away_goalie, 
           home_team, away_team, 
           event_length, 
           event_team, 
           event_type, 
           home_lead
    )
}
pbp_part <- fun.pbp_prepare(pbp1617)

# Create data frame with all players, goalies, teams, and event_types and their respective IDs
fun.names_match <- function(sub_data2) {
  # Skaters
  player_position <- player_position %>% ungroup %>% 
    mutate(ID = row_number() + 10000)
  
  # Goalies
  fun.goalie_find <- function(data) {
    goalie_h <- data.frame(unique(data$home_goalie))
    names(goalie_h) <- c("goalie")
    goalie_a <- data.frame(unique(data$away_goalie))
    names(goalie_a) <- c("goalie")
    
    goalie_all <- rbind(goalie_h, goalie_a)
    goalie_all <- data.frame(unique(goalie_all$goalie))
    names(goalie_all) <- c("player")
    goalie_all <- goalie_all %>% arrange(player)
    
    return(goalie_all)
  }
  goalies <- fun.goalie_find(sub_data2)
  goalies <- goalies %>% mutate(position = NA, ID = NA)
  goalies$position <- 3
  goalies <- goalies %>% arrange(player) %>% 
    mutate(ID = row_number() + 20000)
  
  # Teams
  teams_str <- unique(na.omit(sub_data2$event_team))
  teams <- data.frame(matrix(nrow = length(teams_str), ncol = 3))
  names(teams) <- c("player", "position", "ID")
  teams$player <- teams_str
  teams$position <- 4
  teams <- teams %>% arrange(player) %>% 
    mutate(ID = row_number())
  
  # Event Type
  event_str <- unique(na.omit(sub_data2$event_type))
  event <- data.frame(matrix(nrow = length(event_str), ncol = 3))
  names(event) <- c("player", "position", "ID")
  event$player <- event_str
  event$position <- 5
  event <- event %>% arrange(player) %>% 
    mutate(ID = row_number() + 100)
  
  # Combine
  all <- rbind(teams, player_position, goalies, event)
  return(all)
}
names_match <- fun.names_match(pbp_part)

# Convert prepared pbp data frame to all numeric values
fun.IDs <- function(data) {
  data$home_on_1 <- names_match$ID[match(data$home_on_1, names_match$player)]
  data$home_on_2 <- names_match$ID[match(data$home_on_2, names_match$player)]
  data$home_on_3 <- names_match$ID[match(data$home_on_3, names_match$player)]
  data$home_on_4 <- names_match$ID[match(data$home_on_4, names_match$player)]
  data$home_on_5 <- names_match$ID[match(data$home_on_5, names_match$player)]
  data$home_on_6 <- names_match$ID[match(data$home_on_6, names_match$player)]
  
  data$away_on_1 <- names_match$ID[match(data$away_on_1, names_match$player)]
  data$away_on_2 <- names_match$ID[match(data$away_on_2, names_match$player)]
  data$away_on_3 <- names_match$ID[match(data$away_on_3, names_match$player)]
  data$away_on_4 <- names_match$ID[match(data$away_on_4, names_match$player)]
  data$away_on_5 <- names_match$ID[match(data$away_on_5, names_match$player)]
  data$away_on_6 <- names_match$ID[match(data$away_on_6, names_match$player)]
  
  data$home_goalie <- names_match$ID[match(data$home_goalie, names_match$player)]
  data$away_goalie <- names_match$ID[match(data$away_goalie, names_match$player)]
  
  data$event_team <- names_match$ID[match(data$event_team, names_match$player)]
  data$home_team <- names_match$ID[match(data$home_team, names_match$player)]
  data$away_team <- names_match$ID[match(data$away_team, names_match$player)]
  
  data$event_type <- names_match$ID[match(data$event_type, names_match$player)]
  
  #sparse_mat <- data.matrix(data)
  #sparse_mat <- Matrix(sparse_mat, sparse = TRUE)
  
  return(data)
}
pbp_part <- fun.IDs(pbp_part)

# Create Dummy Variables for all Qualified Players, results in sparse matrix for testing
fun.APM_dummy <- function(data) {
  print("home_df")
  ## Home
  ################################
  test.H <- data %>% group_by(home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, 
                              away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6) %>% 
    summarise(goalie = first(away_goalie), 
              team = first(home_team), 
              length = sum(event_length), 
              CF60 = round((sum(ifelse(event_type %in% c(101, 105, 107, 111) & 
                                         event_team == home_team, (1*scoreadj[home_lead, 2]), 0))/length)*3600, 2), 
              GF60 = round((sum(ifelse(event_type == 105 & 
                                         event_team == home_team, 1, 0))/length)*3600, 2)
    ) %>% 
    ungroup() %>% 
    add_column(., n = 0, .before = 1) %>% 
    mutate(n = as.numeric(row_number()), 
           is_home = 1) %>% data.frame()
  
  # Names
  fun.mat_names <- function(sub_data1) {
    h1 <- unique(sub_data1$home_on_1) 
    h2 <- unique(sub_data1$home_on_2)
    h3 <- unique(sub_data1$home_on_3)
    h4 <- unique(sub_data1$home_on_4) 
    h5 <- unique(sub_data1$home_on_5)
    h6 <- unique(sub_data1$home_on_6)
    
    a1 <- unique(sub_data1$away_on_1) 
    a2 <- unique(sub_data1$away_on_2)
    a3 <- unique(sub_data1$away_on_3)
    a4 <- unique(sub_data1$away_on_4) 
    a5 <- unique(sub_data1$away_on_5)
    a6 <- unique(sub_data1$away_on_6)
    
    mat <- data.frame(matrix(nrow = (length(h1) + length(h2) + length(h3) + length(h4) + length(h5) + length(h6) + 
                                       length(a1) + length(a2) + length(a3) + length(a4) + length(a5) + length(a6)
    ), 
    ncol = 1)
    )
    names(mat) <- "player"
    mat$player <- c(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6)
    mat_unique <- data.frame(mat[!duplicated(mat[,c("player")]), ])
    names(mat_unique) <- "player"
    
    Qualified$player <- names_match$ID[match(Qualified$player, names_match$player)]
    
    mat_all <- mat_unique %>% left_join(., Qualified) %>% 
      mutate(qual = ifelse(is.na(qual), 2, qual))
    
    mat_all <- mat_all %>% filter(qual > 0)
    
    return(mat_all)
  }
  mat_unique <- fun.mat_names(test.H)
  
  print("home_loop_o")
  # Home Offense
  for(name in mat_unique$player) {
    #x <- mat_unique$qual[match(i, mat_unique$player)]
    
    test.H[paste(name, ".o", sep="")] <- 
      ifelse(test.H$home_on_1 == name & test.H$home_on_1 != test.H$goalie | 
               test.H$home_on_2 == name & test.H$home_on_2 != test.H$goalie |
               test.H$home_on_3 == name & test.H$home_on_3 != test.H$goalie |
               test.H$home_on_4 == name & test.H$home_on_4 != test.H$goalie |
               test.H$home_on_5 == name & test.H$home_on_5 != test.H$goalie |
               test.H$home_on_6 == name & test.H$home_on_6 != test.H$goalie, 1, 0
      )
  }
  print("home_loop_d")
  # Home Defense
  for(name in mat_unique$player) {
    #x <- mat_unique$qual[match(i, mat_unique$player)]
    
    test.H[paste(name, ".d", sep="")] <- 
      ifelse(test.H$away_on_1 == name |
               test.H$away_on_2 == name |
               test.H$away_on_3 == name |
               test.H$away_on_4 == name |
               test.H$away_on_5 == name |
               test.H$away_on_6 == name, 1, 0
      )
  }
  
  test.H <- data.matrix(test.H)
  test_sparse.H <- Matrix(test.H, sparse = TRUE)
  rm(test.H)
  
  print("away_df")
  ## Away
  ################################
  test.A <- data %>% group_by(home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, 
                              away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6) %>% 
    summarise(goalie = first(home_goalie), 
              team = first(away_team), 
              length = sum(event_length), 
              CF60 = round((sum(ifelse(event_type %in% c(101, 105, 107, 111) & 
                                         event_team == away_team, (1*scoreadj[home_lead, 3]), 0))/length)*3600, 2), 
              GF60 = round((sum(ifelse(event_type == 105 & 
                                         event_team == away_team, 1, 0))/length)*3600, 2)
    ) %>% 
    ungroup() %>% 
    add_column(., n = 0, .before = 1) %>% 
    mutate(n = row_number(), 
           is_home = 0) %>% data.frame() 
  
  print("away_loop_o")
  # Away Offense
  for(name in mat_unique$player) {
    #x <- mat_unique$qual[match(i, mat_unique$player)]
    
    test.A[paste(name, ".o", sep="")] <- 
      ifelse(test.A$away_on_1 == name & test.A$away_on_1 != test.A$goalie | 
               test.A$away_on_2 == name & test.A$away_on_2 != test.A$goalie |
               test.A$away_on_3 == name & test.A$away_on_3 != test.A$goalie |
               test.A$away_on_4 == name & test.A$away_on_4 != test.A$goalie |
               test.A$away_on_5 == name & test.A$away_on_5 != test.A$goalie |
               test.A$away_on_6 == name & test.A$away_on_6 != test.A$goalie, 1, 0
      )
  }
  print("away_loop_d")
  # Away Defense
  for(name in mat_unique$player) {
    #x <- mat_unique$qual[match(i, mat_unique$player)]
    
    test.A[paste(name, ".d", sep="")] <- 
      ifelse(test.A$home_on_1 == name |
               test.A$home_on_2 == name |
               test.A$home_on_3 == name |
               test.A$home_on_4 == name |
               test.A$home_on_5 == name |
               test.A$home_on_6 == name, 1, 0
      )
  }
  
  test.A <- data.matrix(test.A)
  test_sparse.A <- Matrix(test.A, sparse = TRUE)
  rm(test.A)
  
  print("combine")
  ## Combine Home / Away (O / D)
  ################################
  test_all <- rbind(test_sparse.H, test_sparse.A)
  rm(test_sparse.H)
  rm(test_sparse.A)
  return(test_all)
}
testAPM <- fun.APM_dummy(pbp_part)


# The Model 
################################
GF60 <- testAPM[, 18]
CF60 <- testAPM[, 17]
length <- testAPM[, 16]
test_sparse <- testAPM[, -c(1:18)]

rm(testAPM)

lambda <- cv.glmnet(test_sparse, 
                    #GF60, 
                    CF60, 
                    weights = length, nfolds = 10)

lambda_min <- lambda$lambda.min

ridge <- glmnet(test_sparse, 
                #GF60, 
                CF60, 
                family = c("gaussian"), length, alpha = 0, lambda = lambda_min)

# Bind into df
fun.APM_bind <- function() {
  APM <- data.frame(as.matrix(coef(ridge, s = lambda_min)))
  APM_names <- dimnames(coef(ridge))[[1]]
  APM_test <- cbind(APM_names, APM)
  
  # Remove .d / .o suffixes
  APM_test_d <- APM_test %>% filter(grepl(".d", APM_names), APM_names != "is_home") %>% 
    mutate(APM_names = gsub(".d", "", APM_names)
    ) %>% rename(Def = X1)
  APM_test_o <- APM_test %>% filter(grepl(".o", APM_names), APM_names != "is_home") %>% 
    mutate(APM_names = gsub(".o", "", APM_names)
    ) %>% rename(Off = X1)
  
  # Join
  APM_all <- APM_test_o %>% left_join(., APM_test_d) %>% 
    mutate(APM = Off - Def)
  
  APM_all$APM_names <- names_match$player[match(APM_all$APM_names, names_match$ID)]
  
  return(APM_all)
}
APM_test <- fun.APM_bind()

APM_test <- APM_test %>% rename(player = APM_names) %>% 
  left_join(., names_match, by = c("player")) %>% filter(position != 3)

APM_test <- APM_test %>% left_join(., Qualified)



# Testing Impact
TOI_1617 <- games_5v5_1617 %>% ungroup() %>% group_by(player) %>% 
  summarise(TOI_5v5 = sum(TOI_5v5), 
            Team = first(Team)) %>% 
  rename(APM_names = player)

APM_test_goals <- left_join(APM_all_goals, TOI_1617)
APM_qual_goals <- APM_test_goals %>% filter(TOI_5v5 > 300) %>% 
  mutate(O_impact = Off*(TOI_5v5/60), 
         D_impact = Def*(TOI_5v5/60), 
         All_impact = O_impact - D_impact)

APM_test1 <- APM_qual


