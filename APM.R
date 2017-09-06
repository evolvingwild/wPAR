#################################################################################
#####     Adjusted Plus/Minus Testing       ||          09/02/17            #####
#################################################################################

setwd("~/RStudio/Hockey")

require(dplyr); require(ggplot2); require(glmnet)
#require(ridge); require(car); require(caret)
require(tibble); require(stringr)
options(scipen=999); 

# Set Seed
set.seed(123)


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


# Setup 
################################

# Qualifying TOI 

# Goalie Qualifying
fun.goalie_qual <- function(data, G_cutoff) {
  
  # data should be a pbp data frame - I used Manny's scraper to compile this
  
  data <- data %>% filter(game_strength_state == "5v5")
  
  pbp_goalie_H <- data %>% group_by(home_goalie, season) %>% 
    summarise(TOI = sum(event_length)) %>% filter(!is.na(TOI) & home_goalie != 50) %>% rename(player = home_goalie)
  
  pbp_goalie_A <- data %>% group_by(away_goalie, season) %>% 
    summarise(TOI = sum(event_length)) %>% filter(!is.na(TOI) & away_goalie != 50) %>% rename(player = away_goalie)
  
  pbp_goalie <- pbp_goalie_A %>% rbind(., pbp_goalie_H) %>% group_by(player) %>% 
    summarise(TOI = sum(TOI/60)) %>% 
    mutate(qual = ifelse(TOI > G_cutoff, 1, 0)) %>% 
    select(player, qual)
}
goalie_qual <- fun.goalie_qual(pbp_full, 4700)

# Find qualified players from games data - USE FOR GOALS REGRESSION
fun.qualified_GF <- function(data, F_cutoff, D_cutoff) {
  
  # data should be a data frame that includes player totals for a span of games. Only TOI is required here
  
  Qualified <- data %>% 
    ungroup() %>% group_by(player) %>% 
    summarise(TOI_5v5 = round(sum(TOI_5v5), 2)) %>% 
    left_join(., player_position) %>% 
    mutate(qual = ifelse(TOI_5v5 > F_cutoff & position == 1, 1, 
                         ifelse(TOI_5v5 > D_cutoff & position == 2, 1, 0))) %>% 
    select(player, qual)
  
  Qualified <- rbind(Qualified, goalie_qual)
  
  return(Qualified)
}
Qualified <- fun.qualified_GF(games_full, 3230, 3930)

# Find qualified players from games data - USE FOR SHOTS (CF / xGF) REGRESSION
fun.qualified_shots <- function(data, F_cutoff, D_cutoff) {
  
  # data should be a data frame that includes player totals for a span of games. Only TOI is required here
  
  Qualified <- data %>% 
    ungroup() %>% group_by(player) %>% 
    summarise(TOI_5v5 = round(sum(TOI_5v5), 2)) %>% 
    left_join(., player_position) %>% 
    mutate(qual = ifelse(TOI_5v5 > F_cutoff & position == 1, 1, 
                         ifelse(TOI_5v5 > D_cutoff & position == 2, 1, 0))) %>% 
    select(player, qual) %>% data.frame()
  
  return(Qualified)
}
Qualified_shots <- fun.qualified_shots(games_full, 3230, 3930)

rm(goalie_qual)

# Prepare pbp (initial)
fun.pbp_prepare <- function(data) {
  
  # The same pbp data frame that is used for fun.goalie_qual should be used here
  # scradj is a data frame that lists the score adjustments per Micah's method
  
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
           home_lead, 
           prob_goal
           )
}
pbp_part <- fun.pbp_prepare(pbp_full)

rm(pbp_full)

# Create data frame with all players, goalies, teams, and event_types and their respective IDs
fun.names_match <- function(sub_data2) {
  
  # player_position is a data frame that includes all unique skater names from the past 10 seasons in column 1 and 
  # their respective position (F = 1, D = 2) in column two. It should be loaded prior to running this function. 
  
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

# Identify and save specific event type IDs for dummy function/creation below
st.corsi_events <- names_match[which(names_match[, 1] %in% c("SHOT", "GOAL", "MISS", "BLOCK")), 3]
st.fenwick_events <- names_match[which(names_match[, 1] %in% c("SHOT", "GOAL", "MISS")), 3]
st.goal_ID <- names_match[which(names_match[, 1] %in% c("GOAL")), 3] 

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
  
  return(data)
}
pbp_part <- fun.IDs(pbp_part)

rm(games_full)

# Create Dummy Variables for all Qualified Players, returns a sparse matrix for testing
fun.APM_dummy <- function(data, type) {
  
  # type should be either "GF" or "shots" (CF / xGF) depending on which which regression is preferred
  
  if(type == "GF") {
    print("home_df_GF")
    ## Home
    ################################
    test.H <- data %>% group_by(home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, 
                                away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6) %>% 
      summarise(home_goalie = first(home_goalie), 
                away_goalie = first(away_goalie), 
                team = first(home_team), 
                length = sum(event_length),
                GF60 = round((sum(ifelse(event_type == st.goal_ID & 
                                           event_team == home_team, 1, 0))/length)*3600, 2)
                ) %>% 
      ungroup() %>% 
      add_column(., n = 0, .before = 1) %>% 
      mutate(n = as.numeric(row_number()), 
             is_home = 1) %>% data.frame()
    
    print("unique_names")
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
                               ncol = 1
                               )
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
      
      test.H[paste(name, ".o", sep="")] <- 
        ifelse(test.H$home_on_1 == name & name != test.H$home_goalie & name != test.H$away_goalie | 
               test.H$home_on_2 == name & name != test.H$home_goalie & name != test.H$away_goalie |
               test.H$home_on_3 == name & name != test.H$home_goalie & name != test.H$away_goalie |
               test.H$home_on_4 == name & name != test.H$home_goalie & name != test.H$away_goalie |
               test.H$home_on_5 == name & name != test.H$home_goalie & name != test.H$away_goalie |
               test.H$home_on_6 == name & name != test.H$home_goalie & name != test.H$away_goalie, 1, 0
               )
    }
    print("home_loop_d")
    # Home Defense
    for(name in mat_unique$player) {
      
      test.H[paste(name, ".d", sep="")] <- 
        ifelse(test.H$away_on_1 == name & name != test.H$home_goalie |
               test.H$away_on_2 == name & name != test.H$home_goalie |
               test.H$away_on_3 == name & name != test.H$home_goalie |
               test.H$away_on_4 == name & name != test.H$home_goalie |
               test.H$away_on_5 == name & name != test.H$home_goalie |
               test.H$away_on_6 == name & name != test.H$home_goalie, 1, 0
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
      summarise(home_goalie = first(home_goalie), 
                away_goalie = first(away_goalie),  
                team = first(away_team), 
                length = sum(event_length), 
                GF60 = round((sum(ifelse(event_type == st.goal_ID & 
                                           event_team == away_team, 1, 0))/length)*3600, 2)
                ) %>% 
      ungroup() %>% 
      add_column(., n = 0, .before = 1) %>% 
      mutate(n = row_number(), 
             is_home = 0) %>% data.frame() 
    
    print("away_loop_o")
    # Away Offense
    for(name in mat_unique$player) {
      
      test.A[paste(name, ".o", sep="")] <- 
        ifelse(test.A$away_on_1 == name & name != test.A$away_goalie & name != test.A$home_goalie | 
               test.A$away_on_2 == name & name != test.A$away_goalie & name != test.A$home_goalie |
               test.A$away_on_3 == name & name != test.A$away_goalie & name != test.A$home_goalie |
               test.A$away_on_4 == name & name != test.A$away_goalie & name != test.A$home_goalie |
               test.A$away_on_5 == name & name != test.A$away_goalie & name != test.A$home_goalie |
               test.A$away_on_6 == name & name != test.A$away_goalie & name != test.A$home_goalie, 1, 0
               )
    }
    print("away_loop_d")
    # Away Defense
    for(name in mat_unique$player) {
      
      test.A[paste(name, ".d", sep="")] <- 
        ifelse(test.A$home_on_1 == name & name != test.A$away_goalie |
               test.A$home_on_2 == name & name != test.A$away_goalie |
               test.A$home_on_3 == name & name != test.A$away_goalie |
               test.A$home_on_4 == name & name != test.A$away_goalie |
               test.A$home_on_5 == name & name != test.A$away_goalie |
               test.A$home_on_6 == name & name != test.A$away_goalie, 1, 0
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
  
  else if(type == "shots") {
    print("home_df_shots")
    ## Home
    ################################
    test.H <- data %>% group_by(home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, 
                                away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6) %>% 
      summarise(team = first(home_team), 
                length = sum(event_length),
                CF60 = round((sum(ifelse(event_type %in% st.corsi_events & 
                                           event_team == home_team, (1*scoreadj[home_lead, 2]), 0))/length)*3600, 2),
                xGF60 = round((sum(ifelse(event_type %in% st.fenwick_events & 
                                            event_team == home_team, (prob_goal*(1*scoreadj[home_lead, 2])), 0))/length)*3600, 2)
                ) %>% 
      ungroup() %>% 
      add_column(., n = 0, .before = 1) %>% 
      mutate(n = as.numeric(row_number()), 
             is_home = 1) %>% data.frame()
    
    print("unique_names")
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
                               ncol = 1
                               )
                        )
      names(mat) <- "player"
      mat$player <- c(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6)
      mat_unique <- data.frame(mat[!duplicated(mat[,c("player")]), ])
      names(mat_unique) <- "player"
      
      Qualified_shots$player <- names_match$ID[match(Qualified_shots$player, names_match$player)]
      
      mat_all <- mat_unique %>% left_join(., Qualified_shots) %>% filter(qual > 0)
      
      return(mat_all)
    }
    mat_unique <- fun.mat_names(test.H)
    
    print("home_loop_o")
    # Home Offense
    for(name in mat_unique$player) {
      
      test.H[paste(name, ".o", sep="")] <- 
        ifelse(test.H$home_on_1 == name | 
               test.H$home_on_2 == name |
               test.H$home_on_3 == name |
               test.H$home_on_4 == name |
               test.H$home_on_5 == name |
               test.H$home_on_6 == name, 1, 0
               )
    }
    print("home_loop_d")
    # Home Defense
    for(name in mat_unique$player) {
      
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
    
    print("away_df_shots")
    ## Away
    ################################
    test.A <- data %>% group_by(home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, 
                                away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6) %>% 
      summarise(team = first(away_team), 
                length = sum(event_length), 
                xGF60 = round((sum(ifelse(event_type %in% st.fenwick_events & 
                                            event_team == away_team, (prob_goal*(1*scoreadj[home_lead, 3])), 0))/length)*3600, 2), 
                CF60 = round((sum(ifelse(event_type %in% st.corsi_events & 
                                           event_team == away_team, (1*scoreadj[home_lead, 3]), 0))/length)*3600, 2)
                ) %>% 
      ungroup() %>% 
      add_column(., n = 0, .before = 1) %>% 
      mutate(n = row_number(), 
             is_home = 0) %>% data.frame() 
    
    print("away_loop_o")
    # Away Offense
    for(name in mat_unique$player) {
      
      test.A[paste(name, ".o", sep="")] <- 
        ifelse(test.A$away_on_1 == name | 
               test.A$away_on_2 == name |
               test.A$away_on_3 == name |
               test.A$away_on_4 == name |
               test.A$away_on_5 == name |
               test.A$away_on_6 == name, 1, 0
               )
    }
    print("away_loop_d")
    # Away Defense
    for(name in mat_unique$player) {
      
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
  
}
APM <- fun.APM_dummy(pbp_part, "shots")


# The Models
################################

## Goals
# Response / Weights
GF60 <- list(APM[, 20])
length_l <- list(APM[, 17])
# Remove NaNs
length <- unlist(rapply(length_l, f=function(x) ifelse(x == 0, 1, x), how = "replace"))
GF60 <- unlist(rapply(GF60_l, f=function(x) ifelse(is.nan(x), 1, x), how = "replace"))
# Make Predictors
APM_g <- APM[, -c(1:20)]
# Cleanup
rm(APM, length, CF60, xGF60)


## Shots
# Response / Weights
length_l <- list(APM[, 15])
CF60_l <- list(APM[, 16])
xGF60_l <- list(APM[, 17])
# Remove NaNs
length <- unlist(rapply(length_l, f=function(x) ifelse(x == 0, 1, x), how = "replace"))
CF60 <- unlist(rapply(CF60_l, f=function(x) ifelse(is.nan(x), 0, x), how = "replace"))
xGF60 <- unlist(rapply(xGF60_l, f=function(x) ifelse(is.nan(x), 0, x), how = "replace"))
# Make Predictors
APM_s <- APM[, -c(1:17)]
# Cleanup
rm(APM, length_l, CF60_l, xGF60_l)


# CV, and Ridge
CV_results <- cv.glmnet(APM_s, 
                        #GF60, 
                        CF60, 
                        #xGF60, 
                        weights = length, nfolds = 10)

lambda_min <- CV_results$lambda.min


ridge <- glmnet(APM_s, 
                #GF60, 
                CF60, 
                #xGF60,  
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
APM_initial <- fun.APM_bind()

APM <- APM_initial %>% rename(player = APM_names) %>% 
  left_join(., names_match, by = c("player"))

APM <- APM %>% left_join(., Qualified_shots)



# Testing Impact
TOI_full <- games_full %>% ungroup() %>% group_by(player) %>% 
  summarise(TOI_5v5 = sum(TOI_5v5))

APM_join <- left_join(APM, TOI_full)
APM_all <- APM_join %>% 
  mutate(O_impact = Off*(TOI_5v5/60), 
         D_impact = Def*(TOI_5v5/60), 
         Corsi_impact = O_impact - D_impact, 
         Off = ifelse(Off != 0 & position == 3, 0, Off), 
         APM = ifelse(player == "MARC-ANDRE.FLEURY", Off - Def, APM)) %>% 
  select(player, position, TOI_5v5, Off:APM, O_impact:Corsi_impact) %>% 
  mutate_each(funs(round(., 2)), Off:Corsi_impact) %>% rename(CPM = APM)


