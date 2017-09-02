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

counts_adjust_allsit <- readRDS("counts_adjust_allsit.rds")
counts_adjust_5v5 <- readRDS("counts_adjust_5v5.rds")


## Objects - UPDATED event_types to match HTM names throughout (including functions below)
st.shot_events <- c("SHOT", "GOAL")
st.fenwick_events <- c("SHOT", "GOAL", "MISS")
st.corsi_events <- c("SHOT", "GOAL", "MISS", "BLOCK")
st.strength_states <- c("3v3", "5v5", "4v4", "5v4", "4v5", 
                        "5v3", "3v5", "4v3", "3v4", "5vE", 
                        "Ev5", "4vE", "Ev4", "3vE", "Ev3") %>% 
  as.factor()



######    Testing    ######
###########################
setwd("~/RStudio/Hockey/pbp New HTM scrape files")
pbp1617 <- readRDS("pbp1617x.rds")
games_5v5_1617 <- readRDS("games_5v5_1617.rds")


# Prep
pbp_part <- pbp1617 %>% filter(game_id < 2016020050, 
                               game_strength_state == "5v5") %>% 
  mutate(scradj = home_score - away_score, 
         home_lead = ifelse(scradj >= 3, 3, ifelse(scradj <= -3, -3, scradj)), 
         home_lead = home_lead + 4, 
         event_length = ifelse(is.na(event_length), 0, event_length))


#### SET DIRECTORY ####

# Prepare TOI For Qualifying (will be total 5v5 TOI over entire stretch of RAPM)
# Selects player and qual after for testing in loop when writing dummies
Qualified <- games_5v5_1617 %>% 
  ungroup() %>% group_by(player) %>% 
  summarise(TOI_5v5 = round(sum(TOI_5v5), 2)) %>% 
  left_join(., player_position) %>% 
  mutate(qual = ifelse(TOI_5v5 > 337 & position == 1, 1, 
                       ifelse(TOI_5v5 > 410 & position == 2, 1, 0)
  )
  ) %>% 
  select(player, qual)


# APM All
fun.APM <- function(data, folds) {
  # BEFORE running function, load:
  #     player_position
  #     scoreadj
  #
  # Change GF60 to CF 60 or vice-versa depending on situation
  
  
  ## Home
  ################################
  test.H <- data %>% group_by(home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, 
                              away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6) %>% 
    summarise(goalie = first(away_goalie), 
              team = first(home_team), 
              length = sum(event_length), 
              CF60 = round((sum(ifelse(event_type %in% st.corsi_events & 
                                         event_team == home_team, (1*scoreadj[home_lead, 2]), 0))/length)*3600, 2), 
              GF60 = round((sum(ifelse(event_type == "GOAL" & 
                                         event_team == home_team, 1, 0))/length)*3600, 2)
    ) %>% 
    data.frame() %>% 
    ungroup() %>% 
    add_column(., n = 0, .before = 1) %>% 
    mutate(n = row_number(), 
           is_home = 1)
  
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
    mat_unique$player <- as.character(mat_unique$player)
    mat_all <- mat_unique %>% left_join(., Qualified) %>% 
      mutate(qual = ifelse(is.na(qual), 2, qual))
    
    return(mat_all)
  }
  mat_unique <- fun.mat_names(test.H)
  
  # Home Offense
  for(i in mat_unique$player) {
    x <- mat_unique$qual[match(i, mat_unique$player)]
    
    test.H[paste(i, ".o", sep="")] <- 
      ifelse(test.H$home_on_1 == i & test.H$home_on_1 != test.H$goalie & x > 0 | 
               test.H$home_on_2 == i & test.H$home_on_2 != test.H$goalie & x > 0 | 
               test.H$home_on_3 == i & test.H$home_on_3 != test.H$goalie & x > 0 | 
               test.H$home_on_4 == i & test.H$home_on_4 != test.H$goalie & x > 0 | 
               test.H$home_on_5 == i & test.H$home_on_5 != test.H$goalie & x > 0 | 
               test.H$home_on_6 == i & test.H$home_on_6 != test.H$goalie & x > 0, 1, 0
      )
  }
  # Home Defense
  for(i in mat_unique$player) {
    x <- mat_unique$qual[match(i, mat_unique$player)]
    
    test.H[paste(i, ".d", sep="")] <- 
      ifelse(test.H$away_on_1 == i & x > 0 | 
               test.H$away_on_2 == i & x > 0 | 
               test.H$away_on_3 == i & x > 0 | 
               test.H$away_on_4 == i & x > 0 | 
               test.H$away_on_5 == i & x > 0 | 
               test.H$away_on_6 == i & x > 0, 1, 0
      )
  }
  
  
  ## Away
  ################################
  test.A <- data %>% group_by(home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, 
                              away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6) %>% 
    summarise(goalie = first(home_goalie), 
              team = first(away_team), 
              length = sum(event_length), 
              CF60 = round((sum(ifelse(event_type %in% st.corsi_events & 
                                         event_team == away_team, (1*scoreadj[home_lead, 3]), 0))/length)*3600, 2), 
              GF60 = round((sum(ifelse(event_type == "GOAL" & 
                                         event_team == away_team, 1, 0))/length)*3600, 2)
    ) %>% 
    data.frame() %>% 
    ungroup() %>% 
    add_column(., n = 0, .before = 1) %>% 
    mutate(n = row_number(), 
           is_home = 0)
  
  # Names
  mat_unique <- fun.mat_names(test.A)
  
  # Away Offense
  for(i in mat_unique$player) {
    x <- mat_unique$qual[match(i, mat_unique$player)]
    
    test.A[paste(i, ".o", sep="")] <- 
      ifelse(test.A$away_on_1 == i & test.A$away_on_1 != test.A$goalie & x > 0 | 
               test.A$away_on_2 == i & test.A$away_on_2 != test.A$goalie & x > 0 | 
               test.A$away_on_3 == i & test.A$away_on_3 != test.A$goalie & x > 0 | 
               test.A$away_on_4 == i & test.A$away_on_4 != test.A$goalie & x > 0 | 
               test.A$away_on_5 == i & test.A$away_on_5 != test.A$goalie & x > 0 | 
               test.A$away_on_6 == i & test.A$away_on_6 != test.A$goalie & x > 0, 1, 0
      )
  }
  # Away Defense
  for(i in mat_unique$player) {
    x <- mat_unique$qual[match(i, mat_unique$player)]
    
    test.A[paste(i, ".d", sep="")] <- 
      ifelse(test.A$home_on_1 == i & x > 0 | 
               test.A$home_on_2 == i & x > 0 | 
               test.A$home_on_3 == i & x > 0 | 
               test.A$home_on_4 == i & x > 0 | 
               test.A$home_on_5 == i & x > 0 | 
               test.A$home_on_6 == i & x > 0, 1, 0
      )
  }
  
  
  ## Combine Home / Away (O / D)
  ################################
  test_all <- test.H %>% rbind(., test.A) %>% arrange(n)
  test_sparse <- data.matrix(test_all[, -c(1:15)])
  test_sparse <- Matrix(test_sparse, sparse = TRUE)
  
  
  # The Model 
  ################################
  #GF60 <- test_sparse[, 3] 
  CF60 <- test_sparse[, 2] 
  length <- test_sparse[, 1] 
  test_sparse <- test_sparse[, -c(1:3)]
  
  lambda <- cv.glmnet(test_sparse, 
                      #GF60, 
                      CF60, 
                      weights = length, nfolds = folds)
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
    
    return(APM_all)
  }
  APM_all <- fun.APM_bind()
  
  return(APM_all)
  
}
APM_test_upd <- fun.APM(pbp_part, 10)






### Testing / Misc loaded locally

TOI_1617 <- games_5v5_1617 %>% ungroup() %>% group_by(player) %>% 
  summarise(TOI_5v5 = sum(TOI_5v5), 
            Team = first(Team)) %>% 
  rename(APM_names = player)

Team <- games_5v5_1617 %>% ungroup() %>% group_by(player) %>% 
  summarise(Team = first(Team)) %>% 
  rename(APM_names = player)

GA_GA_1617 <- games_5v5_1617 %>% ungroup() %>% group_by(player) %>% 
  summarise(TOI_5v5 = sum(TOI_5v5), 
            onGF_5v5 = sum(onGF_5v5),
            onGA_5v5 = sum(onGA_5v5)) %>% 
  mutate(G_diff = onGF_5v5 - onGA_5v5) %>% 
  rename(APM_names = player) %>% select(APM_names, G_diff)

APM_test <- left_join(APM_test, TOI_1617)
APM_qual <- APM_test %>% filter(TOI_5v5 > 300) %>% 
  mutate(O_impact = Off*(TOI_5v5/60), 
         D_impact = Def*(TOI_5v5/60), 
         All_impact = O_impact - D_impact) %>% 
  left_join(., GA_GA_1617)

APM_qual <- APM_qual %>% left_join(., Team)

APM_qual <- APM_qual %>% mutate_each(funs(round(., 2)), Off:TOI_5v5, O_impact:All_impact)


