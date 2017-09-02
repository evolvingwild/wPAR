#################################################################################
#####     Rel Shot metrics / models    ||             09/02/17              #####
#################################################################################

setwd("~/RStudio/Hockey")
require(dplyr); require(ggplot2); require(googlesheets); require(RSQLite);
require(ggthemes); require(ggpmisc); require(Kmisc); 

options(scipen=999)



# LOAD
scoreadj <- read.csv("ScoreAdj.csv")
fun.position <- function() {
  player_position <- read.csv("player_position.csv", stringsAsFactors = FALSE)
  pos_upd <- data.frame(matrix(nrow = 2, ncol = 2)); names(pos_upd) <- c("player", "position")
  pos_upd[1, 1] <- c("FRÉDÉRIC.ST-DENIS"); pos_upd[1, 2] <- 1
  pos_upd[2, 1] <- c("MICHAËL.BOURNIVAL"); pos_upd[2, 2] <- 1
  player_position <- player_position %>% rbind(., pos_upd) %>% arrange(player)
  return(player_position)
}
player_position <- fun.position()

## Objects - UPDATED event_types to match HTM names throughout (including functions below)
st.shot_events <- c("SHOT", "GOAL")
st.fenwick_events <- c("SHOT", "GOAL", "MISS")
st.corsi_events <- c("SHOT", "GOAL", "MISS", "BLOCK")
st.strength_states <- c("3v3", "5v5", "4v4", "5v4", "4v5", 
                        "5v3", "3v5", "4v3", "3v4", "5vE", 
                        "Ev5", "4vE", "Ev4", "3vE", "Ev3") %>% 
  as.factor()





##    Relative_TM Corsi / xG: filter pbp to 5v5 and add score adj columns
####################################################################################

fun.QoT_H <- function(data) {
  hold <- data %>% filter(game_period < 5) %>%  
    summarise(TOI = sum(event_length)/60,
              GF = sum(ifelse(event_team == home_team & event_type == "GOAL", 1, 0)), 
              GA = sum(ifelse(event_team == away_team & event_type == "GOAL", 1, 0)),
              CF = sum(ifelse(event_type %in% st.corsi_events & event_team == home_team, (1*scoreadj[home_lead, 2]), 0)), 
              CA = sum(ifelse(event_type %in% st.corsi_events & event_team == away_team, (1*scoreadj[home_lead, 3]), 0)),
              xGF = sum(na.omit(ifelse(event_type %in% st.fenwick_events & event_team == home_team, (1*prob_goal)*(scoreadj[home_lead, 2]), 0))),
              xGA = sum(na.omit(ifelse(event_type %in% st.fenwick_events & event_team == away_team, (1*prob_goal)*(scoreadj[home_lead, 3]), 0))))
  return(hold)
}
fun.QoT_A <- function(data) {
  hold <- data %>% filter(game_period < 5) %>%  
    summarise(TOI = sum(event_length)/60,
              GF = sum(ifelse(event_team == away_team & event_type == "GOAL", 1, 0)), 
              GA = sum(ifelse(event_team == home_team & event_type == "GOAL", 1, 0)),
              CF = sum(ifelse(event_type %in% st.corsi_events & event_team == away_team, (1*scoreadj[home_lead, 3]), 0)), 
              CA = sum(ifelse(event_type %in% st.corsi_events & event_team == home_team, (1*scoreadj[home_lead, 2]), 0)),
              xGF = sum(na.omit(ifelse(event_type %in% st.fenwick_events & event_team == away_team, (1*prob_goal)*(scoreadj[home_lead, 3]), 0))),
              xGA = sum(na.omit(ifelse(event_type %in% st.fenwick_events & event_team == home_team, (1*prob_goal)*(scoreadj[home_lead, 2]), 0))),
              Team = first(away_team))
  return(hold)
}
fun.event_playerH <- function(data, player) {
  if(player == "home_on_1") {
    #home_on_1
    h1 <- data %>% 
      group_by(game_id, home_on_1, home_on_2, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_2)
    h2 <- data %>% 
      group_by(game_id, home_on_1, home_on_3, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_3)
    h3 <- data %>% 
      group_by(game_id, home_on_1, home_on_4, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_4)
    h4 <- data %>% 
      group_by(game_id, home_on_1, home_on_5, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_5)
    h5 <- data %>% 
      group_by(game_id, home_on_1, home_on_6, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_6)
    
    hbind <- rbind(h1, h2, h3, h4, h5)
    hbind <- hbind %>% group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI:xGA), sum) %>% 
      filter(TOI > 0)
    return(hbind)
    
  }
  else if(player == "home_on_2") {
    #home_on_2
    h1 <- data %>% 
      group_by(game_id, home_on_2, home_on_1, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_1)
    h2 <- data %>% 
      group_by(game_id, home_on_2, home_on_3, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_3)
    h3 <- data %>% 
      group_by(game_id, home_on_2, home_on_4, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_4)
    h4 <- data %>% 
      group_by(game_id, home_on_2, home_on_5, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_5)
    h5 <- data %>% 
      group_by(game_id, home_on_2, home_on_6, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_6)
    
    hbind <- rbind(h1, h2, h3, h4, h5)
    hbind <- hbind %>% group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI:xGA), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
  else if(player == "home_on_3") {
    #home_on_3
    h1 <- data %>% 
      group_by(game_id, home_on_3, home_on_1, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_1)
    h2 <- data %>% 
      group_by(game_id, home_on_3, home_on_2, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_2)
    h3 <- data %>% 
      group_by(game_id, home_on_3, home_on_4, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_4)
    h4 <- data %>% 
      group_by(game_id, home_on_3, home_on_5, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_5)
    h5 <- data %>% 
      group_by(game_id, home_on_3, home_on_6, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_6)
    
    hbind <- rbind(h1, h2, h3, h4, h5)
    hbind <- hbind %>% group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI:xGA), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
  else if(player == "home_on_4") {
    #home_on_4
    h1 <- data %>% 
      group_by(game_id, home_on_4, home_on_1, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_1)
    h2 <- data %>% 
      group_by(game_id, home_on_4, home_on_2, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_2)
    h3 <- data %>% 
      group_by(game_id, home_on_4, home_on_3, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_3)
    h4 <- data %>% 
      group_by(game_id, home_on_4, home_on_5, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_5)
    h5 <- data %>% 
      group_by(game_id, home_on_4, home_on_6, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_6)
    
    hbind <- rbind(h1, h2, h3, h4, h5)
    hbind <- hbind %>% group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI:xGA), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
  else if(player == "home_on_5") {
    #home_on_5
    h1 <- data %>% 
      group_by(game_id, home_on_5, home_on_1, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_1)
    h2 <- data %>% 
      group_by(game_id, home_on_5, home_on_2, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_2)
    h3 <- data %>% 
      group_by(game_id, home_on_5, home_on_3, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_3)
    h4 <- data %>% 
      group_by(game_id, home_on_5, home_on_4, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_4)
    h5 <- data %>% 
      group_by(game_id, home_on_5, home_on_6, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_6)
    
    hbind <- rbind(h1, h2, h3, h4, h5)
    hbind <- hbind %>% group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI:xGA), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
  else if(player == "home_on_6") {
    #home_on_6
    h1 <- data %>% 
      group_by(game_id, home_on_6, home_on_1, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_1)
    h2 <- data %>% 
      group_by(game_id, home_on_6, home_on_2, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_2)
    h3 <- data %>% 
      group_by(game_id, home_on_6, home_on_3, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_3)
    h4 <- data %>% 
      group_by(game_id, home_on_6, home_on_4, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_4)
    h5 <- data %>% 
      group_by(game_id, home_on_6, home_on_5, home_team) %>% fun.QoT_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_5)
    
    hbind <- rbind(h1, h2, h3, h4, h5)
    hbind <- hbind %>% group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI:xGA), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
}
fun.event_playerA <- function(data, player) {
  if(player == "away_on_1") {
    #away_on_1
    a1 <- data %>% 
      group_by(game_id, away_on_1, away_on_2, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_2)
    a2 <- data %>% 
      group_by(game_id, away_on_1, away_on_3, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_3)
    a3 <- data %>% 
      group_by(game_id, away_on_1, away_on_4, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_4)
    a4 <- data %>% 
      group_by(game_id, away_on_1, away_on_5, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_5)
    a5 <- data %>% 
      group_by(game_id, away_on_1, away_on_6, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_6)
    
    hbind <- rbind(a1, a2, a3, a4, a5)
    hbind <- hbind %>% group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI:xGA), sum) %>% 
      filter(TOI > 0)
    return(hbind)
    
  }
  else if(player == "away_on_2") {
    #away_on_2
    a1 <- data %>% 
      group_by(game_id, away_on_2, away_on_1, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_1)
    a2 <- data %>% 
      group_by(game_id, away_on_2, away_on_3, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_3)
    a3 <- data %>% 
      group_by(game_id, away_on_2, away_on_4, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_4)
    a4 <- data %>% 
      group_by(game_id, away_on_2, away_on_5, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_5)
    a5 <- data %>% 
      group_by(game_id, away_on_2, away_on_6, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_6)
    
    hbind <- rbind(a1, a2, a3, a4, a5)
    hbind <- hbind %>% group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI:xGA), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
  else if(player == "away_on_3") {
    #away_on_3
    a1 <- data %>% 
      group_by(game_id, away_on_3, away_on_1, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_1)
    a2 <- data %>% 
      group_by(game_id, away_on_3, away_on_2, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_2)
    a3 <- data %>% 
      group_by(game_id, away_on_3, away_on_4, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_4)
    a4 <- data %>% 
      group_by(game_id, away_on_3, away_on_5, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_5)
    a5 <- data %>% 
      group_by(game_id, away_on_3, away_on_6, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_6)
    
    hbind <- rbind(a1, a2, a3, a4, a5)
    hbind <- hbind %>% group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI:xGA), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
  else if(player == "away_on_4") {
    #away_on_4
    a1 <- data %>% 
      group_by(game_id, away_on_4, away_on_1, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_1)
    a2 <- data %>% 
      group_by(game_id, away_on_4, away_on_2, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_2)
    a3 <- data %>% 
      group_by(game_id, away_on_4, away_on_3, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_3)
    a4 <- data %>% 
      group_by(game_id, away_on_4, away_on_5, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_5)
    a5 <- data %>% 
      group_by(game_id, away_on_4, away_on_6, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_6)
    
    hbind <- rbind(a1, a2, a3, a4, a5)
    hbind <- hbind %>% group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI:xGA), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
  else if(player == "away_on_5") {
    #away_on_5
    a1 <- data %>% 
      group_by(game_id, away_on_5, away_on_1, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_1)
    a2 <- data %>% 
      group_by(game_id, away_on_5, away_on_2, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_2)
    a3 <- data %>% 
      group_by(game_id, away_on_5, away_on_3, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_3)
    a4 <- data %>% 
      group_by(game_id, away_on_5, away_on_4, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_4)
    a5 <- data %>% 
      group_by(game_id, away_on_5, away_on_6, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_6)
    
    hbind <- rbind(a1, a2, a3, a4, a5)
    hbind <- hbind %>% group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI:xGA), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
  else if(player == "away_on_6") {
    #away_on_6
    a1 <- data %>% 
      group_by(game_id, away_on_6, away_on_1, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_1)
    a2 <- data %>% 
      group_by(game_id, away_on_6, away_on_2, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_2)
    a3 <- data %>% 
      group_by(game_id, away_on_6, away_on_3, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_3)
    a4 <- data %>% 
      group_by(game_id, away_on_6, away_on_4, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_4)
    a5 <- data %>% 
      group_by(game_id, away_on_6, away_on_5, away_team) %>% fun.QoT_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_5)
    
    hbind <- rbind(a1, a2, a3, a4, a5)
    hbind <- hbind %>% group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI:xGA), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
}
fun.goalie_remove <- function(data) {
  goalie_h <- data.frame(unique(data$home_goalie))
  names(goalie_h) <- c("goalie")
  goalie_a <- data.frame(unique(data$away_goalie))
  names(goalie_a) <- c("goalie")
  
  goalie_all <- rbind(goalie_h, goalie_a)
  goalie_all <- data.frame(unique(goalie_all$goalie))
  names(goalie_all) <- c("player")
  
  goalie_all <- goalie_all %>% filter(!is.na(player)) %>% mutate(is_goalie = 1)
  goalie_all$player <- as.character(goalie_all$player)
  return(goalie_all)
}


# Combine 
fun.teammate <- function(data) {
  
  home1 <- fun.event_playerH(data, "home_on_1")
  home2 <- fun.event_playerH(data, "home_on_2")
  home3 <- fun.event_playerH(data, "home_on_3")
  home4 <- fun.event_playerH(data, "home_on_4")
  home5 <- fun.event_playerH(data, "home_on_5")
  home6 <- fun.event_playerH(data, "home_on_6")
  
  away1 <- fun.event_playerA(data, "away_on_1")
  away2 <- fun.event_playerA(data, "away_on_2")
  away3 <- fun.event_playerA(data, "away_on_3")
  away4 <- fun.event_playerA(data, "away_on_4")
  away5 <- fun.event_playerA(data, "away_on_5")
  away6 <- fun.event_playerA(data, "away_on_6")
  
  # HOME
  home_all <- rbind(home1, home2, home3, home4, home5, home6)
  home_all <- home_all %>% ungroup() %>% group_by(player, teammate, game_id, home_team) %>% 
    summarise_at(vars(TOI:xGA), sum) %>% filter(!is.na(player), !is.na(teammate)) %>% rename(Team = home_team)
  # Remove Goalie
  goalieremove <- fun.goalie_remove(data)
  testH <- home_all[!(home_all$player %in% goalieremove$player),]
  testH <- testH[!(testH$teammate %in% goalieremove$player),]
  testH$is_home <- 1
  
  # AWAY
  away_all <- rbind(away1, away2, away3, away4, away5, away6)
  away_all <- away_all %>% ungroup() %>% group_by(player, teammate, game_id, away_team) %>% 
    summarise_at(vars(TOI:xGA), sum) %>% filter(!is.na(player), !is.na(teammate)) %>% rename(Team = away_team) 
  # Remove Goalies
  goalieremove <- fun.goalie_remove(data)
  testA <- away_all[!(away_all$player %in% goalieremove$player),]
  testA <- testA[!(testA$teammate %in% goalieremove$player),]
  testA$is_home <- 0
  
  # Combine
  ALL <- rbind(testH, testA)
  ALL <- ALL %>% ungroup() %>% 
    mutate(season = 20162017) %>% group_by(player, teammate, game_id, Team) %>% 
    mutate_each(funs(round(., 3)), TOI, xGF, xGA) %>% 
    arrange(player, teammate, game_id)
  return(ALL)
}


# 5v5 Relative_TM Corsi / xG: Impact Per Season - update pending...
fun.reltotal <- function(TM_data, games_data, year) {
  # Prep
  TM_data <- TM_data %>% group_by(player, teammate) %>% 
    filter(!is.na(CF)) %>%  mutate(G_T = n())
  
  prep_TM <- TM_data %>% group_by(player, teammate, Team) %>% 
    summarise(TOI_TM = sum(TOI),
              CF_TM = sum(CF), 
              CA_TM = sum(CA),
              xGF_TM = sum(xGF),
              xGA_TM = sum(xGA)) %>% data.frame()
  
  # Sum for season 
  ## This is the same games1617 dataframe from above
  ## Only every player's 5v5 on-ice CF, CA, xGF, xGA and TOI are needed/used.
  onice_totals <- games_data %>% group_by(player, season, Team) %>% filter(!is.na(onCF)) %>% 
    summarise_at(vars(TOI:GA, onGF_5v5:offxGA), sum) %>% 
    mutate(FOdiff = FOW - FOL) %>% 
    mutate_each(funs(round(.,2)), TOI, TOI_5v5, ixG, onCF:t_TOI_5v5, t_CF:t_xGA, offCF:offxGA)
  
  # Setting up and adding in totals for Teammate compare/rel
  rel_player <- onice_totals %>% select(player, season, Team, TOI_5v5, onCF, onCA, onxGF, onxGA) %>% 
    mutate(CF60 = round(((onCF/TOI_5v5)*60),2), 
           CA60 = round(((onCA/TOI_5v5)*60),2),
           xGF60 = round(((onxGF/TOI_5v5)*60),2),
           xGA60 = round(((onxGA/TOI_5v5)*60),2))
  
  relteammate <- rel_player %>% rename(teammate = player)
  
  relmergeP <- left_join(prep_TM, rel_player, by = c("player", "Team"))
  relmergeT <- left_join(prep_TM, relteammate, by = c("teammate", "Team"))
  rel_All <- left_join(relmergeP, relmergeT, by = c("player", "teammate", "Team", "CF_TM", "CA_TM", "xGF_TM", "xGA_TM"))
  
  
  # Final Rel numbers
  rel_TM_metrics <- rel_All %>% 
    mutate(TM_TOI_w.o = TOI_5v5.y - TOI_TM.x,
           
           TM_CF_w.o = onCF.y - CF_TM,
           TM_CA_w.o = onCA.y - CA_TM,
           TM_xGF_w.o = onxGF.y - xGF_TM,
           TM_xGA_w.o = onxGA.y - xGA_TM,
           
           TM_CF_60_w.o = ifelse(TM_TOI_w.o > 0, round(((TM_CF_w.o/TM_TOI_w.o)*60),2), 0), 
           TM_CA_60_w.0 = ifelse(TM_TOI_w.o > 0, round(((TM_CA_w.o/TM_TOI_w.o)*60),2), 0), 
           TM_xGF_60_w.o = ifelse(TM_TOI_w.o > 0, round(((TM_xGF_w.o/TM_TOI_w.o)*60),2), 0), 
           TM_xGA_60_w.0 = ifelse(TM_TOI_w.o > 0, round(((TM_xGA_w.o/TM_TOI_w.o)*60),2), 0), 
           
           player_TOI_perc_w = round(TOI_TM.x / TOI_5v5.x,4),
           
           weighted_TM_CF60 = ifelse(TM_TOI_w.o > 0, TM_CF_60_w.o*player_TOI_perc_w, 0), 
           weighted_TM_CA60 = ifelse(TM_TOI_w.o > 0, TM_CA_60_w.0*player_TOI_perc_w, 0), 
           weighted_TM_xGF60 = ifelse(TM_TOI_w.o > 0, TM_xGF_60_w.o*player_TOI_perc_w, 0), 
           weighted_TM_xGA60 = ifelse(TM_TOI_w.o > 0, TM_xGA_60_w.0*player_TOI_perc_w, 0))
  
  rel_TM_impact <- rel_TM_metrics %>% ungroup() %>% group_by(player, Team) %>%
    summarise(TOI_5v5 = first(TOI_5v5.x),
              CF60 = first(CF60.x),
              CA60 = first(CA60.x),
              xGF60 = first(xGF60.x),
              xGA60 = first(xGA60.x),
              
              perc_total = sum(player_TOI_perc_w),
              
              weighted_TM_CF_raw = sum(weighted_TM_CF60),
              weighted_TM_CA_raw = sum(weighted_TM_CA60),
              weighted_TM_xGF_raw = sum(weighted_TM_xGF60),
              weighted_TM_xGA_raw = sum(weighted_TM_xGA60)) %>% 
    
    mutate(w_TM_CF60 = round(weighted_TM_CF_raw / perc_total,2),
           w_TM_CA60 = round(weighted_TM_CA_raw / perc_total,2),
           w_TM_xGF60 = round(weighted_TM_xGF_raw / perc_total,2),
           w_TM_xGA60 = round(weighted_TM_xGA_raw / perc_total,2),
           
           rel_CF60_TM = round(CF60 - w_TM_CF60,2),
           rel_CA60_TM = round(CA60 - w_TM_CA60,2),
           rel_xGF60_TM = round(xGF60 - w_TM_xGF60,2),
           rel_xGA60_TM = round(xGA60 - w_TM_xGA60,2),
           
           CF_impact = round(rel_CF60_TM * (TOI_5v5/60),2),
           CA_impact = round(rel_CA60_TM * (TOI_5v5/60),2),
           xGF_impact = round(rel_xGF60_TM * (TOI_5v5/60),2),
           xGA_impact = round(rel_xGA60_TM * (TOI_5v5/60),2),
           
           rel_Cdiff60_TM = rel_CF60_TM - rel_CA60_TM,
           rel_xGdiff60_TM = round(rel_xGF60_TM - rel_xGA60_TM,2),
           Corsi_total_impact = CF_impact - CA_impact,
           xG_total_impact = xGF_impact - xGA_impact,
           season = year) %>% 
    
    select(player, Team, season, TOI_5v5, 
           CF60, CA60, xGF60, xGA60, 
           w_TM_CF60, w_TM_CA60, w_TM_xGF60, w_TM_xGA60,
           rel_CF60_TM, rel_CA60_TM, 
           rel_xGF60_TM, rel_xGA60_TM, 
           rel_Cdiff60_TM, rel_xGdiff60_TM,
           CF_impact, CA_impact, xGF_impact, xGA_impact,
           Corsi_total_impact, xG_total_impact) %>% data.frame()
  
  return(rel_TM_impact)
}




##    QoT_TOI: Time On Ice only              
####################################################################################

fun.QoT_TOI_H <- function(data) {
  hold <- data %>% filter(game_period < 5) %>%  
    summarise(TOI = sum(event_length)/60)
  return(hold)
}
fun.QoT_TOI_A <- function(data) {
  hold <- data %>% filter(game_period < 5) %>%  
    summarise(TOI = sum(event_length)/60)
  return(hold)
}
fun.event_player_TOI_H <- function(data, player) {
  if(player == "home_on_1") {
    #home_on_1
    h1 <- data %>% 
      group_by(game_id, home_on_1, home_on_2, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_2)
    h2 <- data %>% 
      group_by(game_id, home_on_1, home_on_3, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_3)
    h3 <- data %>% 
      group_by(game_id, home_on_1, home_on_4, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_4)
    h4 <- data %>% 
      group_by(game_id, home_on_1, home_on_5, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_5)
    h5 <- data %>% 
      group_by(game_id, home_on_1, home_on_6, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_1, 
             teammate = home_on_6)
    
    hbind <- rbind(h1, h2, h3, h4, h5)
    hbind <- hbind %>% group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI), sum) %>% 
      filter(TOI > 0)
    return(hbind)
    
  }
  else if(player == "home_on_2") {
    #home_on_2
    h1 <- data %>% 
      group_by(game_id, home_on_2, home_on_1, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_1)
    h2 <- data %>% 
      group_by(game_id, home_on_2, home_on_3, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_3)
    h3 <- data %>% 
      group_by(game_id, home_on_2, home_on_4, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_4)
    h4 <- data %>% 
      group_by(game_id, home_on_2, home_on_5, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_5)
    h5 <- data %>% 
      group_by(game_id, home_on_2, home_on_6, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_2, 
             teammate = home_on_6)
    
    hbind <- rbind(h1, h2, h3, h4, h5)
    hbind <- hbind %>% group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
  else if(player == "home_on_3") {
    #home_on_3
    h1 <- data %>% 
      group_by(game_id, home_on_3, home_on_1, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_1)
    h2 <- data %>% 
      group_by(game_id, home_on_3, home_on_2, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_2)
    h3 <- data %>% 
      group_by(game_id, home_on_3, home_on_4, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_4)
    h4 <- data %>% 
      group_by(game_id, home_on_3, home_on_5, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_5)
    h5 <- data %>% 
      group_by(game_id, home_on_3, home_on_6, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_3, 
             teammate = home_on_6)
    
    hbind <- rbind(h1, h2, h3, h4, h5)
    hbind <- hbind %>% group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
  else if(player == "home_on_4") {
    #home_on_4
    h1 <- data %>% 
      group_by(game_id, home_on_4, home_on_1, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_1)
    h2 <- data %>% 
      group_by(game_id, home_on_4, home_on_2, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_2)
    h3 <- data %>% 
      group_by(game_id, home_on_4, home_on_3, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_3)
    h4 <- data %>% 
      group_by(game_id, home_on_4, home_on_5, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_5)
    h5 <- data %>% 
      group_by(game_id, home_on_4, home_on_6, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_4, 
             teammate = home_on_6)
    
    hbind <- rbind(h1, h2, h3, h4, h5)
    hbind <- hbind %>% group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
  else if(player == "home_on_5") {
    #home_on_5
    h1 <- data %>% 
      group_by(game_id, home_on_5, home_on_1, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_1)
    h2 <- data %>% 
      group_by(game_id, home_on_5, home_on_2, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_2)
    h3 <- data %>% 
      group_by(game_id, home_on_5, home_on_3, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_3)
    h4 <- data %>% 
      group_by(game_id, home_on_5, home_on_4, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_4)
    h5 <- data %>% 
      group_by(game_id, home_on_5, home_on_6, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_5, 
             teammate = home_on_6)
    
    hbind <- rbind(h1, h2, h3, h4, h5)
    hbind <- hbind %>% group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
  else if(player == "home_on_6") {
    #home_on_6
    h1 <- data %>% 
      group_by(game_id, home_on_6, home_on_1, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_1)
    h2 <- data %>% 
      group_by(game_id, home_on_6, home_on_2, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_2)
    h3 <- data %>% 
      group_by(game_id, home_on_6, home_on_3, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_3)
    h4 <- data %>% 
      group_by(game_id, home_on_6, home_on_4, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_4)
    h5 <- data %>% 
      group_by(game_id, home_on_6, home_on_5, home_team) %>% fun.QoT_TOI_H(.) %>% 
      rename(player = home_on_6, 
             teammate = home_on_5)
    
    hbind <- rbind(h1, h2, h3, h4, h5)
    hbind <- hbind %>% group_by(player, teammate, game_id, home_team) %>% 
      summarise_at(vars(TOI), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
}
fun.event_player_TOI_A <- function(data, player) {
  if(player == "away_on_1") {
    #away_on_1
    a1 <- data %>% 
      group_by(game_id, away_on_1, away_on_2, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_2)
    a2 <- data %>% 
      group_by(game_id, away_on_1, away_on_3, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_3)
    a3 <- data %>% 
      group_by(game_id, away_on_1, away_on_4, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_4)
    a4 <- data %>% 
      group_by(game_id, away_on_1, away_on_5, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_5)
    a5 <- data %>% 
      group_by(game_id, away_on_1, away_on_6, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_1, 
             teammate = away_on_6)
    
    hbind <- rbind(a1, a2, a3, a4, a5)
    hbind <- hbind %>% group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI), sum) %>% 
      filter(TOI > 0)
    return(hbind)
    
  }
  else if(player == "away_on_2") {
    #away_on_2
    a1 <- data %>% 
      group_by(game_id, away_on_2, away_on_1, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_1)
    a2 <- data %>% 
      group_by(game_id, away_on_2, away_on_3, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_3)
    a3 <- data %>% 
      group_by(game_id, away_on_2, away_on_4, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_4)
    a4 <- data %>% 
      group_by(game_id, away_on_2, away_on_5, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_5)
    a5 <- data %>% 
      group_by(game_id, away_on_2, away_on_6, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_2, 
             teammate = away_on_6)
    
    hbind <- rbind(a1, a2, a3, a4, a5)
    hbind <- hbind %>% group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
  else if(player == "away_on_3") {
    #away_on_3
    a1 <- data %>% 
      group_by(game_id, away_on_3, away_on_1, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_1)
    a2 <- data %>% 
      group_by(game_id, away_on_3, away_on_2, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_2)
    a3 <- data %>% 
      group_by(game_id, away_on_3, away_on_4, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_4)
    a4 <- data %>% 
      group_by(game_id, away_on_3, away_on_5, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_5)
    a5 <- data %>% 
      group_by(game_id, away_on_3, away_on_6, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_3, 
             teammate = away_on_6)
    
    hbind <- rbind(a1, a2, a3, a4, a5)
    hbind <- hbind %>% group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
  else if(player == "away_on_4") {
    #away_on_4
    a1 <- data %>% 
      group_by(game_id, away_on_4, away_on_1, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_1)
    a2 <- data %>% 
      group_by(game_id, away_on_4, away_on_2, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_2)
    a3 <- data %>% 
      group_by(game_id, away_on_4, away_on_3, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_3)
    a4 <- data %>% 
      group_by(game_id, away_on_4, away_on_5, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_5)
    a5 <- data %>% 
      group_by(game_id, away_on_4, away_on_6, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_4, 
             teammate = away_on_6)
    
    hbind <- rbind(a1, a2, a3, a4, a5)
    hbind <- hbind %>% group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
  else if(player == "away_on_5") {
    #away_on_5
    a1 <- data %>% 
      group_by(game_id, away_on_5, away_on_1, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_1)
    a2 <- data %>% 
      group_by(game_id, away_on_5, away_on_2, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_2)
    a3 <- data %>% 
      group_by(game_id, away_on_5, away_on_3, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_3)
    a4 <- data %>% 
      group_by(game_id, away_on_5, away_on_4, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_4)
    a5 <- data %>% 
      group_by(game_id, away_on_5, away_on_6, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_5, 
             teammate = away_on_6)
    
    hbind <- rbind(a1, a2, a3, a4, a5)
    hbind <- hbind %>% group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
  else if(player == "away_on_6") {
    #away_on_6
    a1 <- data %>% 
      group_by(game_id, away_on_6, away_on_1, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_1)
    a2 <- data %>% 
      group_by(game_id, away_on_6, away_on_2, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_2)
    a3 <- data %>% 
      group_by(game_id, away_on_6, away_on_3, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_3)
    a4 <- data %>% 
      group_by(game_id, away_on_6, away_on_4, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_4)
    a5 <- data %>% 
      group_by(game_id, away_on_6, away_on_5, away_team) %>% fun.QoT_TOI_A(.) %>% 
      rename(player = away_on_6, 
             teammate = away_on_5)
    
    hbind <- rbind(a1, a2, a3, a4, a5)
    hbind <- hbind %>% group_by(player, teammate, game_id, away_team) %>% 
      summarise_at(vars(TOI), sum) %>%
      filter(TOI > 0)
    return(hbind)
  }
}
fun.goalie_remove <- function(data) {
  goalie_h <- data.frame(unique(data$home_goalie))
  names(goalie_h) <- c("goalie")
  goalie_a <- data.frame(unique(data$away_goalie))
  names(goalie_a) <- c("goalie")
  
  goalie_all <- rbind(goalie_h, goalie_a)
  goalie_all <- data.frame(unique(goalie_all$goalie))
  names(goalie_all) <- c("player")
  
  goalie_all <- goalie_all %>% filter(!is.na(player)) %>% mutate(is_goalie = 1)
  goalie_all$player <- as.character(goalie_all$player)
  return(goalie_all)
}

# Combine 
fun.teammate_TOI <- function(data, year) {
  
  home1 <- fun.event_player_TOI_H(data, "home_on_1")
  home2 <- fun.event_player_TOI_H(data, "home_on_2")
  home3 <- fun.event_player_TOI_H(data, "home_on_3")
  home4 <- fun.event_player_TOI_H(data, "home_on_4")
  home5 <- fun.event_player_TOI_H(data, "home_on_5")
  home6 <- fun.event_player_TOI_H(data, "home_on_6")
  
  away1 <- fun.event_player_TOI_A(data, "away_on_1")
  away2 <- fun.event_player_TOI_A(data, "away_on_2")
  away3 <- fun.event_player_TOI_A(data, "away_on_3")
  away4 <- fun.event_player_TOI_A(data, "away_on_4")
  away5 <- fun.event_player_TOI_A(data, "away_on_5")
  away6 <- fun.event_player_TOI_A(data, "away_on_6")
  
  # HOME
  home_all <- rbind(home1, home2, home3, home4, home5, home6)
  home_all <- home_all %>% ungroup() %>% group_by(player, teammate, game_id, home_team) %>% 
    summarise_at(vars(TOI), sum) %>% filter(!is.na(player), !is.na(teammate)) %>% rename(Team = home_team)
  # Remove Goalie
  goalieremove <- fun.goalie_remove(data)
  testH <- home_all[!(home_all$player %in% goalieremove$player),]
  testH <- testH[!(testH$teammate %in% goalieremove$player),]
  testH$is_home <- 1
  
  # AWAY
  away_all <- rbind(away1, away2, away3, away4, away5, away6)
  away_all <- away_all %>% ungroup() %>% group_by(player, teammate, game_id, away_team) %>% 
    summarise_at(vars(TOI), sum) %>% filter(!is.na(player), !is.na(teammate)) %>% rename(Team = away_team) 
  # Remove Goalies
  goalieremove <- fun.goalie_remove(data)
  testA <- away_all[!(away_all$player %in% goalieremove$player),]
  testA <- testA[!(testA$teammate %in% goalieremove$player),]
  testA$is_home <- 0
  
  # Combine
  ALL <- rbind(testH, testA)
  ALL <- ALL %>% ungroup() %>% 
    mutate(season = year) %>% group_by(player, teammate, game_id, Team) %>% 
    mutate_each(funs(round(., 3)), TOI) %>% 
    arrange(player, teammate, game_id)
  return(ALL)
}







