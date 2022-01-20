
## If not installed the following packages remove the "#"
#install.packages(nflreadr)
#install.packages(tidyverse)
#install.pacakges(stringr)
#install.packages(rlang)

library(nflreadr)
library(tidyverse)
library(ffscrapr)
library(stringr)
library(rlang)
options(scipen = 9999)

## Set WD if wanting to analyse and compare different mock drafts.
setwd("C:\\Users\\zacro\\Documents\\Apprenticeship Work\\FFverse MDM\\v8")

fantasy_ids <- load_ff_playerids() 


fantasypros_rankings2021 <- load_ff_rankings() %>%
  filter(page_type == "redraft-overall") %>%
  filter(pos == "QB" | pos == "RB"| pos == "WR"| pos == "TE"| pos == "FB") %>%
  select(id,player,pos,team,ecr) %>%
  rename(fantasypros_id = id) %>%
  mutate(fantasypros_rank = rank(ecr)) %>%
  select(fantasypros_id,fantasypros_rank) 

pff_rankings2021 <- read_csv(url("https://raw.githubusercontent.com/rogers1000/LWP4/main/PFF_2021_rankings.csv")) %>%
  select(Id,expertConsensus,expertAndrewErickson,expertNathanJahnke,expertIanHartitz,expertJaradEvans,expertDwainMcFarland,expertKevinCole,expertBenBrown) %>%
  rename(PFF_Consensus = expertConsensus) %>%
  separate(Id, into = c("pff","player","pff_id", sep = "-")) %>%
  select(pff_id,PFF_Consensus,expertAndrewErickson,expertNathanJahnke,expertIanHartitz,expertJaradEvans,expertDwainMcFarland,expertKevinCole,expertBenBrown) 


#?separate

all_ranks <- fantasy_ids %>%
  filter(position == "QB" | position == "RB" | position == "WR" | position == "TE") %>%
  left_join(fantasypros_rankings2021, by = "fantasypros_id") %>%
  left_join(pff_rankings2021, by = "pff_id") %>%
  select(name,position,fantasypros_rank,PFF_Consensus,expertAndrewErickson,expertNathanJahnke,expertIanHartitz,expertJaradEvans,expertDwainMcFarland,expertKevinCole,expertBenBrown) %>%
  mutate(FFverse_Consensus = (fantasypros_rank+PFF_Consensus+expertAndrewErickson+expertNathanJahnke+expertIanHartitz+expertJaradEvans+expertDwainMcFarland+expertKevinCole+expertBenBrown)/8) %>%
  select(name,position,FFverse_Consensus,fantasypros_rank,PFF_Consensus,expertAndrewErickson,expertNathanJahnke,expertIanHartitz,expertJaradEvans,expertDwainMcFarland,expertKevinCole,expertBenBrown) %>%
  filter(!is.na(FFverse_Consensus)) %>%
  mutate(FFverse_OverallRank = rank(FFverse_Consensus)) %>%
  select(name,position,FFverse_OverallRank,FFverse_Consensus,fantasypros_rank,PFF_Consensus,expertAndrewErickson,expertNathanJahnke,expertIanHartitz,expertJaradEvans,expertDwainMcFarland,expertKevinCole,expertBenBrown) %>%
  arrange(FFverse_OverallRank) 


### Player Selection Formula

## Mock Draft
# Note: Keep Picks under 200 players as only 200 players are ranked.
Teams <- 12
Rounds <- 16
Picks <- Teams*Rounds

Team_Picks <- data.frame(Pick = c(1:Picks),Linear = c(1:Teams), Snake = c(1:Teams,Teams:1)) %>%
  #Using Snake Method
  select(Pick,Snake) %>%
  mutate(Snake = ifelse(Snake == 1, "A",Snake)) %>%
  mutate(Snake = ifelse(Snake == 2, "B",Snake)) %>%
  mutate(Snake = ifelse(Snake == 3, "C",Snake)) %>%
  mutate(Snake = ifelse(Snake == 4, "D",Snake)) %>%
  mutate(Snake = ifelse(Snake == 5, "E",Snake)) %>%
  mutate(Snake = ifelse(Snake == 6, "F",Snake)) %>%
  mutate(Snake = ifelse(Snake == 7, "G",Snake)) %>%
  mutate(Snake = ifelse(Snake == 8, "H",Snake)) %>%
  mutate(Snake = ifelse(Snake == 9, "I",Snake)) %>%
  mutate(Snake = ifelse(Snake == 10, "J",Snake)) %>%
  mutate(Snake = ifelse(Snake == 11, "K",Snake)) %>%
  mutate(Snake = ifelse(Snake == 12, "L",Snake)) %>%
  rename(Team = Snake)

Count <- 0

### How Many Drafts do you want to make at once?
## Default is 1
Simulations <- 10000

### Team Next Pick Default is League Size * 2
Team_Next_Picks <- Teams*2


repeat{
  ## Random Number Generator for randomised rankers. 
  ## Swap "sample(1:7,1)" for one of the rankers listed below if you want to keep the same rankers for multiple drafts.
  A_Ranker <- sample(1:7,1)
  B_Ranker <- sample(1:7,1)
  C_Ranker <- sample(1:7,1)
  D_Ranker <- sample(1:7,1)
  E_Ranker <- sample(1:7,1)
  F_Ranker <- sample(1:7,1)
  G_Ranker <- sample(1:7,1)
  H_Ranker <- sample(1:7,1)
  I_Ranker <- sample(1:7,1)
  J_Ranker <- sample(1:7,1)
  K_Ranker <- sample(1:7,1)
  L_Ranker <- sample(1:7,1)
  
  Team_Rankers <- data.frame(Team = c("A","B","C","D","E","F","G","H","I","J","K","L"), Ranker_Number = c(A_Ranker,B_Ranker,C_Ranker,D_Ranker,E_Ranker,F_Ranker,G_Ranker,H_Ranker,I_Ranker,J_Ranker,K_Ranker,L_Ranker)) %>%
    mutate(Ranker = ifelse(Ranker_Number == 1, "PFF_Consensus",Ranker_Number),
           Ranker = ifelse(Ranker_Number == 2, "expertAndrewErickson",Ranker),
           Ranker = ifelse(Ranker_Number == 3, "expertNathanJahnke",Ranker),
           Ranker = ifelse(Ranker_Number == 4, "expertIanHartitz",Ranker),
           Ranker = ifelse(Ranker_Number == 5, "expertJaradEvans",Ranker),
           Ranker = ifelse(Ranker_Number == 6, "expertDwainMcFarland",Ranker),
           Ranker = ifelse(Ranker_Number == 7, "expertKevinCole",Ranker),
           Ranker = ifelse(Ranker_Number == 8, "expertBenBrown",Ranker))
  
  A_Tactic <- sample(1:4,1)
  B_Tactic <- sample(1:4,1)
  C_Tactic <- sample(1:4,1)
  D_Tactic <- sample(1:4,1)
  E_Tactic <- sample(1:4,1)
  F_Tactic <- sample(1:4,1)
  G_Tactic <- sample(1:4,1)
  H_Tactic <- sample(1:4,1)
  I_Tactic <- sample(1:4,1)
  J_Tactic <- sample(1:4,1)
  K_Tactic <- sample(1:4,1)
  L_Tactic <- sample(1:4,1)
  
  Team_Tactics <- data.frame(Team = c("A","B","C","D","E","F","G","H","I","J","K","L"), Tactic_Number = c(A_Tactic,B_Tactic,C_Tactic,D_Tactic,E_Tactic,F_Tactic,G_Tactic,H_Tactic,I_Tactic,J_Tactic,K_Tactic,L_Tactic)) %>%
    mutate(Tactic = ifelse(Tactic_Number == 1, "WR0",Tactic_Number),
           Tactic = ifelse(Tactic_Number == 2, "RB1",Tactic),
           Tactic = ifelse(Tactic_Number == 3, "RB0",Tactic),
           Tactic = ifelse(Tactic_Number == 4, "BPA",Tactic))
  
  
  Roster_Rules <- data.frame(QB = 1, RB = 2, WR = 3, TE = 1, Flex = 1, SuperFlex = 0) %>%
    mutate(RB = RB + Flex + SuperFlex) %>%
    mutate(WR = WR + Flex + SuperFlex) %>%
    mutate(TE = TE + Flex + SuperFlex) %>%
    mutate(QB = QB + SuperFlex)
  
  RR_QBs <- dplyr::pull(Roster_Rules,QB)
  RR_RBs <- dplyr::pull(Roster_Rules,RB)
  RR_WRs <- dplyr::pull(Roster_Rules,WR)
  RR_TEs <- dplyr::pull(Roster_Rules,TE)
  
  Team_Next_Picks <- Teams*2
  Mock_Draft <- "None"
  player_position <- "None"
  percentage_lottery <- 0
  expected_player_name <- "None"
  expected_player_percent <- 0
  Picks_Drafted <- 0
  Mock_Table <- data.frame(Team = c("A","B","C","D","E","F","G","H","I","J","K","L"), Name = "None",Position = "None")
  

  
  
  repeat{  
  Team_Ranker <- Team_Picks %>%
  filter(Pick > Picks_Drafted) %>%
    mutate(Next_Pick = ifelse(Pick == Picks_Drafted + 1,1,0)) %>%
    group_by(Team) %>%
    summarise(On_the_clock = sum(Next_Pick)) %>%
    ungroup() %>%
    left_join(Team_Picks, by = "Team") %>%
    filter(On_the_clock == 1) %>%
    arrange(Pick) %>%
    head(1) %>%
    left_join(Team_Rankers, by = "Team")
  
  
  Ranker <- dplyr::pull(Team_Ranker,Ranker)
  
  On_clock_team <- Team_Picks %>%
    filter(Pick > Picks_Drafted) %>%
    mutate(Next_Pick = ifelse(Pick == Picks_Drafted + 1,1,0)) %>%
    group_by(Team) %>%
    summarise(On_the_clock = sum(Next_Pick)) %>%
    ungroup() %>%
    filter(On_the_clock == 1) 
  
  On_clock_team_vector <- dplyr::pull(On_clock_team,Team)
  
  TT_AfterR1 <- Mock_Table %>%
    filter(Team == On_clock_team_vector) %>%
    select(Team,Name,Position) %>%
    mutate(QBs = ifelse(Position == "QB",1,0),
           RBs = ifelse(Position == "RB",1,0),
           WRs = ifelse(Position == "WR",1,0),
           TEs = ifelse(Position == "TE",1,0)) %>%
    group_by(Team) %>%
    summarise(QBs = sum(QBs), RBs = sum(RBs), WRs = sum(WRs), TEs = sum(TEs)) %>%
    mutate(QB_Starting_Spot = RR_QBs - QBs) %>%
    mutate(RB_Starting_Spot = RR_RBs - RBs) %>%
    mutate(WR_Starting_Spot = RR_WRs - WRs) %>%
    mutate(TE_Starting_Spot = RR_TEs - TEs) %>%
    #select(Team,QB_Starting_Spot,RB_Starting_Spot,WR_Starting_Spot,TE_Starting_Spot) %>%
    left_join(Team_Tactics, by = "Team") %>%
    mutate(Position_Target_WR0_a = ifelse(Tactic == "WR0" & RB_Starting_Spot > 0, "RB","")) %>%
    mutate(Position_Target_WR0_b = ifelse(Tactic == "WR0" & RB_Starting_Spot < 1 & WR_Starting_Spot > 0, "TE_WR","")) %>%
    mutate(Position_Target_WR0_c = ifelse(Tactic == "WR0" & RB_Starting_Spot < 1 & WR_Starting_Spot < 1, "ADP","")) %>%
    mutate(Position_Target_RB0_a = ifelse(Tactic == "RB0" & WR_Starting_Spot > 0, "TE_WR","")) %>%
    mutate(Position_Target_RB0_b = ifelse(Tactic == "RB0" & WR_Starting_Spot < 1 & RB_Starting_Spot > 0, "RB","")) %>%
    mutate(Position_Target_RB0_c = ifelse(Tactic == "RB0" & WR_Starting_Spot < 1 & RB_Starting_Spot < 1, "ADP","")) %>%
    mutate(Position_Target_RB1_a = ifelse(Tactic == "RB1" & RBs == 0, "RB", "")) %>%
    mutate(Position_Target_RB1_b = ifelse(Tactic == "RB1" & RBs > 0, "TE_WR", "")) %>%
    mutate(Position_Target_RB1_c = ifelse(Tactic == "RB1" & RB_Starting_Spot > 0 & WR_Starting_Spot < 1, "RB", "")) %>%
    mutate(Position_Target_RB1_d = ifelse(Tactic == "RB1" & WR_Starting_Spot < 1 & RB_Starting_Spot < 1, "ADP","")) %>%
    mutate(PT1 = ifelse(Position_Target_WR0_a == "RB","RB",Position_Target_WR0_b)) %>%
    mutate(PT2 = ifelse(PT1 == "", Position_Target_WR0_c, PT1)) %>%
    mutate(PT3 = ifelse(PT2 == "", Position_Target_RB0_a, PT2)) %>%
    mutate(PT4 = ifelse(PT3 == "",Position_Target_RB0_b, PT3)) %>%
    mutate(PT5 = ifelse(PT4 == "",Position_Target_RB0_c, PT4)) %>%
    mutate(PT6 = ifelse(PT5 == "",Position_Target_RB1_a, PT5)) %>%
    mutate(PT7 = ifelse(PT6 == "",Position_Target_RB1_b, PT6)) %>%
    mutate(PT8 = ifelse(PT7 == "",Position_Target_RB1_c, PT7)) %>%
    mutate(PT9 = ifelse(PT8 == "",Position_Target_RB1_d, PT8)) %>%
    mutate(PT10 = ifelse(PT9 == "","ADP",PT9)) %>%
    mutate(Tactic_Target = paste0(PT10,"_Score")) 
    #select(Team,Tactic_Target) 
  
  Team_Target_afterR1 <- dplyr::pull(TT_AfterR1,Tactic_Target)
  
  TT_forR1 <- Team_Picks %>%
    filter(Pick > Picks_Drafted) %>%
    mutate(Next_Pick = ifelse(Pick == Picks_Drafted + 1,1,0)) %>%
    group_by(Team) %>%
    summarise(On_the_clock = sum(Next_Pick)) %>%
    ungroup() %>%
    filter(On_the_clock == 1) %>%
    left_join(Team_Tactics, by = "Team") %>%
    mutate(Team_Tactic = ifelse(Tactic == "RB0","TE_WR",""),
           Team_Tactic = ifelse(Tactic == "RB1", "RB",Team_Tactic),
           Team_Tactic = ifelse(Tactic == "WR0", "RB",Team_Tactic),
           Team_Tactic = ifelse(Tactic == "BPA", "ADP",Team_Tactic)) %>%
    mutate(Tactic_Target = paste0(Team_Tactic,"_Score")) 
  
  Team_Target_forR1 <- dplyr::pull(TT_forR1,Tactic_Target)
  
  Tactic_Vector <- ifelse(Picks_Drafted <= 12,Team_Target_forR1,Team_Target_afterR1)
  
  
    Player_Selection <- all_ranks %>%
      filter(!name %in% Mock_Draft) %>%
      mutate(fantasy_pos = ifelse(position == "WR" | position == "TE", "WR", position)) %>%
      group_by(fantasy_pos) %>%
      mutate(QB_Rank = ifelse(fantasy_pos == "QB",rank(.data[[Ranker]]),0)) %>%
      mutate(QB_Score = ifelse(QB_Rank == 1, 50,0),
             QB_Score = ifelse(QB_Rank == 2, 40,QB_Score),
             QB_Score = ifelse(QB_Rank == 3, 30,QB_Score),
             QB_Score = ifelse(QB_Rank == 4, 20,QB_Score),
             QB_Score = ifelse(QB_Rank == 5, 10,QB_Score),
             QB_Score = ifelse(QB_Rank == 6, 5,QB_Score),
             QB_Score = ifelse(QB_Rank == 7, 4,QB_Score),
             QB_Score = ifelse(QB_Rank == 8, 3,QB_Score),
             QB_Score = ifelse(QB_Rank == 9, 2,QB_Score),
             QB_Score = ifelse(QB_Rank == 10, 1,QB_Score)) %>%
      mutate(RB_Rank = ifelse(fantasy_pos == "RB",rank(.data[[Ranker]]),0)) %>%
      mutate(RB_Score = ifelse(RB_Rank == 1, 50,0),
             RB_Score = ifelse(RB_Rank == 2, 40,RB_Score),
             RB_Score = ifelse(RB_Rank == 3, 30,RB_Score),
             RB_Score = ifelse(RB_Rank == 4, 20,RB_Score),
             RB_Score = ifelse(RB_Rank == 5, 10,RB_Score),
             RB_Score = ifelse(RB_Rank == 6, 5,RB_Score),
             RB_Score = ifelse(RB_Rank == 7, 4,RB_Score),
             RB_Score = ifelse(RB_Rank == 8, 3,RB_Score),
             RB_Score = ifelse(RB_Rank == 9, 2,RB_Score),
             RB_Score = ifelse(RB_Rank == 10, 1,RB_Score)) %>%
      #mutate(WR_Rank = ifelse(fantasy_pos == "WR",rank(.data[[Ranker]]),0)) %>%
      #mutate(WR_Score = ifelse(WR_Rank == 1, 50,0),
      #       WR_Score = ifelse(WR_Rank == 2, 40,WR_Score),
      #       WR_Score = ifelse(WR_Rank == 3, 30,WR_Score),
      #       WR_Score = ifelse(WR_Rank == 4, 20,WR_Score),
      #       WR_Score = ifelse(WR_Rank == 5, 10,WR_Score),
      #       WR_Score = ifelse(WR_Rank == 6, 5,WR_Score),
      #       WR_Score = ifelse(WR_Rank == 7, 4,WR_Score),
      #       WR_Score = ifelse(WR_Rank == 8, 3,WR_Score),
      #       WR_Score = ifelse(WR_Rank == 9, 2,WR_Score),
      #       WR_Score = ifelse(WR_Rank == 10, 1,WR_Score)) %>%
      #mutate(TE_Rank = ifelse(fantasy_pos == "TE",rank(.data[[Ranker]]),0)) %>%
      #mutate(TE_Score = ifelse(TE_Rank == 1, 50,0),
      #       TE_Score = ifelse(TE_Rank == 2, 40,TE_Score),
      #       TE_Score = ifelse(TE_Rank == 3, 30,TE_Score),
      #       TE_Score = ifelse(TE_Rank == 4, 20,TE_Score),
      #       TE_Score = ifelse(TE_Rank == 5, 10,TE_Score),
      #       TE_Score = ifelse(TE_Rank == 6, 5,TE_Score),
      #       TE_Score = ifelse(TE_Rank == 7, 4,TE_Score),
      #       TE_Score = ifelse(TE_Rank == 8, 3,TE_Score),
      #       TE_Score = ifelse(TE_Rank == 9, 2,TE_Score),
      #       TE_Score = ifelse(TE_Rank == 10, 1,TE_Score)) %>%
    mutate(TE_WR_Rank = ifelse(fantasy_pos == "WR",rank(.data[[Ranker]]),0)) %>%
      mutate(TE_WR_Score = ifelse(TE_WR_Rank == 1, 50,0),
             TE_WR_Score = ifelse(TE_WR_Rank == 2, 40,TE_WR_Score),
             TE_WR_Score = ifelse(TE_WR_Rank == 3, 30,TE_WR_Score),
             TE_WR_Score = ifelse(TE_WR_Rank == 4, 20,TE_WR_Score),
             TE_WR_Score = ifelse(TE_WR_Rank == 5, 10,TE_WR_Score),
             TE_WR_Score = ifelse(TE_WR_Rank == 6, 5,TE_WR_Score),
             TE_WR_Score = ifelse(TE_WR_Rank == 7, 4,TE_WR_Score),
             TE_WR_Score = ifelse(TE_WR_Rank == 8, 3,TE_WR_Score),
             TE_WR_Score = ifelse(TE_WR_Rank == 9, 2,TE_WR_Score),
             TE_WR_Score = ifelse(TE_WR_Rank == 10, 1,TE_WR_Score)) %>%
      ungroup() %>%
      mutate(ADP_Rank = rank(FFverse_OverallRank)) %>%
      mutate(ADP_Score = ifelse(ADP_Rank == 1, 50,0),
             ADP_Score = ifelse(ADP_Rank == 2, 40,ADP_Score),
             ADP_Score = ifelse(ADP_Rank == 3, 30,ADP_Score),
             ADP_Score = ifelse(ADP_Rank == 4, 20,ADP_Score),
             ADP_Score = ifelse(ADP_Rank == 5, 10,ADP_Score),
             ADP_Score = ifelse(ADP_Rank == 6, 5,ADP_Score),
             ADP_Score = ifelse(ADP_Rank == 7, 4,ADP_Score),
             ADP_Score = ifelse(ADP_Rank == 8, 3,ADP_Score),
             ADP_Score = ifelse(ADP_Rank == 9, 2,ADP_Score),
             ADP_Score = ifelse(ADP_Rank == 10, 1,ADP_Score)) %>%
      ### SELECT FUNCTION NEEDS UPDATING WHENEVER A NEW RANKER IS ADDED
      select(ADP_Rank,name,position,FFverse_OverallRank,PFF_Consensus,expertAndrewErickson,expertNathanJahnke,expertIanHartitz,expertJaradEvans,expertDwainMcFarland,expertKevinCole,expertBenBrown,ADP_Score,QB_Score,RB_Score,TE_WR_Score) %>%
      #Removed TE_Score and WR_Score from formula
      mutate(Total_Score = ADP_Score+QB_Score+RB_Score+TE_WR_Score+.data[[Tactic_Vector]]) %>%
      mutate(Rank_Weighting_min = min(.data[[Ranker]])) %>%
      mutate(Rank_Weighting1 = (Rank_Weighting_min)/(.data[[Ranker]] + Rank_Weighting_min)) %>%
      mutate(Rank_Weighting2 = (Total_Score + Rank_Weighting1)*Rank_Weighting1) %>%
      mutate(Rank_Weighting3 = ifelse(Rank_Weighting2 <0, 0, Rank_Weighting2)) %>%
      group_by(name,position,FFverse_OverallRank) %>%
      mutate(Ranker1 = ifelse(PFF_Consensus < Team_Next_Picks,1,0)) %>%
      mutate(Ranker2 = ifelse(expertAndrewErickson < Team_Next_Picks,1,0)) %>%
      mutate(Ranker3 = ifelse(expertNathanJahnke < Team_Next_Picks,1,0)) %>%
      mutate(Ranker4 = ifelse(expertIanHartitz < Team_Next_Picks,1,0)) %>%
      mutate(Ranker5 = ifelse(expertJaradEvans < Team_Next_Picks,1,0)) %>%
      mutate(Ranker6 = ifelse(expertDwainMcFarland < Team_Next_Picks,1,0)) %>%
      mutate(Ranker7 = ifelse(expertBenBrown < Team_Next_Picks,1,0)) %>%
      mutate(Ranker8 = ifelse(expertKevinCole < Team_Next_Picks,1,0)) %>%
      mutate(Survival_percent = sum(Ranker1,Ranker2,Ranker3,Ranker4,Ranker5,Ranker6,Ranker7,Ranker8)) %>%
      mutate(Survival_percent = Survival_percent/8) %>%
      mutate(Survival_percent = ifelse(Survival_percent < 0.1,0.1,Survival_percent)) %>%
      ungroup() %>%
      mutate(ADP_Weighting = Rank_Weighting3*Survival_percent) %>%
      mutate(ADP_Weighting2 = sum(ADP_Weighting)) %>%
      mutate(ADP_Weighting3 = 100/ADP_Weighting2*ADP_Weighting) %>%
      mutate(PlayerSectionLottery = ADP_Weighting3) %>%
      select(name,position,FFverse_OverallRank,.data[[Ranker]],PlayerSectionLottery) %>%
      arrange(-PlayerSectionLottery,.data[[Ranker]],FFverse_OverallRank) %>%
      mutate(player_selection_lottery = runif(1, 0.00, 100.00)) %>%
      mutate(row_number_formula = 1:n()) %>%
      mutate(lottery_max = cumsum(c(PlayerSectionLottery))) %>%
      mutate(lottery_min = lottery_max - PlayerSectionLottery) %>%
      mutate(Drafted_Player = ifelse(player_selection_lottery > lottery_min & player_selection_lottery <= lottery_max,1,0)) %>%
      #filter(Drafted_Player == 1) %>%
      select(name,position,FFverse_OverallRank,.data[[Ranker]],PlayerSectionLottery,Drafted_Player) %>%
      arrange(-Drafted_Player,-PlayerSectionLottery,.data[[Ranker]],FFverse_OverallRank) 
    
    
    player_drafted <- dplyr::pull(Player_Selection %>% filter(Drafted_Player == 1),name)
    drafted_player_position <- dplyr::pull(Player_Selection %>% filter(Drafted_Player == 1),position)
    Lottery_percent <- dplyr::pull(Player_Selection %>% filter(Drafted_Player == 1),PlayerSectionLottery)
    expected_pick_name <- dplyr::pull(Player_Selection %>% filter(PlayerSectionLottery == max(PlayerSectionLottery)),name)
    expected_pick_percent <- dplyr::pull(Player_Selection %>% filter(PlayerSectionLottery == max(PlayerSectionLottery)),PlayerSectionLottery)
    
    Mock_Draft <- c(Mock_Draft,player_drafted) %>%
      unique()
    
    player_position <- c(player_position,drafted_player_position)
    
    percentage_lottery <- c(percentage_lottery,Lottery_percent)
    
    expected_player_name <- c(expected_player_name,expected_pick_name)
    
    expected_player_percent <- c(expected_player_percent,expected_pick_percent)
    
    
    Mock_Table <- data.frame(Name = c(Mock_Draft), Lottery = c(percentage_lottery), EP_Name = c(expected_player_name), EP_Percent = c(expected_player_percent), Position = c(player_position)) %>%
      filter(Name != "None") %>%
      mutate(Pick = 1:n()) %>%
      select(Pick,Name,Position,Lottery,EP_Name,EP_Percent) %>%
      left_join(Team_Picks, by = "Pick") %>%
      select(Pick,Team,Name,Position,Lottery,EP_Name,EP_Percent) %>%
      mutate(Round = ifelse(Pick%%Teams == 0, Pick/Teams,(Pick/Teams)+1)) %>%
      mutate(Round = floor(Round)) %>%
      select(Round,Pick,Team,Name,Position,Lottery,EP_Name,EP_Percent)
    
    Picks_Drafted <- max(dplyr::pull(Mock_Table,Pick)) 
    
    Team_Next_Pick <- Team_Picks %>%
      filter(Pick > Picks_Drafted) %>%
      mutate(Next_Pick = ifelse(Pick == Picks_Drafted + 1,1,0)) %>%
      group_by(Team) %>%
      summarise(On_the_clock = sum(Next_Pick)) %>%
      ungroup() %>%
      left_join(Team_Picks, by = "Team") %>%
      filter(On_the_clock == 1) %>%
      group_by(Team) %>%
      mutate(filter_column = ifelse(Pick > Picks_Drafted + 1,1,0)) %>%
      mutate(filter_column = ifelse(On_the_clock == 1, filter_column + 1, filter_column)) %>%
      mutate(pick_order = rank(Pick)) %>%
      mutate(filter_column = ifelse(pick_order == 3,filter_column + 1,filter_column)) %>%
      ungroup() %>%
      arrange(-filter_column,Team,Pick) %>%
      head(1)
    
    Team_Next_Picks <- dplyr::pull(Team_Next_Pick,Pick)
    
    if (Picks_Drafted == Picks) {
      break
    }
  }
  
  
  Count <- Count + 1
  
  Savefile <- paste0("Draft",Count)
  
  MDM_Combined <- Mock_Table %>%
    select(Name,Position,Pick)
  
  write_csv(MDM_Combined,paste0(Savefile,".csv"))
  
  if (Count == Simulations) {
    break
  }
}
#


combined_mocks <- map_dfr(paste0("Draft",1:Count,".csv"),readr::read_csv) 


combined_mocks_analysis <- combined_mocks %>%
  left_join(all_ranks, by = c("Name" = "name")) %>%
  group_by(Name,Position,FFverse_OverallRank) %>%
  summarise(mean_pick = mean(Pick),
            median_pick = quantile(Pick,0.5),
            quantile90 = quantile(Pick, 0.1),
            quantile75 = quantile(Pick, 0.25),
            quantile25 = quantile(Pick, 0.75),
            quantile10 = quantile(Pick, 0.9),
            drafted_in = n()) %>%
  arrange(mean_pick,median_pick,quantile90,quantile75,quantile25,quantile10,FFverse_OverallRank) %>%
  view()
