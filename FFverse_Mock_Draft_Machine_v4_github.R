

library(nflreadr)
library(tidyverse)
library(ffscrapr)
library(stringr)
library(rlang)
options(scipen = 9999)

## Set WD if wanting to analyse and compare different mock drafts.
#setwd("")

fantasy_ids <- load_ff_playerids() %>%
  view()

fantasypros_rankings2021 <- load_ff_rankings() %>%
  filter(page_type == "redraft-overall") %>%
  filter(pos == "QB" | pos == "RB"| pos == "WR"| pos == "TE"| pos == "FB") %>%
  select(id,player,pos,team,ecr) %>%
  rename(fantasypros_id = id) %>%
  mutate(fantasypros_rank = rank(ecr)) %>%
  select(fantasypros_id,fantasypros_rank) %>%
  view()

pff_rankings2021 <- read_csv(url("https://raw.githubusercontent.com/rogers1000/LWP4/main/PFF_2021_rankings.csv")) %>%
  select(Id,expertConsensus,expertAndrewErickson,expertNathanJahnke,expertIanHartitz,expertJaradEvans,expertDwainMcFarland,expertKevinCole,expertBenBrown) %>%
  rename(PFF_Consensus = expertConsensus) %>%
  separate(Id, into = c("pff","player","pff_id", sep = "-")) %>%
  select(pff_id,PFF_Consensus,expertAndrewErickson,expertNathanJahnke,expertIanHartitz,expertJaradEvans,expertDwainMcFarland,expertKevinCole,expertBenBrown) %>%
  view()

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
  arrange(FFverse_OverallRank) %>%
  view()

write_csv(all_ranks,"all_ranks.csv")


ranking_spreadsheet <- all_ranks %>%
  select(name,position,FFverse_OverallRank,fantasypros_rank,PFF_Consensus) %>%
  view()

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

Draft_Count <- 0

### How Many Drafts do you want to make at once?
## Default is 1
Draft_Simulations <- 1

### Team Next Pick Default is League Size * 2
Team_Next_Picks <- Teams*2

repeat{
  Team_Next_Picks <- Teams*2
  Mock_Draft <- "None"
  player_position <- "None"
  percentage_lottery <- 0
  expected_player_name <- "None"
  expected_player_percent <- 0
  
  
  repeat
    {
    Player_Selection <- all_ranks %>%
      #select(name,position,FFverse_OverallRank,PFF_Consensus) %>%
      filter(!name %in% Mock_Draft) %>%
      ### Team is using PFF_Consensus as their Rankings
      group_by(position) %>%
      mutate(QB_Rank = ifelse(position == "QB",rank(PFF_Consensus),0)) %>%
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
      mutate(RB_Rank = ifelse(position == "RB",rank(PFF_Consensus),0)) %>%
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
      mutate(WR_Rank = ifelse(position == "WR",rank(PFF_Consensus),0)) %>%
      mutate(WR_Score = ifelse(WR_Rank == 1, 50,0),
             WR_Score = ifelse(WR_Rank == 2, 40,WR_Score),
             WR_Score = ifelse(WR_Rank == 3, 30,WR_Score),
             WR_Score = ifelse(WR_Rank == 4, 20,WR_Score),
             WR_Score = ifelse(WR_Rank == 5, 10,WR_Score),
             WR_Score = ifelse(WR_Rank == 6, 5,WR_Score),
             WR_Score = ifelse(WR_Rank == 7, 4,WR_Score),
             WR_Score = ifelse(WR_Rank == 8, 3,WR_Score),
             WR_Score = ifelse(WR_Rank == 9, 2,WR_Score),
             WR_Score = ifelse(WR_Rank == 10, 1,WR_Score)) %>%
      mutate(TE_Rank = ifelse(position == "TE",rank(PFF_Consensus),0)) %>%
      mutate(TE_Score = ifelse(TE_Rank == 1, 50,0),
             TE_Score = ifelse(TE_Rank == 2, 40,TE_Score),
             TE_Score = ifelse(TE_Rank == 3, 30,TE_Score),
             TE_Score = ifelse(TE_Rank == 4, 20,TE_Score),
             TE_Score = ifelse(TE_Rank == 5, 10,TE_Score),
             TE_Score = ifelse(TE_Rank == 6, 5,TE_Score),
             TE_Score = ifelse(TE_Rank == 7, 4,TE_Score),
             TE_Score = ifelse(TE_Rank == 8, 3,TE_Score),
             TE_Score = ifelse(TE_Rank == 9, 2,TE_Score),
             TE_Score = ifelse(TE_Rank == 10, 1,TE_Score)) %>%
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
      select(ADP_Rank,name,position,FFverse_OverallRank,PFF_Consensus,PFF_Consensus,expertNathanJahnke,expertIanHartitz,expertJaradEvans,expertDwainMcFarland,expertKevinCole,expertBenBrown,ADP_Score,QB_Score,RB_Score,WR_Score,TE_Score) %>%
      mutate(Team_Strategy = ADP_Score) %>%
      mutate(Total_Score = ADP_Score+QB_Score+RB_Score+WR_Score+TE_Score+Team_Strategy) %>%
      mutate(Rank_Weighting_min = min(PFF_Consensus)) %>%
      mutate(Rank_Weighting1 = (Rank_Weighting_min)/(PFF_Consensus + Rank_Weighting_min)) %>%
      mutate(Rank_Weighting2 = (Total_Score + Rank_Weighting1)*Rank_Weighting1) %>%
      mutate(Rank_Weighting3 = ifelse(Rank_Weighting2 <0, 0, Rank_Weighting2)) %>%
      group_by(name,position,FFverse_OverallRank,PFF_Consensus) %>%
      #mutate(Ranker1 = ifelse(fantasypros_rank < Team_Next_Picks,1,0)) %>%
      mutate(Ranker2 = ifelse(PFF_Consensus < Team_Next_Picks,1,0)) %>%
      mutate(Ranker3 = ifelse(expertNathanJahnke < Team_Next_Picks,1,0)) %>%
      mutate(Ranker4 = ifelse(expertIanHartitz < Team_Next_Picks,1,0)) %>%
      mutate(Ranker5 = ifelse(expertJaradEvans < Team_Next_Picks,1,0)) %>%
      mutate(Ranker6 = ifelse(expertDwainMcFarland < Team_Next_Picks,1,0)) %>%
      mutate(Ranker7 = ifelse(expertKevinCole < Team_Next_Picks,1,0)) %>%
      mutate(Ranker8 = ifelse(expertBenBrown < Team_Next_Picks,1,0)) %>%
      mutate(Survival_percent = sum(Ranker2,Ranker3,Ranker4,Ranker5,Ranker6,Ranker7,Ranker8)) %>%
      mutate(Survival_percent = Survival_percent/7) %>%
      mutate(Survival_percent = ifelse(Survival_percent < 0.1,0.1,Survival_percent)) %>%
        ungroup() %>%
      mutate(ADP_Weighting = Rank_Weighting3*Survival_percent) %>%
      mutate(ADP_Weighting2 = sum(ADP_Weighting)) %>%
      mutate(ADP_Weighting3 = 100/ADP_Weighting2*ADP_Weighting) %>%
      mutate(PlayerSectionLottery = ADP_Weighting3) %>%
      select(name,position,FFverse_OverallRank,PFF_Consensus,PlayerSectionLottery) %>%
      arrange(-PlayerSectionLottery,PFF_Consensus,FFverse_OverallRank) %>%
      mutate(player_selection_lottery = runif(1, 0.00, 100.00)) %>%
      mutate(row_number_formula = 1:n()) %>%
      mutate(lottery_max = cumsum(c(PlayerSectionLottery))) %>%
      mutate(lottery_min = lottery_max - PlayerSectionLottery) %>%
      mutate(Drafted_Player = ifelse(player_selection_lottery > lottery_min & player_selection_lottery <= lottery_max,1,0)) %>%
      #filter(Drafted_Player == 1) %>%
      select(name,position,FFverse_OverallRank,PFF_Consensus,PlayerSectionLottery,Drafted_Player) %>%
      arrange(-Drafted_Player,-PlayerSectionLottery,PFF_Consensus,FFverse_OverallRank) 
      
    
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
    
    
    Mock_Draft_Table <- data.frame(Name = c(Mock_Draft), Lottery = c(percentage_lottery), EP_Name = c(expected_player_name), EP_Percent = c(expected_player_percent), Position = c(player_position)) %>%
      filter(Name != "None") %>%
      mutate(Pick = 1:n()) %>%
      select(Pick,Name,Position,Lottery,EP_Name,EP_Percent) %>%
      left_join(Team_Picks, by = "Pick") %>%
      select(Pick,Team,Name,Position,Lottery,EP_Name,EP_Percent) %>%
      mutate(Round = ifelse(Pick%%Teams == 0, Pick/Teams,(Pick/Teams)+1)) %>%
      mutate(Round = floor(Round)) %>%
      select(Round,Pick,Team,Name,Position,Lottery,EP_Name,EP_Percent) 
    
    Picks_Drafted <- max(dplyr::pull(Mock_Draft_Table,Pick))
    
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


Draft_Count <- Draft_Count + 1

Draft_Savefile <- paste0("Draft",Draft_Count)

MDM_Combined <- Mock_Draft_Table %>%
  select(Name,Position,Pick)
  #rename(!!Draft_Savefile := Pick) %>%

write_csv(MDM_Combined,paste0(Draft_Savefile,".csv"))

if (Draft_Count == Draft_Simulations) {
  break
}
}
#


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
  view()

Team_Next_Picks <- dplyr::pull(Team_Next_Pick,Pick)


#####
Draft01 <- read_csv("Draft1.csv")
Draft02 <- read_csv("Draft2.csv")
Draft03 <- read_csv("Draft3.csv")
Draft04 <- read_csv("Draft4.csv")
Draft05 <- read_csv("Draft5.csv")
Draft06 <- read_csv("Draft6.csv")
Draft07 <- read_csv("Draft7.csv")
Draft08 <- read_csv("Draft8.csv")
Draft09 <- read_csv("Draft9.csv")
Draft10 <- read_csv("Draft10.csv")
Draft11 <- read_csv("Draft11.csv")
Draft12 <- read_csv("Draft12.csv")
Draft13 <- read_csv("Draft13.csv")
Draft14 <- read_csv("Draft14.csv")
Draft15 <- read_csv("Draft15.csv")
Draft16 <- read_csv("Draft16.csv")
Draft17 <- read_csv("Draft17.csv")
Draft18 <- read_csv("Draft18.csv")
Draft19 <- read_csv("Draft19.csv")
Draft20 <- read_csv("Draft20.csv")
Draft21 <- read_csv("Draft21.csv")
Draft22 <- read_csv("Draft22.csv")
Draft23 <- read_csv("Draft23.csv")
Draft24 <- read_csv("Draft24.csv")
Draft25 <- read_csv("Draft25.csv")
Draft26 <- read_csv("Draft26.csv")
Draft27 <- read_csv("Draft27.csv")
Draft28 <- read_csv("Draft28.csv")
Draft29 <- read_csv("Draft29.csv")
Draft30 <- read_csv("Draft30.csv")
Draft31 <- read_csv("Draft31.csv")
Draft32 <- read_csv("Draft32.csv")
Draft33 <- read_csv("Draft33.csv")
Draft34 <- read_csv("Draft34.csv")
Draft35 <- read_csv("Draft35.csv")
Draft36 <- read_csv("Draft36.csv")
Draft37 <- read_csv("Draft37.csv")
Draft38 <- read_csv("Draft38.csv")
Draft39 <- read_csv("Draft39.csv")
Draft40 <- read_csv("Draft40.csv")
Draft41 <- read_csv("Draft41.csv")
Draft42 <- read_csv("Draft42.csv")
Draft43 <- read_csv("Draft43.csv")
Draft44 <- read_csv("Draft44.csv")
Draft45 <- read_csv("Draft45.csv")
Draft46 <- read_csv("Draft46.csv")
Draft47 <- read_csv("Draft47.csv")
Draft48 <- read_csv("Draft48.csv")
Draft49 <- read_csv("Draft49.csv")
Draft50 <- read_csv("Draft50.csv")
Draft51 <- read_csv("Draft51.csv")
Draft52 <- read_csv("Draft52.csv")
Draft53 <- read_csv("Draft53.csv")
Draft54 <- read_csv("Draft54.csv")
Draft55 <- read_csv("Draft55.csv")
Draft56 <- read_csv("Draft56.csv")
Draft57 <- read_csv("Draft57.csv")
Draft58 <- read_csv("Draft58.csv")
Draft59 <- read_csv("Draft59.csv")
Draft60 <- read_csv("Draft60.csv")
Draft61 <- read_csv("Draft61.csv")
Draft62 <- read_csv("Draft62.csv")
Draft63 <- read_csv("Draft63.csv")
Draft64 <- read_csv("Draft64.csv")
Draft65 <- read_csv("Draft65.csv")
Draft66 <- read_csv("Draft66.csv")
Draft67 <- read_csv("Draft67.csv")
Draft68 <- read_csv("Draft68.csv")
Draft69 <- read_csv("Draft69.csv")
Draft70 <- read_csv("Draft70.csv")
Draft71 <- read_csv("Draft71.csv")
Draft72 <- read_csv("Draft72.csv")
Draft73 <- read_csv("Draft73.csv")
Draft74 <- read_csv("Draft74.csv")
Draft75 <- read_csv("Draft75.csv")
Draft76 <- read_csv("Draft76.csv")
Draft77 <- read_csv("Draft77.csv")
Draft78 <- read_csv("Draft78.csv")
Draft79 <- read_csv("Draft79.csv")
Draft80 <- read_csv("Draft80.csv")
Draft81 <- read_csv("Draft81.csv")
Draft82 <- read_csv("Draft82.csv")
Draft83 <- read_csv("Draft83.csv")
Draft84 <- read_csv("Draft84.csv")
Draft85 <- read_csv("Draft85.csv")
Draft86 <- read_csv("Draft86.csv")
Draft87 <- read_csv("Draft87.csv")
Draft88 <- read_csv("Draft88.csv")
Draft89 <- read_csv("Draft89.csv")
Draft90 <- read_csv("Draft90.csv")
Draft91 <- read_csv("Draft91.csv")
Draft92 <- read_csv("Draft92.csv")
Draft93 <- read_csv("Draft93.csv")
Draft94 <- read_csv("Draft94.csv")
Draft95 <- read_csv("Draft95.csv")
Draft96 <- read_csv("Draft96.csv")
Draft97 <- read_csv("Draft97.csv")
Draft98 <- read_csv("Draft98.csv")
Draft99 <- read_csv("Draft99.csv")
Draft100 <- read_csv("Draft100.csv")
Draft101 <- read_csv("Draft101.csv")
Draft102 <- read_csv("Draft102.csv")
Draft103 <- read_csv("Draft103.csv")
Draft104 <- read_csv("Draft104.csv")
Draft105 <- read_csv("Draft105.csv")
Draft106 <- read_csv("Draft106.csv")
Draft107 <- read_csv("Draft107.csv")
Draft108 <- read_csv("Draft108.csv")
Draft109 <- read_csv("Draft109.csv")
Draft110 <- read_csv("Draft110.csv")
Draft111 <- read_csv("Draft111.csv")
Draft112 <- read_csv("Draft112.csv")
Draft113 <- read_csv("Draft113.csv")
Draft114 <- read_csv("Draft114.csv")
Draft115 <- read_csv("Draft115.csv")
Draft116 <- read_csv("Draft116.csv")
Draft117 <- read_csv("Draft117.csv")
Draft118 <- read_csv("Draft118.csv")
Draft119 <- read_csv("Draft119.csv")
Draft120 <- read_csv("Draft120.csv")
Draft121 <- read_csv("Draft121.csv")
Draft122 <- read_csv("Draft122.csv")
Draft123 <- read_csv("Draft123.csv")
Draft124 <- read_csv("Draft124.csv")
Draft125 <- read_csv("Draft125.csv")
Draft126 <- read_csv("Draft126.csv")
Draft127 <- read_csv("Draft127.csv")
Draft128 <- read_csv("Draft128.csv")
Draft129 <- read_csv("Draft129.csv")
Draft130 <- read_csv("Draft130.csv")
Draft131 <- read_csv("Draft131.csv")
Draft132 <- read_csv("Draft132.csv")
Draft133 <- read_csv("Draft133.csv")
Draft134 <- read_csv("Draft134.csv")
Draft135 <- read_csv("Draft135.csv")
Draft136 <- read_csv("Draft136.csv")
Draft137 <- read_csv("Draft137.csv")
Draft138 <- read_csv("Draft138.csv")
Draft139 <- read_csv("Draft139.csv")
Draft140 <- read_csv("Draft140.csv")
Draft141 <- read_csv("Draft141.csv")
Draft142 <- read_csv("Draft142.csv")
Draft143 <- read_csv("Draft143.csv")
Draft144 <- read_csv("Draft144.csv")
Draft145 <- read_csv("Draft145.csv")
Draft146 <- read_csv("Draft146.csv")
Draft147 <- read_csv("Draft147.csv")
Draft148 <- read_csv("Draft148.csv")
Draft149 <- read_csv("Draft149.csv")
Draft150 <- read_csv("Draft150.csv")
Draft151 <- read_csv("Draft151.csv")
Draft152 <- read_csv("Draft152.csv")
Draft153 <- read_csv("Draft153.csv")
Draft154 <- read_csv("Draft154.csv")
Draft155 <- read_csv("Draft155.csv")
Draft156 <- read_csv("Draft156.csv")
Draft157 <- read_csv("Draft157.csv")
Draft158 <- read_csv("Draft158.csv")
Draft159 <- read_csv("Draft159.csv")
Draft160 <- read_csv("Draft160.csv")
Draft161 <- read_csv("Draft161.csv")
Draft162 <- read_csv("Draft162.csv")
Draft163 <- read_csv("Draft163.csv")
Draft164 <- read_csv("Draft164.csv")
Draft165 <- read_csv("Draft165.csv")
Draft166 <- read_csv("Draft166.csv")
Draft167 <- read_csv("Draft167.csv")
Draft168 <- read_csv("Draft168.csv")
Draft169 <- read_csv("Draft169.csv")
Draft170 <- read_csv("Draft170.csv")
Draft171 <- read_csv("Draft171.csv")
Draft172 <- read_csv("Draft172.csv")
Draft173 <- read_csv("Draft173.csv")
Draft174 <- read_csv("Draft174.csv")
Draft175 <- read_csv("Draft175.csv")
Draft176 <- read_csv("Draft176.csv")
Draft177 <- read_csv("Draft177.csv")
Draft178 <- read_csv("Draft178.csv")
Draft179 <- read_csv("Draft179.csv")
Draft180 <- read_csv("Draft180.csv")
Draft181 <- read_csv("Draft181.csv")
Draft182 <- read_csv("Draft182.csv")
Draft183 <- read_csv("Draft183.csv")
Draft184 <- read_csv("Draft184.csv")
Draft185 <- read_csv("Draft185.csv")
Draft186 <- read_csv("Draft186.csv")
Draft187 <- read_csv("Draft187.csv")
Draft188 <- read_csv("Draft188.csv")
Draft189 <- read_csv("Draft189.csv")
Draft190 <- read_csv("Draft190.csv")
Draft191 <- read_csv("Draft191.csv")
Draft192 <- read_csv("Draft192.csv")
Draft193 <- read_csv("Draft193.csv")
Draft194 <- read_csv("Draft194.csv")
Draft195 <- read_csv("Draft195.csv")
Draft196 <- read_csv("Draft196.csv")
Draft197 <- read_csv("Draft197.csv")
Draft198 <- read_csv("Draft198.csv")
Draft199 <- read_csv("Draft199.csv")
Draft200 <- read_csv("Draft200.csv")

analysis_Draft0 <- rbind(Draft01,Draft02,Draft03,Draft04,Draft05,Draft06,Draft07,Draft08,Draft09)
analysis_Draft1 <- rbind(analysis_Draft0,Draft11,Draft12,Draft13,Draft14,Draft15,Draft16,Draft17,Draft18,Draft19)
analysis_Draft2 <- rbind(analysis_Draft1,Draft21,Draft22,Draft23,Draft24,Draft25,Draft26,Draft27,Draft28,Draft29)
analysis_Draft3 <- rbind(analysis_Draft2,Draft31,Draft32,Draft33,Draft34,Draft35,Draft36,Draft37,Draft38,Draft39)
analysis_Draft4 <- rbind(analysis_Draft3,Draft41,Draft42,Draft43,Draft44,Draft45,Draft46,Draft47,Draft48,Draft49)
analysis_Draft5 <- rbind(analysis_Draft4,Draft51,Draft52,Draft53,Draft54,Draft55,Draft56,Draft57,Draft58,Draft59)
analysis_Draft6 <- rbind(analysis_Draft5,Draft61,Draft62,Draft63,Draft64,Draft65,Draft66,Draft67,Draft68,Draft69)
analysis_Draft7 <- rbind(analysis_Draft6,Draft71,Draft72,Draft73,Draft74,Draft75,Draft76,Draft77,Draft78,Draft79)
analysis_Draft8 <- rbind(analysis_Draft7,Draft81,Draft82,Draft83,Draft84,Draft85,Draft86,Draft87,Draft88,Draft89)
analysis_Draft9 <- rbind(analysis_Draft8,Draft91,Draft92,Draft93,Draft94,Draft95,Draft96,Draft97,Draft98,Draft99)
analysis_Draft10 <- rbind(analysis_Draft9,Draft10,Draft20,Draft30,Draft40,Draft50,Draft60,Draft70,Draft80,Draft90,Draft100)
analysis_Draft11 <- rbind(analysis_Draft10,Draft101,Draft102,Draft103,Draft104,Draft105,Draft106,Draft107,Draft108,Draft109)
analysis_Draft12 <- rbind(analysis_Draft11,Draft111,Draft112,Draft113,Draft114,Draft115,Draft116,Draft117,Draft118,Draft119)
analysis_Draft13 <- rbind(analysis_Draft12,Draft121,Draft122,Draft123,Draft124,Draft125,Draft126,Draft127,Draft128,Draft129)
analysis_Draft14 <- rbind(analysis_Draft13,Draft131,Draft132,Draft133,Draft134,Draft135,Draft136,Draft137,Draft138,Draft139)
analysis_Draft15 <- rbind(analysis_Draft14,Draft141,Draft142,Draft143,Draft144,Draft145,Draft146,Draft147,Draft148,Draft149)
analysis_Draft16 <- rbind(analysis_Draft15,Draft151,Draft152,Draft153,Draft154,Draft155,Draft156,Draft157,Draft158,Draft159)
analysis_Draft17 <- rbind(analysis_Draft16,Draft161,Draft162,Draft163,Draft164,Draft165,Draft166,Draft167,Draft168,Draft169)
analysis_Draft18 <- rbind(analysis_Draft17,Draft171,Draft172,Draft173,Draft174,Draft175,Draft176,Draft177,Draft178,Draft179)
analysis_Draft19 <- rbind(analysis_Draft18,Draft181,Draft182,Draft183,Draft184,Draft185,Draft186,Draft187,Draft188,Draft189)
analysis_Draft20 <- rbind(analysis_Draft19,Draft191,Draft192,Draft193,Draft194,Draft195,Draft196,Draft197,Draft198,Draft199)
analysis_Draft21 <- rbind(analysis_Draft20,Draft110,Draft120,Draft130,Draft140,Draft150,Draft160,Draft170,Draft180,Draft190,Draft200)


draft_analysis <- analysis_Draft21 %>%
  left_join(ranking_spreadsheet, by = c("Name" = "name")) %>%
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
  
