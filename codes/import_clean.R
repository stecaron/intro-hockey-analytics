# Title: import_clean_functions
# Date: December 2017
# Goal: Import and clean the data 


# Load packages -----------------------------------------------------------

library(data.table)
library(tidyverse)

# Importer les données ----------------------------------------------------

data_raw <- fread("data/raw/All Sits-Table 1.csv", skip = 2)


# Select variables --------------------------------------------------------

variables_selected <- c("First Name", "Last Name", "Position", "Team", "GP", "G", "A", "PTS", "TOI/GP", "G.Bkhd", "G.Dflct", "G.Slap", "G.Snap", "G.Tip", "G.Wrap", "G.Wrst", "GA", "S.Bkhd", "S.Dflct", "S.Slap", "S.Snap", "S.Tip", "S.Wrap", "S.Wrst", "+/-", "iBLK", "iHF", "A1", "A2", "Wide", "1G", "GWG", "iGVA", "iTKA", "PIM")
data_selected <- data_raw[, (variables_selected), with = F]


# Clean the data  -----------------------------------------------

# The first position in the column seems to be the "main" position of the player
data_selected[, c("main_position") := tstrsplit(Position, "/", fixed = TRUE, keep = 1L)][]

# The first team in the column seems to be the "last" team for which the player was playing
data_selected[, c("last_team") := tstrsplit(Team, "/", fixed = TRUE, keep = 1L)][]

# Rename variables --------------------------------------------------------

data_renamed <- data_selected %>% 
                  rename(
                    All.Position = Position,
                    Position = main_position,
                    Last_Name = `Last Name`,
                    First_Name = `First Name`,
                    All.Team = Team,
                    Team = last_team,
                    TOI_GP = `TOI/GP`,
                    Goal.Backend = G.Bkhd,
                    Gaol.Deflected = G.Dflct,
                    Goal.SlapShot = G.Slap,
                    Goal.SnapShot = G.Snap,
                    Goal.Tip = G.Tip,
                    Goal.WrapAround = G.Wrap,
                    Goal.WristShot = G.Wrst,
                    Goal.Against = GA,
                    Goal.First = `1G`,
                    Game.Winning.Goal = GWG,
                    Shot.Backend = S.Bkhd,
                    Shot.Deflected = S.Dflct,
                    Shot.SlapShot = S.Slap,
                    Shot.SnapShot = S.Snap,
                    Shot.Tip = S.Tip,
                    Shot.WrapAround = S.Wrap,
                    Shot.WristShot = S.Wrst,
                    Shot.Wide = Wide,
                    Giveways = iGVA,
                    Takeaways = iTKA,
                    Hits = iHF,
                    Shots.Blocked = iBLK,
                    Plus.Minus = `+/-`
                  )


# Set order ---------------------------------------------------------------

data_final <- data_renamed %>% 
                select(
                  First_Name,
                  Last_Name,
                  Position,
                  All.Position,
                  Team,
                  All.Team,
                  everything()
                )


# Prepare data for clustering ---------------------------------------------

# We also need to replace missing by 0 (we make it simple)
data_final[is.na(data_final)] <- 0
