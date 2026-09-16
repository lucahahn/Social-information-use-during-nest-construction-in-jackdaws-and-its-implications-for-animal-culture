
#Nest observation data 2024
#Author: Luca Hahn
#Last update: 10/01/2026

#Load packages 
install.packages("asnipe")
install.packages("car")
install.packages("carData")
install.packages("chisq.posthoc.test")
install.packages("ClusterR")
install.packages("corrplot")
install.packages("data.table")
install.packages("DHARMa")
install.packages("dplyr")
install.packages("emmeans")
install.packages("ggplot2")
install.packages("glmmTMB")
install.packages("hms")
install.packages("igraph")
install.packages("lme4")
install.packages("lmerTest")
install.packages("lubridate")
install.packages("MASS")
install.packages("multcomp")
install.packages("RColorBrewer")
install.packages("reshape2")
install.packages("rptR")
install.packages("STbayes")
install.packages("stringi")
install.packages("stringr")
install.packages("svMisc")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("viridis")  

library(asnipe)
library(brms)
library(car)
library(carData)
library(chisq.posthoc.test)
library(ClusterR)
library(corrplot)
library(data.table)
library(DHARMa)
library(dplyr)
library(emmeans)
library(extrafont)
font_import()
loadfonts()
library(ggalluvial)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(ggeffects)
library(glmmTMB)
library(gridGraphics)
library(grid)
library(gridBase)
library(hms)
library(igraph)
library(interactions)
library(lme4)
library(lmerTest)
library(lubridate)
library(MASS)
library(performance)
library(multcomp)
library(RColorBrewer)
library(reshape2)
library(rptR)
library(rSDI)
library(STbayes)
library(stringi)
library(stringr)
library(svMisc)
library(tidybayes)
library(tidyr)
library(tidyverse)
library(vegan)
library(viridis)


# (01) IMPORT DATA ----

#Load nest observations 
nest_obs <- read.csv("C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/nest_obs.csv", header = T, stringsAsFactors = F)
nest_obs <- subset(nest_obs, !is.na(nest_obs$Site))
nest_obs <- subset(nest_obs, !nest_obs$Site == "")

nest_obs[substr(nest_obs$Nest.box,1,1)=="X","Site"]<-"X"
nest_obs[substr(nest_obs$Nest.box,1,1)=="Y","Site"]<-"Y"
nest_obs[substr(nest_obs$Nest.box,1,1)=="Z","Site"]<-"Z"

nest_obs26 <- read.csv("C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/nest_obs_26.csv", header = T, stringsAsFactors = F)
nest_obs26 <- na.omit(nest_obs26)
nest_obs26$Site <- substr(nest_obs26$Nestbox, 1, 1)

nest_obs26 <- nest_obs26 %>%
  group_by(Nestbox) %>%
  summarise(across(-c(Date, Comment), sum, na.rm = TRUE))

nest_obs26$Sticks_binary <- ifelse(nest_obs26$Sticks > 0, 1, 0)
nest_obs26$Grass_binary <- ifelse(nest_obs26$Grass > 0, 1, 0)
nest_obs26$Moss_binary <- ifelse(nest_obs26$Moss > 0, 1, 0)
nest_obs26$Leaves_binary <- ifelse(nest_obs26$Leaves > 0, 1, 0)
nest_obs26$Fur_binary <- ifelse(nest_obs26$Fur > 0, 1, 0)
nest_obs26$Feathers_binary <- ifelse(nest_obs26$Feathers > 0, 1, 0)
nest_obs26$Bark_binary <- ifelse(nest_obs26$Bark > 0, 1, 0)
nest_obs26$Paper_binary <- ifelse(nest_obs26$Paper > 0, 1, 0)
nest_obs26$Plastic_binary <- ifelse(nest_obs26$Plastic > 0, 1, 0)
nest_obs26$Fabric_binary <- ifelse(nest_obs26$Fabric > 0, 1, 0)
nest_obs26$Col_wool_binary <- ifelse(nest_obs26$Col_wool > 0, 1, 0)

nest_obs_direct_26 <- nest_obs26

#Load long-term nest checks
nest_checks <- read.csv("C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/nest_checks.csv", header = T, stringsAsFactors = F)

#Filter endoscope observations
nest_obs_indirect <- subset(nest_obs, nest_obs$Method == "endoscope")

#Filter direct observations
nest_obs_direct <- subset(nest_obs, nest_obs$Method == "direct")

#Correct nestbox IDs with a space
nest_obs_direct$Nest.box <- gsub(" ", "", nest_obs_direct$Nest.box, fixed = TRUE)

#Subset
nest_obs_direct_X <- subset(nest_obs_direct, nest_obs_direct$Site == "X")
nest_obs_direct_Y <- subset(nest_obs_direct, nest_obs_direct$Site == "Y")
nest_obs_direct_Z <- subset(nest_obs_direct, nest_obs_direct$Site == "Z")
nest_obs_direct_YZ <- subset(nest_obs_direct, nest_obs_direct$Site == "Y" | nest_obs_direct$Site == "Z")

nest_obs_direct$Cluster <- nests_summary$Cluster[match(nest_obs_direct$Nest.box, nests_summary$Box)]
nest_obs_direct$Cluster <- as.factor(nest_obs_direct$Cluster)
nest_obs_direct$Site <- as.factor(nest_obs_direct$Site)

nest_obs_direct$Sticks <- ifelse(nest_obs_direct$Sticks.twigs == 0, 0, 1)

nest_obs_direct$Grass <- as.numeric(nest_obs_direct$Grass)
nest_obs_direct$Moss <- as.numeric(nest_obs_direct$Moss)
nest_obs_direct$Fur <- as.numeric(nest_obs_direct$Fur)
#nest_obs_direct$Other <- as.numeric(nest_obs_direct$Other)
nest_obs_direct$Anthropogenic <- as.numeric(nest_obs_direct$Anthropogenic)
nest_obs_direct$Leaves <- as.numeric(nest_obs_direct$Leaves)
nest_obs_direct$Feather <- as.numeric(nest_obs_direct$Feather)
nest_obs_direct$Bark <- as.numeric(nest_obs_direct$Bark)
nest_obs_direct$Paper <- as.numeric(nest_obs_direct$Paper)
nest_obs_direct$Plastic <- as.numeric(nest_obs_direct$Plastic)
nest_obs_direct$Fabric <- as.numeric(nest_obs_direct$Fabric)
nest_obs_direct$Coloured.wool <- as.numeric(nest_obs_direct$Coloured.wool)

nest_obs_direct_noNA <- subset(nest_obs_direct, !is.na(nest_obs_direct$Grass))

nest_obs_direct_noNA$Diversity <- (nest_obs_direct_noNA$Sticks + nest_obs_direct_noNA$Grass + nest_obs_direct_noNA$Moss + nest_obs_direct_noNA$Leaves + nest_obs_direct_noNA$Fur + nest_obs_direct_noNA$Bark + nest_obs_direct_noNA$Paper +  nest_obs_direct_noNA$Fabric + nest_obs_direct_noNA$Plastic + nest_obs_direct_noNA$Coloured.wool)

cor.test(nest_obs_direct_noNA$Study.Day, nest_obs_direct_noNA$Diversity)

#Load nest summary
nests_summary <- read.csv("C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/nests_summary.csv", header = T, stringsAsFactors = F)

nests_summary <- subset(nests_summary, !nests_summary$Box == "")

nests_summary_X <- subset(nests_summary, nests_summary$Site == "X")
nests_summary_Y <- subset(nests_summary, nests_summary$Site == "Y")
nests_summary_Z <- subset(nests_summary, nests_summary$Site == "Z")
nests_summary_YZ <- subset(nests_summary, nests_summary$Site == "Y" | nests_summary$Site == "Z")

#Load prelay summary
prelay_summary <- read.csv("C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/2024_prelay_summary.csv", header = T, stringsAsFactors = F)

#Load owner summary 
owner_summary <- read.csv("C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/Owner_Summary_2024.csv", header = T, stringsAsFactors = F)

#Load distance matrix 
distance <- read.csv("C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/distance_matrix.csv", header = T, stringsAsFactors = F)

distance$InputSite <- substr(distance$InputID, 1,1)
distance$TargetSite <- substr(distance$TargetID, 1,1)

distance$SiteComb <- paste(distance$InputSite, distance$TargetSite, sep = "")

distance$InputIDStart <- nests_summary$Start[match(distance$InputID, nests_summary$Box)]
distance$TargetIDStart <- nests_summary$Start[match(distance$TargetID, nests_summary$Box)]

distance$StartDiff <- distance$InputIDStart - distance$TargetIDStart

distance$StartDiffAbs <- abs(distance$StartDiff)

plot(distance$Distance, distance$StartDiffAbs)
cor.test(distance$Distance, distance$StartDiffAbs)

distanceX <- subset(distance, distance$SiteComb == "XX")
distanceY <- subset(distance, distance$SiteComb == "YY")
distanceZ <- subset(distance, distance$SiteComb == "ZZ")

distanceY$InputSquirrel <- nests_summary$squirrel[match(distanceY$InputID, nests_summary$Box)]
distanceY$TargetSquirrel <- nests_summary$squirrel[match(distanceY$TargetID, nests_summary$Box)]

distanceY <- subset(distanceY, InputSquirrel == 0)
distanceY <- subset(distanceY, TargetSquirrel == 0)

#Load box coordinates
box_coordinates <- read.csv("C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/box_coordinates.csv", header = T, stringsAsFactors = F)

#Load box densities
box_densities <- read.csv("C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/box_densities.csv", header = T, stringsAsFactors = F)
box_densities <- subset(box_densities, Year == 24)

#Load box environments
nests_environments <- read.csv("C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/nests_environments.csv", header = T, stringsAsFactors = F)

#Load social pedigree
social_pedigree <- read.csv("C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/social_pedigree.csv", header = T, stringsAsFactors = F)
social_pedigree$kin <- "kin"

#Add age
#Adding individual age
Current_year <- 2024  #Set reference year

#Load saved life history csv file 
LH <- read.csv("C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/LH20241117.csv", header = T, stringsAsFactors = F)
LH <- read.csv("C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/LH20260213.csv", header = T, stringsAsFactors = F)

LH_sex <- subset(LH, !LH$SEX == "")

LH$DATE <- strptime(LH$DATE,format= "%d/%m/%Y")
LH$DATE <- as.Date(LH$DATE, format = "%d/%m/%Y") # convert to date
LH$year <- as.numeric(format(LH$DATE,"%Y"))   #  #Get year from the date
Ringed <- LH %>% filter(CODE == "RINGED")   #Get only records of when birds were ringed for the first time

Ringed$known_age <- 0   #binary 0/1 do we know the exact age (e.g. birds ringed as a 6 are 0)
Ringed$min_age <- 0  #Either actual age (if known_age = 1), or minimum age (if known_age = 0) - currently as number of new years crossed.

for (i in  1:nrow(Ringed)) {
  if(Ringed[i,12] == "4"){
    Ringed$known_age[i] = 0
    Ringed$min_age[i] = (Current_year - Ringed$year[i] +2)
  }
  else if(Ringed[i,12] %in% c("1","1J","3","3J")){
    Ringed$known_age[i] = 1
    Ringed$min_age[i] = (Current_year - Ringed$year[i]+1) 
  }
  else if(Ringed[i,12] == "5"){
    Ringed$known_age[i] = 1
    Ringed$min_age[i] = (Current_year - Ringed$year[i] +2) 
  }
  else if (Ringed[i,12] == "6"){
    Ringed$known_age[i] = 0
    Ringed$min_age[i] = (Current_year - Ringed$year[i] +3) 
  }
  else { Ringed$known_age[i] = 0
  Ringed$min_age[i] = NA }
}

sum(Ringed$known_age)
length(Ringed$known_age)

prelay_summary$female_JID <- Ringed$ID[match(prelay_summary$Female.Rings.2024, Ringed$COMBINATION)]
prelay_summary$female_age <- Ringed$min_age[match(prelay_summary$Female.Rings.2024, Ringed$COMBINATION)]
prelay_summary$male_JID <- Ringed$ID[match(prelay_summary$Male.Rings.2024, Ringed$COMBINATION)]
prelay_summary$male_age <- Ringed$min_age[match(prelay_summary$Male.Rings.2024, Ringed$COMBINATION)]
prelay_summary$pair_age <- (prelay_summary$female_age + prelay_summary$male_age)/2
prelay_summary$pair_age <- ifelse(!is.na(prelay_summary$pair_age), prelay_summary$pair_age, ifelse(is.na(prelay_summary$female_age), prelay_summary$male_age, prelay_summary$female_age))
prelay_summary$pair_age_diff <- prelay_summary$female_age - prelay_summary$male_age

owner_summary$female_JID <- Ringed$ID[match(owner_summary$fem.comb, Ringed$COMBINATION)]
owner_summary$female_age <- Ringed$min_age[match(owner_summary$fem.comb, Ringed$COMBINATION)]
owner_summary$male_JID <- Ringed$ID[match(owner_summary$male.comb, Ringed$COMBINATION)]
owner_summary$male_age <- Ringed$min_age[match(owner_summary$male.comb, Ringed$COMBINATION)]
owner_summary$pair_age <- (owner_summary$female_age + owner_summary$male_age)/2
owner_summary$pair_age <- ifelse(!is.na(owner_summary$pair_age), owner_summary$pair_age, ifelse(is.na(owner_summary$female_age), owner_summary$male_age, owner_summary$female_age))
owner_summary$pair_age_diff <- owner_summary$female_age - owner_summary$male_age
owner_summary$female_JID_prelay <- prelay_summary$female_JID[match(owner_summary$box, prelay_summary$Box)]
owner_summary$male_JID_prelay <- prelay_summary$male_JID[match(owner_summary$box, prelay_summary$Box)]
owner_summary$female_same <- ifelse(owner_summary$female_JID == owner_summary$female_JID_prelay, 1, 0)
owner_summary$male_same <- ifelse(owner_summary$male_JID == owner_summary$male_JID_prelay, 1, 0)

LH_owners <- subset(LH, LH$CODE == "OWNER")
LH_owners$SITE <- substr(LH_owners$BOX, 1,1)
LH_owners <- subset(LH_owners, !SITE == "")
LH_owners$DATE <- as.Date(LH_owners$DATE, format = "%d/%m/%Y") # convert to date
LH_owners$year <- as.numeric(format(LH_owners$DATE,"%Y"))   #  #Get year from the date
LH_owners$box_year <- paste(LH_owners$BOX, LH_owners$year, sep = "_")

#How many times have individuals bred in boxes before
LH_owners <- LH_owners[order(LH_owners$DATE),]
LH_owners <- LH_owners %>%
  group_by(ID) %>%
  mutate(prev_ownership = row_number() - 1) %>%
  ungroup()

LH_owners_females <- subset(LH_owners, SEX == "F")
LH_owners_males <- subset(LH_owners, SEX == "M")

LH_owners_females$JID_year <- paste(LH_owners_females$ID, LH_owners_females$year, sep = "_")
LH_owners_males$JID_year <- paste(LH_owners_males$ID, LH_owners_males$year, sep = "_")

LH_owners_females$JID_site <- paste(LH_owners_females$ID, LH_owners_females$SITE, sep = "_")
LH_owners_males$JID_site <- paste(LH_owners_males$ID, LH_owners_males$SITE, sep = "_")

LH_owners_females$JID_box_year <- paste(LH_owners_females$ID, LH_owners_females$box_year, sep = "_")
LH_owners_males$JID_box_year <- paste(LH_owners_males$ID, LH_owners_males$box_year, sep = "_")

LH_owners_females <- LH_owners_females %>%
  group_by(JID_site) %>%
  mutate(prev_ownership = row_number() - 1) %>%
  ungroup()

LH_owners_males <- LH_owners_males %>%
  group_by(ID_site) %>%
  mutate(prev_ownership = row_number() - 1) %>%
  ungroup()

#Eggs 
LH_eggs <- subset(LH, LH$CODE == "LAID")
LH_eggs$DATE <- as.Date(LH_eggs$DATE, format = "%d/%m/%Y") # convert to date
LH_eggs$day <- yday(LH_eggs$DATE)
LH_eggs$box_year <- paste(LH_eggs$BOX, LH_eggs$year, sep = "_")
LH_eggs <- LH_eggs %>%
  group_by(box_year) %>%
  slice(1)

#Body condition 
LH_body <- subset(LH, !LH$WEIGHT == "")
LH_body <- subset(LH_body, !LH_body$TARSUS == "")
LH_body$BODY_COND <- resid(lm(WEIGHT ~ TARSUS, data = LH_body))

#Take average body condition
LH_body <- subset(LH_body, AGE == "5" | AGE == "6")

LH_body <- LH_body %>% 
  group_by(ID) %>%
  mutate(body_cond = mean(BODY_COND))

LH_body <- LH_body %>% 
  group_by(ID) %>%
  slice(1)

#Take most recent body condition (go back up and skip code on average body cond)
LH_body <- LH_body %>% 
  group_by(ID) %>%
  slice_tail()

nests_summary$Duration <- nests_summary$Egg - nests_summary$Start
nests_summary$female_age <- owner_summary$female_age[match(nests_summary$Box, prelay_summary$Box)]
nests_summary$male_age <- owner_summary$male_age[match(nests_summary$Box, prelay_summary$Box)]

nests_summary$pair_age <- prelay_summary$pair_age[match(nests_summary$Box, prelay_summary$Box)]
nests_summary$pair_age_diff <- prelay_summary$pair_age_diff[match(nests_summary$Box, prelay_summary$Box)]

nests_summary$female_age2 <- ifelse(!is.na(nests_summary$female_age), paste(nests_summary$female_age), paste(nests_summary$male_age))
nests_summary$male_age2 <- ifelse(!is.na(nests_summary$male_age), paste(nests_summary$male_age), paste(nests_summary$female_age))

nests_summary$female_age2 <- as.numeric(nests_summary$female_age2)
nests_summary$male_age2 <- as.numeric(nests_summary$male_age2)

nests_summary_age <- subset(nests_summary, !nests_summary$female_age2 == "NA")
nests_summary_age$pair_age_2 <- (nests_summary_age$female_age2 + nests_summary_age$male_age2) /2

nests_summary$box_year <- paste(nests_summary$Box, 2024, sep = "_")

nests_summary$female_JID <- owner_summary$fem.id[match(nests_summary$Box, owner_summary$box)]
nests_summary$male_JID <- owner_summary$male.id[match(nests_summary$Box, owner_summary$box)]

nests_summary$female_body_cond <- LH_body$BODY_COND[match(nests_summary$female_JID, LH_body$ID)]
nests_summary$male_body_cond <- LH_body$BODY_COND[match(nests_summary$male_JID, LH_body$ID)]

write.csv(nests_summary,"C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/nests_summary.csv", row.names = FALSE)

nest_obs_direct$pair_age <- prelay_summary$pair_age[match(nest_obs_direct$Nest.box, prelay_summary$Box)]
nest_obs_direct$pair_age_diff <- prelay_summary$pair_age_diff[match(nest_obs_direct$Nest.box, prelay_summary$Box)]

nest_obs_direct$female_JID <- prelay_summary$female_JID[match(nest_obs_direct$Nest.box, prelay_summary$Box)]
nest_obs_direct$male_JID <- prelay_summary$male_JID[match(nest_obs_direct$Nest.box, prelay_summary$Box)]

nest_obs_direct$female_feeder <- visit_number$visit_number[match(nest_obs_direct$female_JID, visit_number$JID)]
nest_obs_direct$male_feeder <- visit_number$visit_number[match(nest_obs_direct$male_JID, visit_number$JID)]

nest_obs_direct$female_feeder[is.na(nest_obs_direct$female_feeder)] <- 0
nest_obs_direct$male_feeder[is.na(nest_obs_direct$male_feeder)] <- 0
nest_obs_direct$pair_feeder <- (nest_obs_direct$female_feeder + nest_obs_direct$male_feeder)/2

nest_obs_direct_noNA <- subset(nest_obs_direct, !is.na(nest_obs_direct$Grass))

#Nest observations: anthropogenic material
nest_obs_direct_anthro <- as.data.frame(aggregate(nest_obs_direct_noNA$Anthropogenic, by = list(nest_obs_direct_noNA$Nest.box), FUN = "sum", na.rm = TRUE))
nest_obs_direct_anthro$Box <- nest_obs_direct_anthro$Group.1
nest_obs_direct_anthro$Anthropogenic <- nest_obs_direct_anthro$x
nest_obs_direct_anthro <- subset(nest_obs_direct_anthro, select = -c(Group.1, x))

#Nest observations: paper
nest_obs_direct_paper <- as.data.frame(aggregate(nest_obs_direct_noNA$Paper, by = list(nest_obs_direct_noNA$Nest.box), FUN = "sum", na.rm = TRUE))
nest_obs_direct_paper$Box <- nest_obs_direct_paper$Group.1
nest_obs_direct_paper$Paper <- nest_obs_direct_paper$x
nest_obs_direct_paper <- subset(nest_obs_direct_paper, select = -c(Group.1, x))

#Nest observations: plastic
nest_obs_direct_plastic <- as.data.frame(aggregate(nest_obs_direct_noNA$Plastic, by = list(nest_obs_direct_noNA$Nest.box), FUN = "sum", na.rm = TRUE))
nest_obs_direct_plastic$Box <- nest_obs_direct_plastic$Group.1
nest_obs_direct_plastic$Plastic <- nest_obs_direct_plastic$x
nest_obs_direct_plastic <- subset(nest_obs_direct_plastic, select = -c(Group.1, x))

#Nest observations: fabric
nest_obs_direct_fabric <- as.data.frame(aggregate(nest_obs_direct_noNA$Fabric, by = list(nest_obs_direct_noNA$Nest.box), FUN = "sum", na.rm = TRUE))
nest_obs_direct_fabric$Box <- nest_obs_direct_fabric$Group.1
nest_obs_direct_fabric$Fabric <- nest_obs_direct_fabric$x
nest_obs_direct_fabric <- subset(nest_obs_direct_fabric, select = -c(Group.1, x))

#Nest observations: coloured wool
nest_obs_direct_col_wool <- as.data.frame(aggregate(nest_obs_direct_noNA$Coloured.wool, by = list(nest_obs_direct_noNA$Nest.box), FUN = "sum", na.rm = TRUE))
nest_obs_direct_col_wool$Box <- nest_obs_direct_col_wool$Group.1
nest_obs_direct_col_wool$Col_wool <- nest_obs_direct_col_wool$x
nest_obs_direct_col_wool <- subset(nest_obs_direct_col_wool, select = -c(Group.1, x))

diff_exp <- read.csv("Data/diff_exp.csv")

diff_exp <- diff_exp %>%
  group_by(id) %>%
  slice(1)

diff_exp [23:86,] <- NA

diff_exp$col_wool <- diff_exp$trial

nest_obs_direct_col_wool$Col_wool <- diff_exp$col_wool[match(nest_obs_direct_col_wool$Box, diff_exp$id)]
nest_obs_direct_col_wool$Col_wool <- ifelse(!is.na(nest_obs_direct_col_wool$Col_wool), 1, 0)

#Nest observations: moss
nest_obs_direct_moss <- as.data.frame(aggregate(nest_obs_direct_noNA$Moss, by = list(nest_obs_direct_noNA$Nest.box), FUN = "sum", na.rm = TRUE))
nest_obs_direct_moss$Box <- nest_obs_direct_moss$Group.1
nest_obs_direct_moss$Moss <- nest_obs_direct_moss$x
nest_obs_direct_moss <- subset(nest_obs_direct_moss, select = -c(Group.1, x))

#Nest observations: fur
nest_obs_direct_fur <- as.data.frame(aggregate(nest_obs_direct_noNA$Fur, by = list(nest_obs_direct_noNA$Nest.box), FUN = "sum", na.rm = TRUE))
nest_obs_direct_fur$Box <- nest_obs_direct_fur$Group.1
nest_obs_direct_fur$Fur <- nest_obs_direct_fur$x
nest_obs_direct_fur <- subset(nest_obs_direct_fur, select = -c(Group.1, x))

#Nest observations: leaves
nest_obs_direct_leaves <- as.data.frame(aggregate(nest_obs_direct_noNA$Leaves, by = list(nest_obs_direct_noNA$Nest.box), FUN = "sum", na.rm = TRUE))
nest_obs_direct_leaves$Box <- nest_obs_direct_leaves$Group.1
nest_obs_direct_leaves$Leaves <- nest_obs_direct_leaves$x
nest_obs_direct_leaves <- subset(nest_obs_direct_leaves, select = -c(Group.1, x))

#Nest observations: sticks
nest_obs_direct_sticks <- as.data.frame(aggregate(nest_obs_direct_noNA$Sticks, by = list(nest_obs_direct_noNA$Nest.box), FUN = "sum", na.rm = TRUE))
nest_obs_direct_sticks$Box <- nest_obs_direct_sticks$Group.1
nest_obs_direct_sticks$Sticks <- nest_obs_direct_sticks$x
nest_obs_direct_sticks <- subset(nest_obs_direct_sticks, select = -c(Group.1, x))

#Nest observations: feathers
nest_obs_direct_feathers <- as.data.frame(aggregate(nest_obs_direct_noNA$Feather, by = list(nest_obs_direct_noNA$Nest.box), FUN = "sum", na.rm = TRUE))
nest_obs_direct_feathers$Box <- nest_obs_direct_feathers$Group.1
nest_obs_direct_feathers$Feather <- nest_obs_direct_feathers$x
nest_obs_direct_feathers <- subset(nest_obs_direct_feathers, select = -c(Group.1, x))

#Nest observations: bark
nest_obs_direct_bark <- as.data.frame(aggregate(nest_obs_direct_noNA$Bark, by = list(nest_obs_direct_noNA$Nest.box), FUN = "sum", na.rm = TRUE))
nest_obs_direct_bark$Box <- nest_obs_direct_bark$Group.1
nest_obs_direct_bark$Bark <- nest_obs_direct_bark$x
nest_obs_direct_bark <- subset(nest_obs_direct_bark, select = -c(Group.1, x))

#Nest observations: grass
nest_obs_direct_grass <- as.data.frame(aggregate(nest_obs_direct_noNA$Grass, by = list(nest_obs_direct_noNA$Nest.box), FUN = "sum", na.rm = TRUE))
nest_obs_direct_grass$Box <- nest_obs_direct_grass$Group.1
nest_obs_direct_grass$Grass <- nest_obs_direct_grass$x
nest_obs_direct_grass <- subset(nest_obs_direct_grass, select = -c(Group.1, x))

#Nest observations: other material
nest_obs_direct_other <- as.data.frame(aggregate(nest_obs_direct_noNA$Other, by = list(nest_obs_direct_noNA$Nest.box), FUN = "sum", na.rm = TRUE))
nest_obs_direct_other$Box <- nest_obs_direct_other$Group.1
nest_obs_direct_other$Other <- nest_obs_direct_other$x
nest_obs_direct_other <- subset(nest_obs_direct_other, select = -c(Group.1, x))

#Nest observations: barrier
nest_obs_direct_noNA$Barrier <- as.numeric(nest_obs_direct_noNA$Barrier)
nest_obs_direct_barrier <- as.data.frame(aggregate(nest_obs_direct_noNA$Barrier, by = list(nest_obs_direct_noNA$Nest.box), FUN = "sum", na.rm = TRUE))
nest_obs_direct_barrier$Box <- nest_obs_direct_barrier$Group.1
nest_obs_direct_barrier$Barrier <- nest_obs_direct_barrier$x
nest_obs_direct_barrier <- subset(nest_obs_direct_barrier, select = -c(Group.1, x))
nest_obs_direct_barrier$Barrier_binary <- ifelse(nest_obs_direct_barrier$Barrier >0 ,1,0)
nest_obs_direct_barrier$Density25m <- box_densities$Density25m[match(nest_obs_direct_barrier$Box, box_densities$Nest)]
nest_obs_direct_barrier$Density25m <- ifelse(is.na(nest_obs_direct_barrier$Density25m),0, nest_obs_direct_barrier$Density25m) 
nest_obs_direct_barrier$Site <- substr(nest_obs_direct_barrier$Box, 1,1)
nests_summary$Barrier <- nest_obs_direct_barrier$Barrier_binary[match(nests_summary$Box, nest_obs_direct_barrier$Box)]
table(nests_summary$Barrier, nests_summary$Site)

#Nest observations: all materials
nest_obs_direct_materials <- merge(x = nest_obs_direct_anthro, y = nest_obs_direct_moss, by = "Box", all = TRUE)
nest_obs_direct_materials <- merge(x = nest_obs_direct_materials, y = nest_obs_direct_fur, by = "Box", all = TRUE)
nest_obs_direct_materials <- merge(x = nest_obs_direct_materials, y = nest_obs_direct_sticks, by = "Box", all = TRUE)
nest_obs_direct_materials <- merge(x = nest_obs_direct_materials, y = nest_obs_direct_grass, by = "Box", all = TRUE)
nest_obs_direct_materials <- merge(x = nest_obs_direct_materials, y = nest_obs_direct_leaves, by = "Box", all = TRUE)
nest_obs_direct_materials <- merge(x = nest_obs_direct_materials, y = nest_obs_direct_feathers, by = "Box", all = TRUE)
nest_obs_direct_materials <- merge(x = nest_obs_direct_materials, y = nest_obs_direct_bark, by = "Box", all = TRUE)
nest_obs_direct_materials <- merge(x = nest_obs_direct_materials, y = nest_obs_direct_paper, by = "Box", all = TRUE)
nest_obs_direct_materials <- merge(x = nest_obs_direct_materials, y = nest_obs_direct_plastic, by = "Box", all = TRUE)
nest_obs_direct_materials <- merge(x = nest_obs_direct_materials, y = nest_obs_direct_fabric, by = "Box", all = TRUE)
nest_obs_direct_materials <- merge(x = nest_obs_direct_materials, y = nest_obs_direct_col_wool, by = "Box", all = TRUE)
nest_obs_direct_materials <- merge(x = nest_obs_direct_materials, y = nest_obs_direct_other, by = "Box", all = TRUE)

nest_obs_direct_materials$Cluster <- nests_summary$Cluster[match(nest_obs_direct_materials$Box, nests_summary$Box)]
nest_obs_direct_materials$Site <- nests_summary$Site[match(nest_obs_direct_materials$Box, nests_summary$Box)]

nest_obs_direct_materials$pair_age <- nests_summary$pair_age[match(nest_obs_direct_materials$Box, nests_summary$Box)]

nest_obs_direct_materials$Moss_binary <- ifelse(nest_obs_direct_materials$Moss > 1, 1, 0)
nest_obs_direct_materials$Fur_binary <- ifelse(nest_obs_direct_materials$Fur > 1, 1, 0)
nest_obs_direct_materials$Sticks_binary <- ifelse(nest_obs_direct_materials$Sticks > 1, 1, 0)
nest_obs_direct_materials$Grass_binary <- ifelse(nest_obs_direct_materials$Grass > 1, 1, 0)
nest_obs_direct_materials$Other_binary <- ifelse(nest_obs_direct_materials$Other > 1, 1, 0)
nest_obs_direct_materials$Leaves_binary <- ifelse(nest_obs_direct_materials$Leaves > 1, 1, 0)
nest_obs_direct_materials$Feather_binary <- ifelse(nest_obs_direct_materials$Feather > 1, 1, 0)
nest_obs_direct_materials$Bark_binary <- ifelse(nest_obs_direct_materials$Bark > 1, 1, 0)
nest_obs_direct_materials$Paper_binary <- ifelse(nest_obs_direct_materials$Paper > 1, 1, 0)
nest_obs_direct_materials$Plastic_binary <- ifelse(nest_obs_direct_materials$Plastic > 1, 1, 0)
nest_obs_direct_materials$Col_wool_binary <- ifelse(nest_obs_direct_materials$Col_wool == 1, 1, 0)
nest_obs_direct_materials$Fabric_binary <- ifelse(nest_obs_direct_materials$Fabric > 1, 1, 0)

nest_obs_direct_materials$Anthropogenic2 <- nest_obs_direct_materials$Plastic_binary + nest_obs_direct_materials$Paper_binary + nest_obs_direct_materials$Fabric_binary + nest_obs_direct_materials$Col_wool_binary
nest_obs_direct_materials$Anthropogenic_binary <- ifelse(nest_obs_direct_materials$Anthropogenic2 > 0, 1, 0)

nest_obs_direct_materials$Diversity <- nest_obs_direct_materials$Sticks_binary + nest_obs_direct_materials$Grass_binary + nest_obs_direct_materials$Moss_binary + nest_obs_direct_materials$Fur_binary + nest_obs_direct_materials$Feather_binary + nest_obs_direct_materials$Bark_binary + nest_obs_direct_materials$Leaves_binary + nest_obs_direct_materials$Paper_binary + nest_obs_direct_materials$Plastic_binary + nest_obs_direct_materials$Fabric_binary + nest_obs_direct_materials$Col_wool_binary

nest_obs_direct$female_feeder <- visit_number$visit_number[match(nest_obs_direct$female_JID, visit_number$JID)]
nest_obs_direct$male_feeder <- visit_number$visit_number[match(nest_obs_direct$male_JID, visit_number$JID)]

table(nest_obs_direct_materials$Site, nest_obs_direct_materials$Moss_binary)
table(nest_obs_direct_materials$Site, nest_obs_direct_materials$Sticks_binary)
table(nest_obs_direct_materials$Site, nest_obs_direct_materials$Grass_binary)
table(nest_obs_direct_materials$Site, nest_obs_direct_materials$Fur_binary)
table(nest_obs_direct_materials$Site, nest_obs_direct_materials$Feather_binary)
table(nest_obs_direct_materials$Site, nest_obs_direct_materials$Bark_binary)
table(nest_obs_direct_materials$Site, nest_obs_direct_materials$Leaves_binary)
table(nest_obs_direct_materials$Site, nest_obs_direct_materials$Paper_binary)
table(nest_obs_direct_materials$Site, nest_obs_direct_materials$Plastic_binary)
table(nest_obs_direct_materials$Site, nest_obs_direct_materials$Fabric_binary)
table(nest_obs_direct_materials$Site, nest_obs_direct_materials$Col_wool_binary)

nest_obs_direct_materials <- subset(nest_obs_direct_materials, !nest_obs_direct_materials$Box == "Z25" & !nest_obs_direct_materials$Box == "Z46")

table(nest_obs_direct_materials$Site, nest_obs_direct_materials$Diversity)
boxplot(nest_obs_direct_materials$Diversity ~ nest_obs_direct_materials$Site)
tapply(nest_obs_direct_materials$Diversity, nest_obs_direct_materials$Site, FUN = mean)
tapply(nest_obs_direct_materials$Diversity, nest_obs_direct_materials$Site, FUN = sd)

nest_obs_direct_materials_24 <- nest_obs_direct_materials
nest_obs_direct_materials_24 <- nest_obs_direct_materials_24 %>% 
  rename(Nestbox = Box)

nest_obs_direct_24 <- nest_obs_direct_noNA

nest_obs_direct_24_sub <- nest_obs_direct_24[, c("Nest.box", "Grass", "Moss", "Leaves", "Fur", "Feather", "Bark", "Paper", "Plastic", "Fabric", "Coloured.wool")]

nest_obs_direct_materials_24 <- nest_obs_direct_24_sub %>%
  group_by(Nest.box) %>%
  summarise(across(where(is.numeric), sum))

nest_obs_direct_materials_24$Sticks_binary <- nest_obs_direct_materials$Sticks_binary[match(nest_obs_direct_materials_24$Nest.box, nest_obs_direct_materials$Box)]
nest_obs_direct_materials_24$Grass_binary <- ifelse(nest_obs_direct_materials_24$Grass > 0, 1, 0)
nest_obs_direct_materials_24$Moss_binary <- ifelse(nest_obs_direct_materials_24$Moss > 0, 1, 0)
nest_obs_direct_materials_24$Leaves_binary <- ifelse(nest_obs_direct_materials_24$Leaves > 0, 1, 0)
nest_obs_direct_materials_24$Fur_binary <- ifelse(nest_obs_direct_materials_24$Fur > 0, 1, 0)
nest_obs_direct_materials_24$Feathers_binary <- ifelse(nest_obs_direct_materials_24$Feather > 0, 1, 0)
nest_obs_direct_materials_24$Bark_binary <- ifelse(nest_obs_direct_materials_24$Bark > 0, 1, 0)
nest_obs_direct_materials_24$Paper_binary <- ifelse(nest_obs_direct_materials_24$Paper > 0, 1, 0)
nest_obs_direct_materials_24$Plastic_binary <- ifelse(nest_obs_direct_materials_24$Plastic > 0, 1, 0)
nest_obs_direct_materials_24$Fabric_binary <- ifelse(nest_obs_direct_materials_24$Fabric > 0, 1, 0)
nest_obs_direct_materials_24$Col_wool_binary <- ifelse(nest_obs_direct_materials_24$Coloured.wool > 0, 1, 0)

nest_obs_direct_materials_24$Anthropogenic <- nest_obs_direct_materials_24$Paper_binary + nest_obs_direct_materials_24$Fabric_binary + nest_obs_direct_materials_24$Plastic_binary + nest_obs_direct_materials_24$Col_wool_binary
nest_obs_direct_materials_24$Anthropogenic_binary <- ifelse(nest_obs_direct_materials_24$Anthropogenic > 0, 1, 0)

nest_obs_direct_materials_24$Diversity <- nest_obs_direct_materials_24$Sticks_binary + nest_obs_direct_materials_24$Grass_binary + nest_obs_direct_materials_24$Moss_binary + nest_obs_direct_materials_24$Leaves_binary + nest_obs_direct_materials_24$Fur_binary + nest_obs_direct_materials_24$Feathers_binary + nest_obs_direct_materials_24$Bark_binary + nest_obs_direct_materials_24$Paper_binary + nest_obs_direct_materials_24$Plastic_binary + nest_obs_direct_materials_24$Fabric_binary + nest_obs_direct_materials_24$Col_wool_binary
nest_obs_direct_materials_24$Diversity_max <- 11
nest_obs_direct_materials_24$Site <- substr(nest_obs_direct_materials_24$Nest.box, 1, 1)

nest_obs_direct_materials_24 <- nest_obs_direct_materials_24 %>% 
  rename(Nestbox = Nest.box)

nest_obs_direct_materials_26 <- nest_obs_direct_26 %>%
  group_by(Nestbox) %>%
  summarise(across(where(is.numeric), sum))

nest_obs_direct_materials_26$Sticks_binary <- ifelse(nest_obs_direct_materials_26$Sticks > 0, 1, 0)
nest_obs_direct_materials_26$Grass_binary <- ifelse(nest_obs_direct_materials_26$Grass > 0, 1, 0)
nest_obs_direct_materials_26$Moss_binary <- ifelse(nest_obs_direct_materials_26$Moss > 0, 1, 0)
nest_obs_direct_materials_26$Leaves_binary <- ifelse(nest_obs_direct_materials_26$Leaves > 0, 1, 0)
nest_obs_direct_materials_26$Fur_binary <- ifelse(nest_obs_direct_materials_26$Fur > 0, 1, 0)
nest_obs_direct_materials_26$Feathers_binary <- ifelse(nest_obs_direct_materials_26$Feathers > 0, 1, 0)
nest_obs_direct_materials_26$Bark_binary <- ifelse(nest_obs_direct_materials_26$Bark > 0, 1, 0)
nest_obs_direct_materials_26$Paper_binary <- ifelse(nest_obs_direct_materials_26$Paper > 0, 1, 0)
nest_obs_direct_materials_26$Plastic_binary <- ifelse(nest_obs_direct_materials_26$Plastic > 0, 1, 0)
nest_obs_direct_materials_26$Fabric_binary <- ifelse(nest_obs_direct_materials_26$Fabric > 0, 1, 0)
nest_obs_direct_materials_26$Col_wool_binary <- ifelse(nest_obs_direct_materials_26$Col_wool > 0, 1, 0)

nest_obs_direct_materials_26$Anthropogenic <- nest_obs_direct_materials_26$Paper_binary + nest_obs_direct_materials_26$Fabric_binary + nest_obs_direct_materials_26$Plastic_binary + nest_obs_direct_materials_26$Col_wool_binary
nest_obs_direct_materials_26$Anthropogenic_binary <- ifelse(nest_obs_direct_materials_26$Anthropogenic > 0, 1, 0)

nest_obs_direct_materials_26$Diversity <- nest_obs_direct_materials_26$Sticks_binary + nest_obs_direct_materials_26$Grass_binary + nest_obs_direct_materials_26$Moss_binary + nest_obs_direct_materials_26$Leaves_binary + nest_obs_direct_materials_26$Fur_binary + nest_obs_direct_materials_26$Feathers_binary + nest_obs_direct_materials_26$Bark_binary + nest_obs_direct_materials_26$Paper_binary + nest_obs_direct_materials_26$Plastic_binary + nest_obs_direct_materials_26$Fabric_binary + nest_obs_direct_materials_26$Col_wool_binary
nest_obs_direct_materials_26$Diversity_max <- 11
nest_obs_direct_materials_26$Site <- substr(nest_obs_direct_materials_26$Nestbox, 1, 1)

nest_obs_direct_materials_24_sub <- nest_obs_direct_materials_24[, c("Nestbox", "Diversity", "Diversity_max", "Site", "Anthropogenic_binary")]
nest_obs_direct_materials_24_sub$Year <- 2024

nest_obs_direct_materials_26_sub <- nest_obs_direct_materials_26[, c("Nestbox", "Diversity", "Diversity_max", "Site", "Anthropogenic_binary")]
nest_obs_direct_materials_26_sub$Year <- 2026

nest_obs_direct_materials_2426 <- rbind(nest_obs_direct_materials_24_sub, nest_obs_direct_materials_26_sub)

#Box distance edge list ----

#Calculate distance between nests and experimental setups 
install.packages("rSDI")
library(rSDI)

boxes = unique(box_coordinates$Box)

dist_boxtobox = data.frame()

for (i in seq_along(boxes)) {
  Focal_Box = box_coordinates[box_coordinates$Box == boxes[i], ]
  
  for (j in seq_along(boxes)){
    Other_Box = box_coordinates[box_coordinates$Box == boxes[j], ]
    Distance = haversine(Focal_Box$long, Focal_Box$lat, Other_Box$long, Other_Box$lat, R = 6371)
    
    dfr = data.frame( Focal_Box = boxes[i],
                      Other_Box = boxes[j],
                      Distance = Distance )
    
    dist_boxtobox = rbind(dist_boxtobox, dfr)
    
  }
}

dist_boxtobox$SiteComb <- paste(substr(dist_boxtobox$Focal_Box, 1,1), substr(dist_boxtobox$Other_Box, 1,1), sep = "")

dist_boxtobox$Distance <- dist_boxtobox$Distance * 1000

dist_boxtobox <- dist_boxtobox %>% 
  rename(InputID = Focal_Box, 
         TargetID = Other_Box)

distance <- dist_boxtobox

distance <- subset(distance, !distance$InputID == distance$TargetID)

#Data for manuscript ----

write.csv(event_data24,"C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/Data for manuscript/event_data24.csv", row.names = FALSE)
write.csv(edge_list24,"C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/Data for manuscript/edge_list24.csv", row.names = FALSE)
write.csv(event_data1321,"C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/Data for manuscript/event_data1321.csv", row.names = FALSE)
write.csv(edge_list1321,"C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/Data for manuscript/edge_list1321.csv", row.names = FALSE)
write.csv(nest_checks_start,"C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/Data for manuscript/nest_checks_start.csv", row.names = FALSE)

write.csv(nest_obs_direct_materials_2426,"C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/Data for manuscript/nest_obs_direct_materials_2426.csv", row.names = FALSE)
write.csv(nests_sim_edges,"C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/Data for manuscript/nests_sim_edges.csv", row.names = FALSE)
write.csv(nests_sim_edges_within,"C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/Data for manuscript/nests_sim_edges_within.csv", row.names = FALSE)

write.csv(event_data_exp,"C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/Data for manuscript/event_data_exp.csv", row.names = FALSE)
write.csv(edge_list_exp,"C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/Data for manuscript/edge_list_exp.csv", row.names = FALSE)

write.csv(event_data_exp24,"C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/Data for manuscript/event_data_exp24.csv", row.names = FALSE)
write.csv(edge_list_exp24,"C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/Data for manuscript/edge_list_exp24.csv", row.names = FALSE)


#(02) EXPLORATION ----
plot(nests_summary$pair_age_diff, nests_summary$pair_age)

plot(nests_summary$pair_age, nests_summary$Start)
plot(nests_summary$pair_age_diff, nests_summary$Start)

plot(nests_summary$female_age, nests_summary$Start)
plot(nests_summary$male_age, nests_summary$Start)

plot(nests_summary$pair_age, nests_summary$Egg)
plot(nests_summary$pair_age_diff, nests_summary$Egg)

plot(nests_summary$female_age, nests_summary$Egg)
plot(nests_summary$male_age, nests_summary$Egg)

plot(nests_summary$pair_age, nests_summary$Duration)
plot(nests_summary$pair_age_diff, nests_summary$Duration)

plot(nests_summary$female_age, nests_summary$Duration)
plot(nests_summary$male_age, nests_summary$Duration)

plot(nest_obs_direct_YZ$pair_feeder, nest_obs_direct_YZ$Anthropogenic)

plot(nest_obs_direct$Study.Day, nest_obs_direct$Anthropogenic)

colours <- c("#FFFFFF", "#CCCCCC", "#666666")
colours <- c("#FFFFFF", "#666666")

nest_obs_direct_test <- subset(nest_obs_direct, !is.na(nest_obs_direct$Anthropogenic))

boxplot(nest_obs_direct_test$Anthropogenic ~ nest_obs_direct_test$Site)

start_plot <- ggplot(nests_summary, aes(x = Cluster, y = Start, fill = Site), position=position_dodge(1)) + 
  geom_violin(width = 1) +
  scale_fill_manual(values = colours) +
  labs(x="Cluster", y = "Start of building (day of study period)") +
  theme_classic(base_size = 25)+
  theme(legend.position = "bottom") +
  geom_jitter(shape = 16, width = 0.2, height = 0.02, size = 3) +
  stat_summary(fun=mean, geom="point", shape=8, size=3, col = "black")  
start_plot

start_plot <- ggplot(nests_summary, aes(x = pair_age, y = Start)) + 
  geom_point() 
start_plot

duration_plot <- ggplot(nests_summary, aes(x = Cluster, y = Duration, fill = Site)) + 
  geom_violin(width = 1) +
  scale_fill_manual(values = colours) +
  labs(x="Cluster", y = "Duration of building (number of days)") +
  theme_classic(base_size = 25)+
  theme(legend.position = "bottom") +
  geom_jitter(shape = 16, width = 0.2, height = 0.02, size = 3) +
  stat_summary(fun=mean, geom="point", shape=8, size=3, col = "black")  
duration_plot

Start_Duration_plot <- ggarrange(start_plot, duration_plot, ncol = 2, nrow = 1, labels = c("a", "b"),  widths = c(1, 1), font.label = list(size = 20))
Start_Duration_plot

ggplot(nest_obs_direct_materials) + aes(x = Site, fill = Anthropogenic) +
  scale_fill_manual(values = colours) +
  geom_bar(position = "fill") 

nest_obs_direct_materials$Anthropogenic_binary_ch <- ifelse(nest_obs_direct_materials$Anthropogenic_binary == 1, "Anthropogenic", "No Anthropogenic")

ggplot(nest_obs_direct_materials) +
  aes(x = Site, fill = factor(Anthropogenic_binary_ch)) +
  geom_bar(position = "fill") +
  labs(x = "Site", y = "Proportion of boxes with anthropogenic material", fill = "")

diversity_site_plot <- ggplot(nest_obs_direct_materials) +
  aes(x = Site, fill = factor(Diversity)) +
  geom_bar(position = "fill") +
  labs(x = "Site", y = "Proportion of boxes with anthropogenic material", fill = "")
diversity_site_plot

diversity_cluster_plot <- ggplot(nest_obs_direct_materials) +
  aes(x = Cluster, fill = factor(Diversity)) +
  geom_bar(position = "fill") +
  labs(x = "Site", y = "Proportion of boxes with anthropogenic material", fill = "")
diversity_cluster_plot

nest_obs_direct_materials$Diversity <- as.numeric(nest_obs_direct_materials$Diversity)

diversity_plot <- ggplot(nest_obs_direct_materials, aes(x = Cluster, y = Diversity, fill = Site), position=position_dodge(1)) + 
  geom_violin(width = 1) +
  scale_fill_manual(values = colours) +
  labs(x="Cluster", y = "Number of different material categories") +
  theme_classic(base_size = 25)+
  theme(legend.position = "bottom") +
  geom_jitter(shape = 16, width = 0.2, height = 0.02, size = 3) +
  stat_summary(fun=mean, geom="point", shape=8, size=3, col = "black")  
diversity_plot

Start_Duration_Diversity_plot <- ggarrange(start_plot, duration_plot, diversity_plot, ncol = 2, nrow = 2, labels = c("a", "b", "c"),  widths = c(1, 1), font.label = list(size = 20))
Start_Duration_Diversity_plot

Diversity_Anthro_plot <- ggarrange(diversity_plot, anthro_plot, ncol = 2, nrow = 1, labels = c("a", "b"),  widths = c(1, 1), font.label = list(size = 20))
Diversity_Anthro_plot

anthro_plot <- ggplot(nest_obs_direct_materials, aes(x = Cluster, y = Anthropogenic, col = Site, fill = Site)) +
  geom_violin(position = position_dodge(2)) +  
  scale_fill_manual(values = colours) +
  scale_color_manual(values = lines) +  
  scale_y_continuous(breaks = seq(0, 13, by = 2)) +
  labs(x = "Cluster", y = "Anthropogenic material (no of observations") +
  theme_classic(base_size = 25) +
  theme(legend.position = "bottom") +
  geom_jitter(shape = 16, width = 0.2, height = 0.02, size = 3) +
  stat_summary(fun = mean, geom = "point", shape = 8, size = 3, col = "black")
anthro_plot

Start_Duration_Diversity_Anthro_plot <- ggarrange(start_plot, duration_plot, diversity_plot,anthro_plot, ncol = 2, nrow = 2, labels = c("a", "b", "c"),  widths = c(1, 1), font.label = list(size = 20))
Start_Duration_Diversity_Anthro_plot

geom_jitter(shape=16, position=position_jitter(0.2), size = 3) +
  
plot(nest_obs_direct_materials$pair_age, nest_obs_direct_materials$Anthropogenic_binary)

colours <- c("#FFFFFF", "#999999", "#333333")

anthro_plot <- ggplot(nest_obs_direct_test, aes(x = Site, y = Anthropogenic, fill = Site)) + 
  geom_boxplot(width = 0.75) +
  geom_point() +
  scale_fill_manual(values = colours) +
  labs(x="Site", y = "Anthropogenic material") +
  theme_bw(base_size = 14)
anthro_plot

nest_obs_direct_materials$pair_age_factor <- as.factor(nest_obs_direct_materials$pair_age)

anthro_plot <- ggplot(nest_obs_direct_materials, aes(x = pair_age_factor, y = Anthropogenic)) + 
  geom_boxplot(width = 0.75) +
  geom_point() +
  labs(x="Pair age", y = "Anthropogenic material") +
  theme_bw(base_size = 14)
anthro_plot

anthro_plot <- ggplot(nest_obs_direct_test, aes(x = Site, y = Anthropogenic, fill = Site)) +
  geom_col(color= "black",position = "fill", width = 0.75) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = colours)+ 
  labs(x = "Dyads and Visits", y = "Percentage") + 
  theme_bw(base_size = 16) + 
  theme(legend.position = "bottom")
anthro_plot


anthro_plot <- ggplot(nest_obs_direct_materials) + aes(x = pair_age_factor, fill = Anthropogenic_binary) +
  scale_fill_manual(values = colours) +
  geom_bar() 
anthro_plot

plot(nest_obs_direct_test$Study.Day, nest_obs_direct_test$Diversity)
plot(nest_obs_direct_test$pair_age, nest_obs_direct_test$Diversity)

owner_prospect_plot <- ggplot(individuals, aes(x = owner_visits, y = visits)) + 
  geom_point() +
  scale_y_continuous(limits = c(0, 50))
owner_prospect_plot


#(03) STATISTICAL MODELS ----

#01. Nest initiation ----


#NBDA 2024----

#Final Multi-network NBDA 

#All boxes except for Z25 and Z46 
#Spatial and Prospecting network
#Transmission weight?
#Repeat with boxes that did NOT have squirrels 

#Event data
nests_summary$Egg_Neststart <- nests_summary$Egg - nests_summary$Start

#Exclude boxes with squirrels (default)
event_data <- nests_summary[c("Box", "Start", "squirrel")]
event_data <- nests_summary[c("Box", "Egg", "squirrel")]

event_data <- subset(event_data, !event_data$squirrel == 1)

#Alternatively, with boxes occupied by squirrels)
event_data <- nests_summary[c("Box", "Start")]

event_data <- nests_summary[c("Box", "Egg")]
event_data <- subset(event_data, !is.na(Egg))

event_data <- nests_summary[c("Box", "Egg_Neststart")]
event_data <- subset(event_data, !is.na(Egg_Neststart))

#Rename columns for STBayes
event_data <- event_data  %>% rename(id = Box)
event_data <- event_data  %>% rename(time = Start)

event_data <- event_data  %>% rename(time = Egg)
event_data <- event_data  %>% rename(time = Egg_Neststart)

#All boxes in the same trial
event_data$trial <- 1

#Set end as last initiation + 1 
#Excluding boxes with squirrels 
event_data$t_end <- 31 #Nest start
event_data$t_end <- 66 #Egg
event_data$t_end <- 55 #Egg - nest start

event_data$squirrel <- NULL

#Including boxes with squirrels
event_data$t_end <- 36

#Nests initiated before logger deployment set as demonstrators
event_data$time <- ifelse(event_data$time < 6, 0, event_data$time)

#event_data$time <- ifelse(event_data$time < 5, 0, event_data$time)
#event_data$time <- ifelse(event_data$time < 4, 0, event_data$time)
#event_data$time <- ifelse(event_data$time < 3, 0, event_data$time)
#event_data$time <- ifelse(event_data$time < 2, 0, event_data$time)

#Remove Z25 (start is NA) and Z46 (box deployed late)
event_data <- subset(event_data, !is.na(event_data$time))
event_data <- subset(event_data, !event_data$id == "Z46")

event_data24 <- event_data

#Edge list (start with spatial data)

#Spatial distance matrix
edge_list_spatial <- distance[, c("InputID", "TargetID", "Distance", "SiteComb")]

#Only consider distances within study sites
edge_list_spatial <- subset(edge_list_spatial, edge_list_spatial$SiteComb == "XX" | edge_list_spatial$SiteComb == "YY" | edge_list_spatial$SiteComb == "ZZ")

#Remove weaker spatial connections (more conservative)
median(edge_list_spatial$Distance) #100 m
mean(edge_list_spatial$Distance) #118 m
sd(edge_list_spatial$Distance) #79 m

edge_list_spatial <- subset(edge_list_spatial, Distance < 25)
edge_list_spatial <- subset(edge_list_spatial, Distance < 50)
edge_list_spatial <- subset(edge_list_spatial, Distance < 100)
edge_list_spatial <- subset(edge_list_spatial, Distance < 150)
edge_list_spatial <- subset(edge_list_spatial, Distance < 200)

#Rename nodes
edge_list_spatial <- edge_list_spatial %>% rename(from = InputID)
edge_list_spatial <- edge_list_spatial  %>% rename(to = TargetID)

#Filter edge list to include boxes in event_data
edge_list_spatial <- edge_list_spatial %>%
  filter(from %in% event_data$id)

edge_list_spatial <- edge_list_spatial %>%
  filter(to %in% event_data$id)

#Minimum and maximum distances between boxes
dmin <- min(edge_list_spatial$Distance)
dmax <- max(edge_list_spatial$Distance)

#Same trial 
edge_list_spatial$trial <- 1

#Add spatial weight, standardised from 0 to 1
edge_list_spatial$spatial_weight <- (dmax - edge_list_spatial$Distance) / (dmax - dmin)

#Remove Distance and SiteComb
edge_list_spatial <- edge_list_spatial[, c("from", "to", "trial", "spatial_weight")]

#Add box dyad ID
edge_list_spatial$box_dyad_ID <- paste(edge_list_spatial$from, edge_list_spatial$to, sep = "_")

edge_list_spatial24 <- edge_list_spatial

#Prospecting network 

#Only box owners visiting each other
visits_prospect_owners <- subset(visits_prospect, !is.na(visitor_owned_box))

#Box dyad ID
visits_prospect_owners$box_dyad_ID <- paste(visits_prospect_owners$box, visits_prospect_owners$visitor_owned_box, sep = "_")

#Summary dataset for all dyads
visits_prospect_owners_box_dyads <- as.data.frame(table(visits_prospect_owners$box_dyad_ID))

#Rename variables
visits_prospect_owners_box_dyads <- visits_prospect_owners_box_dyads %>% rename(box_dyad_ID = Var1)
visits_prospect_owners_box_dyads <- visits_prospect_owners_box_dyads %>% rename(prospect_weight = Freq)

#Add nodes
visits_prospect_owners_box_dyads$from <- visits_prospect_owners$box[match(visits_prospect_owners_box_dyads$box_dyad_ID, visits_prospect_owners$box_dyad_ID)]
visits_prospect_owners_box_dyads$to <- visits_prospect_owners$visitor_owned_box[match(visits_prospect_owners_box_dyads$box_dyad_ID, visits_prospect_owners$box_dyad_ID)]

#Edge list for prospecting
edge_list_prospect <- visits_prospect_owners_box_dyads

#Same trial
edge_list_prospect$trial <- 1

#Binary edge list for prospecting
edge_list_prospect$prospect_weight_binary <- 1

#Full edge list 
edge_list <- edge_list_spatial

#Binary prospecting weight
edge_list$prospect_weight <- edge_list_prospect$prospect_weight_binary[match(edge_list$box_dyad_ID, edge_list_prospect$box_dyad_ID)]

#Alternatively, numeric prospecting weight
edge_list$prospect_weight <- edge_list_prospect$prospect_weight[match(edge_list$box_dyad_ID, edge_list_prospect$box_dyad_ID)]

#Remove box dyad ID
edge_list$box_dyad_ID <- NULL

#Fill missing prospect edges as 0 (i.e., no prospecting)
edge_list$prospect_weight <- ifelse(is.na(edge_list$prospect_weight), 0, edge_list$prospect_weight)

edge_list$spatial_weight <- NULL

edge_list24 <- edge_list


#If filtered <50 m, add edge 0 for Y33
edge_list[411,] <- NA 
edge_list$from[411] <- "Y33"
edge_list$to[411] <- "Y01"
edge_list$spatial_weight[411] <- 0
edge_list$prospect_weight[411] <- 0
edge_list$trial[411] <- 1


#Individual-level variables (ILVs)
nests_summary$pair_age_z <- scale(nests_summary$pair_age)

ILV <- data.frame(
  id = nests_summary$Box[nests_summary$squirrel == 0],
  age = nests_summary$pair_age_z[nests_summary$squirrel == 0],
  site = nests_summary$SiteNum[nests_summary$squirrel == 0]
)

#Remove boxes with no start date
ILV <- subset(ILV, !id == "Z25")
ILV <- subset(ILV, !id == "Z46")

ILV <- ILV  %>%
  filter(id %in% event_data$id)

#Y12 pair age is missing, set as 0 in scaled data
ILV$age[ILV$id =="Y12"] <- 0

#Transmission weight (not sure for now)
t_weights_static <- nests_summary[, c("Box", "pair_visits_stand")]
t_weights_static <- t_weights_static %>% rename(id = Box)
t_weights_static <- t_weights_static %>% rename(t_weight = pair_visits_stand)

t_weights_static <- t_weights_static %>%
  filter(id %in% event_data$id)

median(t_weights_static$t_weight, na.rm = TRUE)
mean(t_weights_static$t_weight, na.rm = TRUE)

#Set unobserved weights as 1
t_weights_static$t_weight <- ifelse(is.na(t_weights_static$t_weight), 1, t_weights_static$t_weight)

#Set unobserved weights as mean/median
t_weights_static$t_weight <- ifelse(is.na(t_weights_static$t_weight), 16, t_weights_static$t_weight)

t_weights_static$trial <- 1

#Data list for NBDA

#Code uses event_data and edge_list, so event_data24 and edge_list24 need to be renamed here
event_data <- event_data24
edge_list <- edge_list24

orig_times <- event_data$time

set.seed(200)
event_data_perm <- event_data
event_data_perm$time <- sample(orig_times, replace = FALSE)

edge_list$prospect_weight <- NULL

#Without transmission weights
data_list_social <- import_user_STb(
  event_data = event_data,
  networks = edge_list)

data_list_perm <- import_user_STb(
  event_data = event_data_perm,
  networks = edge_list)

#With transmission weights and without ILVs
data_list_social <- import_user_STb(
  event_data = event_data,
  networks = edge_list,
  t_weights = t_weights_static)

#Without transmission weights and with ILVs
data_list_social_ILV <- import_user_STb(
  event_data = event_data,
  networks = edge_list,
  ILV_c = ILV,
  ILVi = c("age"),
  ILVs = c("age")
)

#With transmission weights and with ILVs
data_list_social_ILV <- import_user_STb(
  event_data = event_data,
  networks = edge_list,
  t_weights = t_weights_static,
  ILVc = ILV,
  ILVi = c("age"),
  ILVs = c("age")
)

#Generate social models with and without ILV
model_social <- generate_STb_model(data_list_social, 
                                   gq = T, 
                                   est_acqTime = F, 
                                   data_type = c("discrete_time"))

model_social <- generate_STb_model(data_list_social, 
                                   gq = T, 
                                   est_acqTime = T, 
                                   data_type = c("discrete_time"),
                                   veff_params = c("lambda_0", "s"), 
                                   veff_type=c("id", "trial")
)

model_social <- generate_STb_model(data_list_social, 
                                   gq = T, 
                                   est_acqTime = T, 
                                   data_type = c("discrete_time"),
                                   transmission_func="freqdep_f")

model_social_perm <- generate_STb_model(data_list_perm, 
                                   gq = T, 
                                   est_acqTime = T, 
                                   data_type = c("discrete_time"))

model_social_ILV <- generate_STb_model(data_list_social_ILV, 
                                 gq = T, 
                                 est_acqTime = F, 
                                 data_type = c("discrete_time"))

#Fit social model
model_social_fit <- fit_STb(data_list_social,
                    model_social,
                    parallel_chains = 4,
                    chains = 4,
                    cores = 4,
                    iter = 4000,
                    refresh=1000
)

model_social_perm_fit <- fit_STb(data_list_perm,
                            model_social,
                            parallel_chains = 4,
                            chains = 4,
                            cores = 4,
                            iter = 4000,
                            refresh=1000
)

s1_draws <- as_draws_df(model_social_perm_fit$draws(variables = "s[1]", inc_warmup = FALSE))$`s[1]`
prob_s1_positive <- mean(s1_draws > 0 )
prob_s1_positive

#Fit social model with ILV
model_social_ILV_fit <- fit_STb(data_list_social_ILV,
                            model_social_ILV,
                            parallel_chains = 4,
                            chains = 4,
                            cores = 4,
                            iter = 8000,
                            refresh=1000
)

#Save social models
STb_save(model_social_fit, output_dir = "cmdstan_saves", name="model_social_fit")
STb_save(model_social_ILV_fit, output_dir = "cmdstan_saves", name="model_social_ILV_fit")

#Summary for social models 
STb_summary(model_social_fit, digits = 3)
STb_summary(model_social_perm_fit, digits = 3)
STb_summary(model_social_ILV_fit, digits = 3)

#Generate asocial models
model_asocial = generate_STb_model(data_list_social, 
                                   model_type="asocial", 
                                   data_type = c("discrete_time"),
                                   gq = T, 
                                   est_acqTime = F, 
)
model_asocial_perm = generate_STb_model(data_list_perm, model_type="asocial", data_type = c("discrete_time"))
model_asocial_ILV = generate_STb_model(data_list_social_ILV, 
                                       model_type="asocial", 
                                       data_type = c("discrete_time"),
                                       gq = T, 
                                       est_acqTime = F, 
)

#Run asocial models
model_asocial_fit = fit_STb(data_list_social,
                      model_asocial,
                      parallel_chains = 4,
                      chains = 4,
                      cores = 4,
                      iter = 4000,
                      refresh=1000)

model_asocial_perm_fit = fit_STb(data_list_perm,
                            model_asocial_perm,
                            parallel_chains = 4,
                            chains = 4,
                            cores = 4,
                            iter = 4000,
                            refresh=1000)

model_asocial_ILV_fit = fit_STb(data_list_social_ILV,
                            model_asocial_ILV,
                            parallel_chains = 4,
                            chains = 4,
                            cores = 4,
                            iter = 4000,
                            refresh=1000)

#Summary for asocial models
STb_summary(model_asocial_fit, digits = 3)
STb_summary(model_asocial_ILV_fit, digits = 3)

#Compare models via LOO cross validation

#All four models
loo_output = STb_compare(model_social_fit, model_asocial_fit, model_social_ILV_fit, model_asocial_ILV_fit, method="loo-psis")

#Just two models without ILV
loo_output = STb_compare(model_social_fit, model_asocial_fit, method="loo-psis")
loo_output = STb_compare(model_social_perm_fit, model_asocial_perm_fit, method="loo-psis")
loo_output = STb_compare(model_social_fit, model_social_perm_fit, method="loo-psis")

#Print LOO output
print(loo_output$comparison, simplify = FALSE)


#Model 2024 ----

#Nest box centrality 

#Edge list
edge_list_spatial_full <- distance[, c("InputID", "TargetID", "Distance", "SiteComb")]
edge_list_spatial_full <- subset(edge_list_spatial_full, edge_list_spatial_full$SiteComb == "XX" | edge_list_spatial_full$SiteComb == "YY" | edge_list_spatial_full$SiteComb == "ZZ")

dmin <- min(edge_list_spatial_full$Distance)
dmax <- max(edge_list_spatial_full$Distance)

edge_list_spatial_full$spatial_weight <- (dmax - edge_list_spatial_full$Distance) / (dmax - dmin)
edge_list_spatial_full$Distance <- NULL
edge_list_spatial_full$SiteComb <- NULL

edge_list_spatial_full <- edge_list_spatial_full  %>% rename(from = InputID)
edge_list_spatial_full <- edge_list_spatial_full  %>% rename(to = TargetID)

edge_list_spatial_full <- edge_list_spatial_full %>%
  filter(from %in% nests_summary$Box)

edge_list_spatial_full <- edge_list_spatial_full %>%
  filter(to %in% nests_summary$Box)

edge_list_spatial_full$box_dyad_ID <- ifelse(edge_list_spatial_full$from < edge_list_spatial_full$to, paste(edge_list_spatial_full$from, edge_list_spatial_full$to, sep = "_"), paste(edge_list_spatial_full$to, edge_list_spatial_full$from, sep = "_"))

edge_list_spatial_full <- edge_list_spatial_full %>%
  group_by(box_dyad_ID) %>%
  arrange(box_dyad_ID) %>%
  slice(1)

edge_list_spatial_full <- edge_list_spatial_full  %>% pivot_longer(
  cols = c("from", "to"), names_to = "edge", values_to = "Box")

#Nest "centrality"
nests_centrality <- as.data.frame(aggregate(edge_list_spatial_full$spatial_weight, by = list(edge_list_spatial_full$Box), FUN = "sum", na.rm = TRUE))
nests_centrality <- nests_centrality %>% rename(Box = Group.1)
nests_centrality <- nests_centrality %>% rename(Centrality = x)
nests_centrality$Centrality_stand <- ifelse(substr(nests_centrality$Box,1,1) == "X", nests_centrality$Centrality/14, ifelse(substr(nests_centrality$Box,1,1) =="Y", nests_centrality$Centrality/33, nests_centrality$Centrality/38))

nests_summary$Centrality <- nests_centrality$Centrality_stand[match(nests_summary$Box, nests_centrality$Box)]

nests_summary$pair_age_z <- scale(nests_summary$pair_age) 
nests_summary$egg_z <- scale(nests_summary$Egg)
nests_summary$Centrality_z <- scale(nests_summary$Centrality) 

nests_summary_nosquirrel <- subset(nests_summary, squirrel == 0)

#Model just with prior
default_prior()

nests_start_brm1_prior <- brm(Start ~ Site + squirrel +
                                Ownership2324 + pair_age_z + egg_z +
                                Centrality_z,   
                              data = nests_summary, 
                              family = Gamma(link = "log"),
                              prior = c(
                                prior(normal(2, 1), class = "Intercept"),        
                                prior(normal(0, 0.5), class = "b"),
                                prior(gamma(2, 0.5), class = "shape")),   
                              sample_prior = "only"
)

#Prior predictive checks 
pp_check(nests_start_brm1_prior, ndraws = 100) +
  scale_x_log10()

#Model 
nests_start_brm1_log <- brm(Start ~ Site + squirrel +
                              Ownership2324 + pair_age_z + egg_z +
                              Centrality_z,   
                              data = nests_summary, 
                              family = lognormal(link = "identity"),
                              prior = c(
                                prior(normal(2, 1), class = "Intercept"),        
                                prior(normal(0, 0.5), class = "b"),
                                prior(normal(0, 1), class = "sigma"))
)          

nests_start_brm1_gamma <- brm(Start ~ Site + squirrel +
                                Ownership2324 + pair_age_z + egg_z +
                                Centrality_z,   
                              data = nests_summary, 
                              family = Gamma(link = "log"),
                              prior = c(
                                prior(normal(2, 1), class = "Intercept"),        
                                prior(normal(0, 0.5), class = "b"),
                                prior(gamma(2, 0.5), class = "shape"))
                              )

nests_start_brm1_gamma <- brm(Start ~ Site + pair_age_z +
                                Centrality_z,   
                              data = nests_summary_nosquirrel, 
                              family = Gamma(link = "log"),
                              prior = c(
                                prior(normal(2, 1), class = "Intercept"),        
                                prior(normal(0, 0.5), class = "b"),
                                prior(gamma(2, 0.5), class = "shape"))
)

nests_start_brm1_gamma <- brm(Start ~ Site +
                                Centrality_z,   
                              data = nests_summary, 
                              family = Gamma(link = "log"),
                              prior = c(
                                prior(normal(2, 1), class = "Intercept"),        
                                prior(normal(0, 0.5), class = "b"),
                                prior(gamma(2, 0.5), class = "shape"))
)

#Check collinearity
check_collinearity(nests_start_brm1)

#Model summary
summary(nests_start_brm1)
summary(nests_start_brm1_gamma)

#Posterior predictive checks
pp_check(nests_start_brm1, type = "dens_overlay", ndraws = 100)
pp_check(nests_start_brm1_log, type = "dens_overlay", ndraws = 100)
pp_check(nests_start_brm1_gamma, type = "dens_overlay", ndraws = 100)

#Plot model
plot(nests_start_brm1)
launch_shinystan(nests_start_brm1)

#Posterior distribution
as_draws_df(nests_start_brm1)
mcmc_areas(nests_start_brm1)
mcmc_intervals(nests_start_brm1)

#Evaluation and interpretation
loo(nests_start_brm1, moment_match = TRUE)
nests_start_brm1_log_loo <- loo(nests_start_brm1_log, moment_match = TRUE)
nests_start_brm1_gamma_loo <- loo(nests_start_brm1_gamma, moment_match = TRUE)

loo_compare(nests_start_brm1_log_loo, nests_start_brm1_gamma_loo)

fitted(nests_start_brm1, scale = "response")
conditional_effects(nests_start_brm1)
bayes_R2(nests_start_brm1)

#Sensitivity analysis and prior checks 
prior_summary(nests_start_brm1)

#Extract predictions
fitted(nests_start_brm1)

#Hypothesis 
hypothesis(nests_start_brm1_gamma, "Centrality_z < 0")

hypothesis(nests_start_brm1_gamma, "Centrality_z < 0")
hypothesis(nests_start_brm1, "squirrel > 0")
hypothesis(nests_start_brm1_gamma, "pair_age_z < 0")
hypothesis(nests_start_brm1_gamma, "egg_z > 0")
hypothesis(nests_start_brm1, "Ownership2324 > 0")

emmeans(nests_start_brm1_gamma, ~ Site, type = "response") |> pairs()


#Long-term nest checks 2013-2023 ----

#Load long-term nest checks
nest_checks <- read.csv("C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/nest_checks.csv", header = T, stringsAsFactors = F)

#Add year and box/year
nest_checks$Date <- as.Date(nest_checks$Date, format = "%d/%m/%Y") # convert to date
nest_checks$year <- as.numeric(format(nest_checks$Date,"%Y"))   #  #Get year from the date
nest_checks$box_year <- paste(nest_checks$BOX, nest_checks$year, sep = "_")

#Remove data from 2020 and 2024
nest_checks <- subset(nest_checks, !year == 2020)
nest_checks <- subset(nest_checks, !year == 2024)

#Classify 1-5 sticks as "few", and make corrections
nest_checks <- nest_checks %>% 
  mutate(Few = case_when(
    Twigs == 0 ~ "0",
    Twigs == 1 ~ "FEW",
    Twigs == 2  ~ "FEW",
    Twigs == 3 ~ "FEW",
    Twigs == 4 ~ "FEW",
    Twigs == 5 ~ "FEW",
    Twigs == "FEW"~ "FEW",
    Twigs == "lined" ~ "LINED",
    Twigs == "LINED" ~ "LINED",
    Twigs == "Many" ~ "MANY",
    Twigs == "MANY" ~ "MANY"))

#Test if nest checks start at different times across years

#Copy of dataset
nest_checks_test <- nest_checks

#Remove instances with no info about twigs
nest_checks_test <- subset(nest_checks_test, !Twigs == "")

#Only take obs with "FEW" or "FEW" and 0 sticks
nest_checks_test <- subset(nest_checks_test, Few == "FEW")
nest_checks_test <- subset(nest_checks_test, Few == "FEW" | Few == "0")

#Add study site
nest_checks_test$site <- substr(nest_checks_test$BOX, 1, 1)

#Only keep sites X, Y, Z
nest_checks_test <- subset(nest_checks_test, !site == "M" & !site == "E" & !site =="W" & !site =="G" & !site == "C")
nest_checks_test <- subset(nest_checks_test, site == "X" | site == "Y" | site == "Z")

#Add site_year and year_site (for plots etc)
nest_checks_test$site_year <- paste(nest_checks_test$site, nest_checks_test$year, sep = "_")
nest_checks_test$year_site <- paste(nest_checks_test$year, nest_checks_test$site, sep = "_")

#Add day of year
nest_checks_test$day <- yday(nest_checks_test$Date)

#If filtered only "FEW", take first observation
nest_checks_test <- nest_checks_test %>%
  group_by(site_year) %>%
  arrange(site_year) %>%
  slice(1)

#To test if sites were sampled equally often
nest_checks_test <- nest_checks_test %>%
  count(box_year)

#Add site, box, year again
nest_checks_test$site <- substr(nest_checks_test$box_year, 1, 1)
nest_checks_test$box <- substr(nest_checks_test$box_year, 1, 3)
nest_checks_test$year <- substr(nest_checks_test$box_year, 5, 8)

nest_checks_test <- subset(nest_checks_test, !year == "023" & !year == "2019" & !year == "2022" & !year== "2023")

nest_checks_test_m <- glmmTMB(n ~ site + (1|box), data = nest_checks_test, family = poisson)

summary(nest_checks_test_m)
Anova(nest_checks_test_m)
emmeans(nest_checks_test_m, ~ site, type = "response") |> pairs()

tapply(nest_checks_test$n, nest_checks_test$site, FUN = mean)
tapply(nest_checks_test$n, nest_checks_test$site, FUN = sd)

#no difference in no of observations across sites

boxplot(nest_checks_test$day ~ nest_checks_test$site_year)
boxplot(nest_checks_test$day ~ nest_checks_test$year_site)

#Similar start of nest checks 2013-2018, 2021
#Nest checks started later in 2019, 2022, and 2023
#No nest checks in 2020
#Just one nest check in 2022
#Nest checks in 2024 are separate due to methodology

#Subset start and lined
nest_checks_start <- subset(nest_checks, Twigs == "FEW")
nest_checks_lined <- subset(nest_checks, Twigs == "LINED")

nest_checks_start <- nest_checks_start %>%
  group_by(box_year) %>%
  arrange(box_year) %>%
  slice(1)

nest_checks_lined <- nest_checks_lined %>%
  group_by(box_year) %>%
  arrange(box_year) %>%
  slice(1)

nest_checks_start$start <- yday(nest_checks_start$Date)
nest_checks_lined$lined <- yday(nest_checks_lined$Date)

nest_checks_start$site <- substr(nest_checks_start$BOX, 1, 1)
nest_checks_lined$site <- substr(nest_checks_lined$BOX, 1, 1)

nest_checks_start$site_year <- paste(nest_checks_start$site, nest_checks_start$year, sep = "_")
nest_checks_lined$site_year <- paste(nest_checks_lined$site, nest_checks_lined$year, sep = "_")

nest_checks_start <- subset(nest_checks_start, site == "X" | site == "Y" | site == "Z")
nest_checks_lined <- subset(nest_checks_lined, site == "X" | site == "Y" | site == "Z")

nest_checks_start$year <- as.numeric(nest_checks_start$year)
nest_checks_lined$year <- as.numeric(nest_checks_lined$year)

nest_checks_start <- subset(nest_checks_start, start > 25)
nest_checks_start <- subset(nest_checks_start, start < 120)
#nest_checks_lined <- subset(nest_checks_lined, day < 120)

nest_checks_start$year_z <- scale(nest_checks_start$year)
nest_checks_lined$year_z <- scale(nest_checks_lined$year)

nest_checks_start$start_first_check <- nest_checks_test$day[match(nest_checks_start$site_year, nest_checks_test$site_year)]
nest_checks_start$start_first_check_z <- scale(nest_checks_start$start_first_check)

nest_checks_start$start_vs_first_check <- (nest_checks_start$start - nest_checks_start$start_first_check)

nest_checks_start$year <- as.factor(nest_checks_start$year)

#Add pairs 
nest_checks_start$female_JID <- LH_owners_females$ID[match(nest_checks_start$box_year, LH_owners_females$box_year)]
nest_checks_start$male_JID <- LH_owners_males$ID[match(nest_checks_start$box_year, LH_owners_males$box_year)]
nest_checks_start$pair_ID <- paste(nest_checks_start$female_JID, nest_checks_start$male_JID, sep = "_")

nest_checks_start$female_age_24 <- Ringed$min_age[match(nest_checks_start$female_JID, Ringed$ID)]
nest_checks_start$male_age_24 <- Ringed$min_age[match(nest_checks_start$male_JID, Ringed$ID)]

nest_checks_start$female_age <- nest_checks_start$female_age_24 - (2024 - nest_checks_start$year)
nest_checks_start$male_age <- nest_checks_start$male_age_24 - (2024 - nest_checks_start$year)

nest_checks_start <- nest_checks_start %>% 
  mutate(pair_age = case_when(
    is.na(female_age) & !is.na(male_age) ~ male_age,
    !is.na(female_age) & is.na(male_age) ~ female_age,
    !is.na(female_age) & !is.na(male_age) ~ (female_age + male_age) /2,
    is.na(female_age) & is.na(male_age) ~ NA))

#Add 2024 data
nest_checks_start[397:474,] <- NA
nest_checks_start$BOX[397:474] <- nests_summary_nosquirrel$Box
nest_checks_start$start[397:474] <- nests_summary_nosquirrel$Start + 61
nest_checks_start$site[397:474] <- nests_summary_nosquirrel$Site
nest_checks_start$year[397:474] <- 2024
nest_checks_start$start_first_check[397:474] <- 60
nest_checks_start$pair_age[397:474] <- nests_summary_nosquirrel$pair_age

nest_checks_start$box_year <- paste(nest_checks_start$BOX, nest_checks_start$year, sep = "_")

nest_checks_start$site_year <- paste(nest_checks_start$site, nest_checks_start$year, sep = "_")
nest_checks_lined$site_year <- paste(nest_checks_lined$site, nest_checks_lined$year, sep = "_")

nest_checks_start$fem_JID_year <- paste(nest_checks_start$female_JID, nest_checks_start$year, sep = "_")
nest_checks_start$male_JID_year <- paste(nest_checks_start$male_JID, nest_checks_start$year, sep = "_")

nest_checks_start$fem_prev_owner <- LH_owners_females$prev_ownership[match(nest_checks_start$fem_JID_year, LH_owners_females$JID_year)]
nest_checks_start$male_prev_owner <- LH_owners_males$prev_ownership[match(nest_checks_start$male_JID_year, LH_owners_males$JID_year)]

nest_checks_start$fem_prev_owner_binary <- ifelse(nest_checks_start$fem_prev_owner < 1, 0, 1)
nest_checks_start$male_prev_owner_binary <- ifelse(nest_checks_start$male_prev_owner < 1, 0, 1)

nest_checks_start <- nest_checks_start %>%
  group_by(site_year) %>%                # Group by 'group'
  mutate(mean_start_site_year = mean(start)) %>% # Calculate mean per group
  ungroup()    

nest_checks_start$start_diff_from_mean <- nest_checks_start$start - nest_checks_start$mean_start_site_year
nest_checks_start$start_diff_from_mean_abs <- abs(nest_checks_start$start_diff_from_mean)


#Scale variabels (again)
nest_checks_start$year_z <- scale(nest_checks_start$year)
nest_checks_start$start_first_check_z <- scale(nest_checks_start$start_first_check)
nest_checks_start$pair_age_z <- scale(nest_checks_start$pair_age)

nest_checks_start_age <- subset(nest_checks_start, !is.na(pair_age))

nest_checks_start_age_unique <- nest_checks_start_age %>%
  group_by(pair_ID) %>%
  mutate(avg_pair_age = mean(pair_age, na.rm = TRUE)) %>%
  ungroup()

nest_checks_start_age$avg_pair_age <- nest_checks_start_age_unique$avg_pair_age[match(nest_checks_start_age$pair_ID, nest_checks_start_age_unique$pair_ID)]
nest_checks_start_age$cent_pair_age <- nest_checks_start_age$pair_age - nest_checks_start_age$avg_pair_age

nest_checks_start_age$avg_pair_age_z <- scale(nest_checks_start_age$avg_pair_age)
nest_checks_start_age$cent_pair_age_z <- scale(nest_checks_start_age$cent_pair_age)

nest_checks_start_age$female_prev_own <- LH_owners_females$prev_ownership[match(nest_checks_start_age$box_year, LH_owners_females$box_year)]
nest_checks_start_age$male_prev_own <- LH_owners_males$prev_ownership[match(nest_checks_start_age$box_year, LH_owners_males$box_year)]

plot(nest_checks_start_age$female_age, nest_checks_start_age$female_prev_own)
cor.test(nest_checks_start_age$female_age, nest_checks_start_age$female_prev_own)

#Add lay date
nest_checks_start$lay_day <- LH_eggs$day[match(nest_checks_start$box_year, LH_eggs$box_year)]
plot(nest_checks_start$start, nest_checks_start$lay_day)
cor.test(nest_checks_start$start, nest_checks_start$lay_day)

#NBDA 2013-2023 ----

#Data 2013

#Event data 
event_data13 <- nest_checks_start[nest_checks_start$year == 2013, c("BOX", "start")]
event_data13 <- nest_checks_start[nest_checks_start$year == 2013, c("BOX", "lay_day")]

#Rename columns for STBayes
event_data13 <- event_data13  %>% rename(id = BOX)
event_data13 <- event_data13  %>% rename(time = start)
event_data13 <- event_data13  %>% rename(time = lay_day)

event_data13 <- subset(event_data13, !is.na(event_data13$time))

#All boxes in the same trial
event_data13$trial <- 1

#Set end as last initiation + 1 
event_data13$t_end <- max(event_data13$time) + 1

#Simulating acquisition times
# Number of events
n <- nrow(event_data13)
set.seed(13)
library(truncnorm)

# Simulate truncated normal times
summary(event_data13$time)
sd(event_data13$time)

sim_times <- rtruncnorm(
  n = n,
  a = 72,
  b = 116,
  mean = 94,
  sd = 10   
)

# Round to create repeated values
sim_times <- round(sim_times)

# Assign to event_data13
event_data13_sim <- event_data13
event_data13_sim$time <- sim_times

#Permuting acquisition times
orig_times <- event_data13$time
set.seed(1)
event_data13_perm <- event_data13
event_data13_perm$time <- sample(orig_times, replace = FALSE)

#Edge list (start with spatial data)

#Spatial distance matrix
edge_list_spatial13 <- distance[, c("InputID", "TargetID", "Distance", "SiteComb")]

#Only consider distances within study sites
edge_list_spatial13 <- subset(edge_list_spatial13, edge_list_spatial13$SiteComb == "XX" | edge_list_spatial13$SiteComb == "YY" | edge_list_spatial13$SiteComb == "ZZ")

#Rename nodes
edge_list_spatial13 <- edge_list_spatial13 %>% rename(from = InputID)
edge_list_spatial13 <- edge_list_spatial13  %>% rename(to = TargetID)

#Filter edge list to include boxes in event_data
edge_list_spatial13 <- edge_list_spatial13 %>%
  filter(from %in% event_data13$id)

edge_list_spatial13 <- edge_list_spatial13 %>%
  filter(to %in% event_data13$id)

#Remove weaker spatial connections (more conservative)
median(edge_list_spatial13$Distance) #100 m
mean(edge_list_spatial13$Distance) #118 m
sd(edge_list_spatial13$Distance) #79 m

edge_list_spatial13 <- subset(edge_list_spatial13, Distance < 25)
edge_list_spatial13 <- subset(edge_list_spatial13, Distance < 50)
edge_list_spatial13 <- subset(edge_list_spatial13, Distance < 100)
edge_list_spatial13 <- subset(edge_list_spatial13, Distance < 200)

#Minimum and maximum distances between boxes
dmin <- min(edge_list_spatial13$Distance)
dmax <- max(edge_list_spatial13$Distance)

#Same trial 
edge_list_spatial13$trial <- 1

#Add spatial weight, standardised from 0 to 1
edge_list_spatial13$spatial_weight <- (dmax - edge_list_spatial13$Distance) / (dmax - dmin)

#Remove Distance and SiteComb
edge_list_spatial13 <- edge_list_spatial13[, c("from", "to", "trial", "spatial_weight")]

#Remove boxes from event data that are not in network
event_data13 <- subset(event_data13, !id == "X10" & !id == "X28" & !id == "X29"
                       & !id == "X34" & !id == "X37" & !id == "X40" & !id == "Y15"
                       & !id == "Z04" & !id == "Z34")

event_data13_sim <- subset(event_data13_sim, !id == "X10" & !id == "X28" & !id == "X29"
                       & !id == "X34" & !id == "X37" & !id == "X40" & !id == "Y15"
                       & !id == "Z04" & !id == "Z34")

#Permuting acquisition times
orig_times <- event_data13$time
set.seed(1)
event_data13_perm <- event_data13
event_data13_perm$time <- sample(orig_times, replace = FALSE)

event_data13_perm <- subset(event_data13_perm, !id == "X10" & !id == "X28" & !id == "X29"
                           & !id == "X34" & !id == "X37" & !id == "X40" & !id == "Y15"
                           & !id == "Z04" & !id == "Z34")

#If filtered <25 m, add edge 0 for Y14
edge_list_spatial13[169,] <- NA 
edge_list_spatial13$from[169] <- "Y14"
edge_list_spatial13$to[169] <- "Y01"
edge_list_spatial13$spatial_weight[169] <- 0
edge_list_spatial13$trial[169] <- 1

#Without transmission weights
data_list_social <- import_user_STb(
  event_data = event_data13,
  networks = edge_list_spatial13)


#Data 2014

#Event data 
event_data14 <- nest_checks_start[nest_checks_start$year == 2014, c("BOX", "start")]
event_data14 <- nest_checks_start[nest_checks_start$year == 2014, c("BOX", "lay_day")]

#Rename columns for STBayes
event_data14 <- event_data14  %>% rename(id = BOX)
event_data14 <- event_data14  %>% rename(time = start)
event_data14 <- event_data14  %>% rename(time = lay_day)

event_data14 <- subset(event_data14, !is.na(time))

#All boxes in the same trial
event_data14$trial <- 2

#Set end as last initiation + 1 
event_data14$t_end <- max(event_data14$time) + 1

#Simulating acquisition times
# Number of events
n <- nrow(event_data14)
set.seed(14)

# Simulate truncated normal times
summary(event_data14$time)
sd(event_data14$time)

sim_times <- rtruncnorm(
  n = n,
  a = 71,
  b = 113,
  mean = 86,
  sd = 13   
)

# Round to create repeated values
sim_times <- round(sim_times)

# Assign to event_data14
event_data14_sim <- event_data14
event_data14_sim$time <- sim_times

#Permuting acquisition times
orig_times <- event_data14$time
set.seed(2)
event_data14_perm <- event_data14
event_data14_perm$time <- sample(orig_times, replace = FALSE)

#Edge list (start with spatial data)

#Spatial distance matrix
edge_list_spatial14 <- distance[, c("InputID", "TargetID", "Distance", "SiteComb")]

#Only consider distances within study sites
edge_list_spatial14 <- subset(edge_list_spatial14, edge_list_spatial14$SiteComb == "XX" | edge_list_spatial14$SiteComb == "YY" | edge_list_spatial14$SiteComb == "ZZ")

#Rename nodes
edge_list_spatial14 <- edge_list_spatial14 %>% rename(from = InputID)
edge_list_spatial14 <- edge_list_spatial14  %>% rename(to = TargetID)

#Filter edge list to include boxes in event_data
edge_list_spatial14 <- edge_list_spatial14 %>%
  filter(from %in% event_data14$id)

edge_list_spatial14 <- edge_list_spatial14 %>%
  filter(to %in% event_data14$id)

#Remove weaker spatial connections (more conservative)
median(edge_list_spatial14$Distance) #100 m
mean(edge_list_spatial14$Distance) #118 m
sd(edge_list_spatial14$Distance) #79 m

edge_list_spatial14 <- subset(edge_list_spatial14, Distance < 25)
edge_list_spatial14 <- subset(edge_list_spatial14, Distance < 50)
edge_list_spatial14 <- subset(edge_list_spatial14, Distance < 100)
edge_list_spatial14 <- subset(edge_list_spatial14, Distance < 200)

#Minimum and maximum distances between boxes
dmin <- min(edge_list_spatial14$Distance)
dmax <- max(edge_list_spatial14$Distance)

#Same trial 
edge_list_spatial14$trial <- 2

#Add spatial weight, standardised from 0 to 1
edge_list_spatial14$spatial_weight <- (dmax - edge_list_spatial14$Distance) / (dmax - dmin)

#Remove Distance and SiteComb
edge_list_spatial14 <- edge_list_spatial14[, c("from", "to", "trial", "spatial_weight")]

#Remove boxes from event data that are not in network
event_data14 <- subset(event_data14, !id == "X01" & !id == "X09" & !id == "X15"
                       & !id == "X24" & !id == "X28" & !id == "X29" & !id == "X40" 
                       & !id == "X43" & !id == "X44" & !id == "X45" & !id == "Y13"
                       & !id == "Z04" & !id == "Z34" & !id == "Z36")

event_data14_sim <- subset(event_data14_sim, !id == "X01" & !id == "X09" & !id == "X15"
                       & !id == "X24" & !id == "X28" & !id == "X29" & !id == "X40" 
                       & !id == "X43" & !id == "X44" & !id == "X45" & !id == "Y13"
                       & !id == "Z04" & !id == "Z34" & !id == "Z36")

#Permuting acquisition times
orig_times <- event_data14$time
set.seed(2)
event_data14_perm <- event_data14
event_data14_perm$time <- sample(orig_times, replace = FALSE)

event_data14_perm <- subset(event_data14_perm, !id == "X01" & !id == "X09" & !id == "X15"
                           & !id == "X24" & !id == "X28" & !id == "X29" & !id == "X40" 
                           & !id == "X43" & !id == "X44" & !id == "X45" & !id == "Y13"
                           & !id == "Z04" & !id == "Z34" & !id == "Z36")

#If filtered <25 m, add edge 0 for some boxes
edge_list_spatial14[165,] <- NA 
edge_list_spatial14$from[165] <- "Y14"
edge_list_spatial14$to[165] <- "Y01"
edge_list_spatial14$spatial_weight[165] <- 0
edge_list_spatial14$trial[165] <- 2

edge_list_spatial14[166,] <- NA 
edge_list_spatial14$from[166] <- "Y31"
edge_list_spatial14$to[166] <- "Y01"
edge_list_spatial14$spatial_weight[166] <- 0
edge_list_spatial14$trial[166] <- 2

edge_list_spatial14[167,] <- NA 
edge_list_spatial14$from[167] <- "Y32"
edge_list_spatial14$to[167] <- "Y01"
edge_list_spatial14$spatial_weight[167] <- 0
edge_list_spatial14$trial[167] <- 2

edge_list_spatial14[168,] <- NA 
edge_list_spatial14$from[168] <- "Y33"
edge_list_spatial14$to[168] <- "Y01"
edge_list_spatial14$spatial_weight[168] <- 0
edge_list_spatial14$trial[168] <- 2

edge_list_spatial14[169,] <- NA 
edge_list_spatial14$from[169] <- "Z29"
edge_list_spatial14$to[169] <- "Y01"
edge_list_spatial14$spatial_weight[169] <- 0
edge_list_spatial14$trial[169] <- 2

#Without transmission weights
data_list_social <- import_user_STb(
  event_data = event_data14,
  networks = edge_list_spatial14)

#Data 2015

#Event data 
event_data15 <- nest_checks_start[nest_checks_start$year == 2015, c("BOX", "start")]
event_data15 <- nest_checks_start[nest_checks_start$year == 2015, c("BOX", "lay_day")]

#Rename columns for STBayes
event_data15 <- event_data15  %>% rename(id = BOX)
event_data15 <- event_data15  %>% rename(time = start)
event_data15 <- event_data15  %>% rename(time = lay_day)

event_data15 <- subset(event_data15, !is.na(time))

#All boxes in the same trial
event_data15$trial <- 3

#Set end as last initiation + 1 
event_data15 <- subset(event_data15, !is.na(time))
event_data15$t_end <- max(event_data15$time) + 1

#Simulating acquisition times
# Number of events
n <- nrow(event_data15)
set.seed(15)

# Simulate truncated normal times
summary(event_data15$time)
sd(event_data15$time)

sim_times <- rtruncnorm(
  n = n,
  a = 77,
  b = 108,
  mean = 87,
  sd = 9   
)

# Round to create repeated values
sim_times <- round(sim_times)

# Assign to event_data15
event_data15_sim <- event_data15
event_data15_sim$time <- sim_times

#Permuting acquisition times
orig_times <- event_data15$time
set.seed(151)
event_data15_perm <- event_data15
event_data15_perm$time <- sample(orig_times, replace = FALSE)

#Edge list (start with spatial data)

#Spatial distance matrix
edge_list_spatial15 <- distance[, c("InputID", "TargetID", "Distance", "SiteComb")]

#Only consider distances within study sites
edge_list_spatial15 <- subset(edge_list_spatial15, edge_list_spatial15$SiteComb == "XX" | edge_list_spatial15$SiteComb == "YY" | edge_list_spatial15$SiteComb == "ZZ")

#Rename nodes
edge_list_spatial15 <- edge_list_spatial15 %>% rename(from = InputID)
edge_list_spatial15 <- edge_list_spatial15  %>% rename(to = TargetID)

#Filter edge list to include boxes in event_data
edge_list_spatial15 <- edge_list_spatial15 %>%
  filter(from %in% event_data15$id)

edge_list_spatial15 <- edge_list_spatial15 %>%
  filter(to %in% event_data15$id)

#Remove weaker spatial connections (more conservative)
median(edge_list_spatial15$Distance) #100 m
mean(edge_list_spatial15$Distance) #118 m
sd(edge_list_spatial15$Distance) #79 m

edge_list_spatial15 <- subset(edge_list_spatial15, Distance < 25)
edge_list_spatial15 <- subset(edge_list_spatial15, Distance < 50)
edge_list_spatial15 <- subset(edge_list_spatial15, Distance < 100)
edge_list_spatial15 <- subset(edge_list_spatial15, Distance < 200)

#Minimum and maximum distances between boxes
dmin <- min(edge_list_spatial15$Distance)
dmax <- max(edge_list_spatial15$Distance)

#Same trial 
edge_list_spatial15$trial <- 3

#Add spatial weight, standardised from 0 to 1
edge_list_spatial15$spatial_weight <- (dmax - edge_list_spatial15$Distance) / (dmax - dmin)

#Remove Distance and SiteComb
edge_list_spatial15 <- edge_list_spatial15[, c("from", "to", "trial", "spatial_weight")]

#Remove boxes from event data that are not in network
event_data15 <- subset(event_data15, !id == "X09" & !id == "X40" & !id == "Z01"
                       & !id == "Z04" & !id == "Z37")

event_data15_sim <- subset(event_data15_sim, !id == "X09" & !id == "X40" & !id == "Z01"
                       & !id == "Z04" & !id == "Z37")

#Permuting acquisition times
orig_times <- event_data15$time
set.seed(3)
event_data15_perm <- event_data15
event_data15_perm$time <- sample(orig_times, replace = FALSE)

event_data15_perm <- subset(event_data15_perm, !id == "X09" & !id == "X40" & !id == "Z01"
                           & !id == "Z04" & !id == "Z37")

#If filtered <25 m, add edge 0 for some boxes
edge_list_spatial15[121,] <- NA 
edge_list_spatial15$from[121] <- "X27"
edge_list_spatial15$to[121] <- "Y01"
edge_list_spatial15$spatial_weight[121] <- 0
edge_list_spatial15$trial[121] <- 3

edge_list_spatial15[122,] <- NA 
edge_list_spatial15$from[122] <- "Y14"
edge_list_spatial15$to[122] <- "Y01"
edge_list_spatial15$spatial_weight[122] <- 0
edge_list_spatial15$trial[122] <- 3

edge_list_spatial15[123,] <- NA 
edge_list_spatial15$from[123] <- "Y23"
edge_list_spatial15$to[123] <- "Y01"
edge_list_spatial15$spatial_weight[123] <- 0
edge_list_spatial15$trial[123] <- 3

edge_list_spatial15[124,] <- NA 
edge_list_spatial15$from[124] <- "Y31"
edge_list_spatial15$to[124] <- "Y01"
edge_list_spatial15$spatial_weight[124] <- 0
edge_list_spatial15$trial[124] <- 3

edge_list_spatial15[125,] <- NA 
edge_list_spatial15$from[125] <- "Z03"
edge_list_spatial15$to[125] <- "Y01"
edge_list_spatial15$spatial_weight[125] <- 0
edge_list_spatial15$trial[125] <- 3

#Without transmission weights
data_list_social <- import_user_STb(
  event_data = event_data15,
  networks = edge_list_spatial15)


#Data 2016

#Event data 
event_data16 <- nest_checks_start[nest_checks_start$year == 2016, c("BOX", "start")]
event_data16 <- nest_checks_start[nest_checks_start$year == 2016, c("BOX", "lay_day")]

#Rename columns for STBayes
event_data16 <- event_data16  %>% rename(id = BOX)
event_data16 <- event_data16  %>% rename(time = start)
event_data16 <- event_data16  %>% rename(time = lay_day)

event_data16 <- subset(event_data16, !is.na(time))

#All boxes in the same trial
event_data16$trial <- 4

#Set end as last initiation + 1 
event_data16$t_end <- max(event_data16$time) + 1

n <- nrow(event_data16)
set.seed(16)

# Simulate truncated normal times
summary(event_data16$time)
sd(event_data16$time)

sim_times <- rtruncnorm(
  n = n,
  a = 74,
  b = 99,
  mean = 84,
  sd = 7   
)

# Round to create repeated values
sim_times <- round(sim_times)

# Assign to event_data16
event_data16_sim <- event_data16
event_data16_sim$time <- sim_times

orig_times <- event_data16$time
set.seed(161)
event_data16_perm <- event_data16
event_data16_perm$time <- sample(orig_times, replace = FALSE)

#Edge list (start with spatial data)

#Spatial distance matrix
edge_list_spatial16 <- distance[, c("InputID", "TargetID", "Distance", "SiteComb")]

#Only consider distances within study sites
edge_list_spatial16 <- subset(edge_list_spatial16, edge_list_spatial16$SiteComb == "XX" | edge_list_spatial16$SiteComb == "YY" | edge_list_spatial16$SiteComb == "ZZ")

#Rename nodes
edge_list_spatial16 <- edge_list_spatial16 %>% rename(from = InputID)
edge_list_spatial16 <- edge_list_spatial16  %>% rename(to = TargetID)

#Filter edge list to include boxes in event_data
edge_list_spatial16 <- edge_list_spatial16 %>%
  filter(from %in% event_data16$id)

edge_list_spatial16 <- edge_list_spatial16 %>%
  filter(to %in% event_data16$id)

#Remove weaker spatial connections (more conservative)
median(edge_list_spatial16$Distance) #100 m
mean(edge_list_spatial16$Distance) #118 m
sd(edge_list_spatial16$Distance) #79 m

edge_list_spatial16 <- subset(edge_list_spatial16, Distance < 25)
edge_list_spatial16 <- subset(edge_list_spatial16, Distance < 50)
edge_list_spatial16 <- subset(edge_list_spatial16, Distance < 100)
edge_list_spatial16 <- subset(edge_list_spatial16, Distance < 200)

#Minimum and maximum distances between boxes
dmin <- min(edge_list_spatial16$Distance)
dmax <- max(edge_list_spatial16$Distance)

#Same trial 
edge_list_spatial16$trial <- 4

#Add spatial weight, standardised from 0 to 1
edge_list_spatial16$spatial_weight <- (dmax - edge_list_spatial16$Distance) / (dmax - dmin)

#Remove Distance and SiteComb
edge_list_spatial16 <- edge_list_spatial16[, c("from", "to", "trial", "spatial_weight")]

#Remove boxes from event data that are not in network
event_data16 <- subset(event_data16, !id == "X01" & !id == "Z37")

event_data16_sim <- subset(event_data16_sim, !id == "X01" & !id == "Z37")

orig_times <- event_data16$time
set.seed(4)
event_data16_perm <- event_data16
event_data16_perm$time <- sample(orig_times, replace = FALSE)

event_data16_perm <- subset(event_data16_perm, !id == "X01" & !id == "Z37")

#If filtered <25 m, add edge 0 for some boxes
edge_list_spatial16[161,] <- NA 
edge_list_spatial16$from[161] <- "Y14"
edge_list_spatial16$to[161] <- "Y01"
edge_list_spatial16$spatial_weight[161] <- 0
edge_list_spatial16$trial[161] <- 4

edge_list_spatial16[162,] <- NA 
edge_list_spatial16$from[162] <- "Y23"
edge_list_spatial16$to[162] <- "Y01"
edge_list_spatial16$spatial_weight[162] <- 0
edge_list_spatial16$trial[162] <- 4

edge_list_spatial16[163,] <- NA 
edge_list_spatial16$from[163] <- "Y31"
edge_list_spatial16$to[163] <- "Y01"
edge_list_spatial16$spatial_weight[163] <- 0
edge_list_spatial16$trial[163] <- 4

edge_list_spatial16[164,] <- NA 
edge_list_spatial16$from[164] <- "Y32"
edge_list_spatial16$to[164] <- "Y01"
edge_list_spatial16$spatial_weight[164] <- 0
edge_list_spatial16$trial[164] <- 4

edge_list_spatial16[165,] <- NA 
edge_list_spatial16$from[165] <- "Z03"
edge_list_spatial16$to[165] <- "Y01"
edge_list_spatial16$spatial_weight[165] <- 0
edge_list_spatial16$trial[165] <- 4

#Without transmission weights
data_list_social <- import_user_STb(
  event_data = event_data16,
  networks = edge_list_spatial16)


#Data 2017

#Event data 
event_data17 <- nest_checks_start[nest_checks_start$year == 2017, c("BOX", "start")]
event_data17 <- nest_checks_start[nest_checks_start$year == 2017, c("BOX", "lay_day")]

#Rename columns for STBayes
event_data17 <- event_data17  %>% rename(id = BOX)
event_data17 <- event_data17  %>% rename(time = start)
event_data17 <- event_data17  %>% rename(time = lay_day)

event_data17 <- subset(event_data17, !is.na(time))

#All boxes in the same trial
event_data17$trial <- 5

#Set end as last initiation + 1 
event_data17$t_end <- max(event_data17$time) + 1

n <- nrow(event_data17)
set.seed(17)

# Simulate truncated normal times
summary(event_data17$time)
sd(event_data17$time)

sim_times <- rtruncnorm(
  n = n,
  a = 74,
  b = 95,
  mean = 83,
  sd = 6   
)

# Round to create repeated values
sim_times <- round(sim_times)

# Assign to event_data17
event_data17_sim <- event_data17
event_data17_sim$time <- sim_times

orig_times <- event_data17$time
set.seed(171)
event_data17_perm <- event_data17
event_data17_perm$time <- sample(orig_times, replace = FALSE)

#Edge list (start with spatial data)

#Spatial distance matrix
edge_list_spatial17 <- distance[, c("InputID", "TargetID", "Distance", "SiteComb")]

#Only consider distances within study sites
edge_list_spatial17 <- subset(edge_list_spatial17, edge_list_spatial17$SiteComb == "XX" | edge_list_spatial17$SiteComb == "YY" | edge_list_spatial17$SiteComb == "ZZ")

#Rename nodes
edge_list_spatial17 <- edge_list_spatial17 %>% rename(from = InputID)
edge_list_spatial17 <- edge_list_spatial17  %>% rename(to = TargetID)

#Filter edge list to include boxes in event_data
edge_list_spatial17 <- edge_list_spatial17 %>%
  filter(from %in% event_data17$id)

edge_list_spatial17 <- edge_list_spatial17 %>%
  filter(to %in% event_data17$id)

#Remove weaker spatial connections (more conservative)
median(edge_list_spatial17$Distance) #100 m
mean(edge_list_spatial17$Distance) #118 m
sd(edge_list_spatial17$Distance) #79 m

edge_list_spatial17 <- subset(edge_list_spatial17, Distance < 25)
edge_list_spatial17 <- subset(edge_list_spatial17, Distance < 50)
edge_list_spatial17 <- subset(edge_list_spatial17, Distance < 100)
edge_list_spatial17 <- subset(edge_list_spatial17, Distance < 200)

#Minimum and maximum distances between boxes
dmin <- min(edge_list_spatial17$Distance)
dmax <- max(edge_list_spatial17$Distance)

#Same trial 
edge_list_spatial17$trial <- 5

#Add spatial weight, standardised from 0 to 1
edge_list_spatial17$spatial_weight <- (dmax - edge_list_spatial17$Distance) / (dmax - dmin)

#Remove Distance and SiteComb
edge_list_spatial17 <- edge_list_spatial17[, c("from", "to", "trial", "spatial_weight")]

#Remove boxes from event data that are not in network
event_data17 <- subset(event_data17, !id == "X48" & !id == "Z37")

event_data17_sim <- subset(event_data17_sim, !id == "X48" & !id == "Z37")

orig_times <- event_data17$time
set.seed(5)
event_data17_perm <- event_data17
event_data17_perm$time <- sample(orig_times, replace = FALSE)

event_data17_perm <- subset(event_data17_perm, !id == "X48" & !id == "Z37")


#If filtered <25 m, add edge 0 for some boxes
edge_list_spatial17[107,] <- NA 
edge_list_spatial17$from[107] <- "Y12"
edge_list_spatial17$to[107] <- "Y01"
edge_list_spatial17$spatial_weight[107] <- 0
edge_list_spatial17$trial[107] <- 5

edge_list_spatial17[108,] <- NA 
edge_list_spatial17$from[108] <- "Y30"
edge_list_spatial17$to[108] <- "Y01"
edge_list_spatial17$spatial_weight[108] <- 0
edge_list_spatial17$trial[108] <- 5

edge_list_spatial17[109,] <- NA 
edge_list_spatial17$from[109] <- "Y31"
edge_list_spatial17$to[109] <- "Y01"
edge_list_spatial17$spatial_weight[109] <- 0
edge_list_spatial17$trial[109] <- 5

edge_list_spatial17[110,] <- NA 
edge_list_spatial17$from[110] <- "Y32"
edge_list_spatial17$to[110] <- "Y01"
edge_list_spatial17$spatial_weight[110] <- 0
edge_list_spatial17$trial[110] <- 5

edge_list_spatial17[111,] <- NA 
edge_list_spatial17$from[111] <- "Y33"
edge_list_spatial17$to[111] <- "Y01"
edge_list_spatial17$spatial_weight[111] <- 0
edge_list_spatial17$trial[111] <- 5

edge_list_spatial17[112,] <- NA 
edge_list_spatial17$from[112] <- "Z02"
edge_list_spatial17$to[112] <- "Y01"
edge_list_spatial17$spatial_weight[112] <- 0
edge_list_spatial17$trial[112] <- 5

edge_list_spatial17[113,] <- NA 
edge_list_spatial17$from[113] <- "Z03"
edge_list_spatial17$to[113] <- "Y01"
edge_list_spatial17$spatial_weight[113] <- 0
edge_list_spatial17$trial[113] <- 5

edge_list_spatial17[114,] <- NA 
edge_list_spatial17$from[114] <- "Z05"
edge_list_spatial17$to[114] <- "Y01"
edge_list_spatial17$spatial_weight[114] <- 0
edge_list_spatial17$trial[114] <- 5

edge_list_spatial17[115,] <- NA 
edge_list_spatial17$from[115] <- "Z10"
edge_list_spatial17$to[115] <- "Y01"
edge_list_spatial17$spatial_weight[115] <- 0
edge_list_spatial17$trial[115] <- 5

edge_list_spatial17[116,] <- NA 
edge_list_spatial17$from[116] <- "Y14"
edge_list_spatial17$to[116] <- "Y01"
edge_list_spatial17$spatial_weight[116] <- 0
edge_list_spatial17$trial[116] <- 5

#Without transmission weights
data_list_social <- import_user_STb(
  event_data = event_data17,
  networks = edge_list_spatial17)


#Data 2018

#Event data 
event_data18 <- nest_checks_start[nest_checks_start$year == 2018, c("BOX", "start")]
event_data18 <- nest_checks_start[nest_checks_start$year == 2018, c("BOX", "lay_day")]

#Rename columns for STBayes
event_data18 <- event_data18  %>% rename(id = BOX)
event_data18 <- event_data18  %>% rename(time = start)
event_data18 <- event_data18  %>% rename(time = lay_day)

event_data18 <- subset(event_data18, !is.na(time))

#All boxes in the same trial
event_data18$trial <- 6

#Set end as last initiation + 1 
event_data18$t_end <- max(event_data18$time) + 1

n <- nrow(event_data18)
set.seed(18)

# Simulate truncated normal times
summary(event_data18$time)
sd(event_data18$time)

sim_times <- rtruncnorm(
  n = n,
  a = 75,
  b = 95,
  mean = 83,
  sd = 5   
)

# Round to create repeated values
sim_times <- round(sim_times)

# Assign to event_data18
event_data18_sim <- event_data18
event_data18_sim$time <- sim_times

orig_times <- event_data18$time
set.seed(181)
event_data18_perm <- event_data18
event_data18_perm$time <- sample(orig_times, replace = FALSE)

#Edge list (start with spatial data)

#Spatial distance matrix
edge_list_spatial18 <- distance[, c("InputID", "TargetID", "Distance", "SiteComb")]

#Only consider distances within study sites
edge_list_spatial18 <- subset(edge_list_spatial18, edge_list_spatial18$SiteComb == "XX" | edge_list_spatial18$SiteComb == "YY" | edge_list_spatial18$SiteComb == "ZZ")

#Rename nodes
edge_list_spatial18 <- edge_list_spatial18 %>% rename(from = InputID)
edge_list_spatial18 <- edge_list_spatial18  %>% rename(to = TargetID)

#Filter edge list to include boxes in event_data
edge_list_spatial18 <- edge_list_spatial18 %>%
  filter(from %in% event_data18$id)

edge_list_spatial18 <- edge_list_spatial18 %>%
  filter(to %in% event_data18$id)

#Remove weaker spatial connections (more conservative)
median(edge_list_spatial18$Distance) #100 m
mean(edge_list_spatial18$Distance) #118 m
sd(edge_list_spatial18$Distance) #79 m

edge_list_spatial18 <- subset(edge_list_spatial18, Distance < 25)
edge_list_spatial18 <- subset(edge_list_spatial18, Distance < 50)
edge_list_spatial18 <- subset(edge_list_spatial18, Distance < 100)
edge_list_spatial18 <- subset(edge_list_spatial18, Distance < 200)

#Minimum and maximum distances between boxes
dmin <- min(edge_list_spatial18$Distance)
dmax <- max(edge_list_spatial18$Distance)

#Same trial 
edge_list_spatial18$trial <- 6

#Add spatial weight, standardised from 0 to 1
edge_list_spatial18$spatial_weight <- (dmax - edge_list_spatial18$Distance) / (dmax - dmin)

#Remove Distance and SiteComb
edge_list_spatial18 <- edge_list_spatial18[, c("from", "to", "trial", "spatial_weight")]

#Remove boxes from event data that are not in network
event_data18 <- subset(event_data18, !id == "Z37")

event_data18_sim <- subset(event_data18_sim, !id == "Z37")

orig_times <- event_data18$time
set.seed(6)
event_data18_perm <- event_data18
event_data18_perm$time <- sample(orig_times, replace = FALSE)

event_data18_perm <- subset(event_data18_perm, !id == "Z37")

#If edge list filtered 
edge_list_spatial18[79,] <- NA 
edge_list_spatial18$from[79] <- "Y12"
edge_list_spatial18$to[79] <- "Y01"
edge_list_spatial18$spatial_weight[79] <- 0
edge_list_spatial18$trial[79] <- 6

edge_list_spatial18[80,] <- NA 
edge_list_spatial18$from[80] <- "Y19"
edge_list_spatial18$to[80] <- "Y01"
edge_list_spatial18$spatial_weight[80] <- 0
edge_list_spatial18$trial[80] <- 6

edge_list_spatial18[81,] <- NA 
edge_list_spatial18$from[81] <- "Y23"
edge_list_spatial18$to[81] <- "Y01"
edge_list_spatial18$spatial_weight[81] <- 0
edge_list_spatial18$trial[81] <- 6

edge_list_spatial18[82,] <- NA 
edge_list_spatial18$from[82] <- "Y31"
edge_list_spatial18$to[82] <- "Y01"
edge_list_spatial18$spatial_weight[82] <- 0
edge_list_spatial18$trial[82] <- 6

edge_list_spatial18[83,] <- NA 
edge_list_spatial18$from[83] <- "Z03"
edge_list_spatial18$to[83] <- "Y01"
edge_list_spatial18$spatial_weight[83] <- 0
edge_list_spatial18$trial[83] <- 6

edge_list_spatial18[84,] <- NA 
edge_list_spatial18$from[84] <- "Z28"
edge_list_spatial18$to[84] <- "Y01"
edge_list_spatial18$spatial_weight[84] <- 0
edge_list_spatial18$trial[84] <- 6

#Without transmission weights
data_list_social <- import_user_STb(
  event_data = event_data18,
  networks = edge_list_spatial18)

#Data 2019

#Event data 
event_data19 <- nest_checks_start[nest_checks_start$year == 2019, c("BOX", "start")]

#Rename columns for STBayes
event_data19 <- event_data19  %>% rename(id = BOX)
event_data19 <- event_data19  %>% rename(time = start)

#All boxes in the same trial
event_data19$trial <- 7

#Set end as last initiation + 1 
event_data19$t_end <- max(event_data19$time) + 1

#Edge list (start with spatial data)

#Spatial distance matrix
edge_list_spatial19 <- distance[, c("InputID", "TargetID", "Distance", "SiteComb")]

#Only consider distances within study sites
edge_list_spatial19 <- subset(edge_list_spatial19, edge_list_spatial19$SiteComb == "XX" | edge_list_spatial19$SiteComb == "YY" | edge_list_spatial19$SiteComb == "ZZ")

#Remove weaker spatial connections (more conservative)
median(edge_list_spatial19$Distance) #100 m
mean(edge_list_spatial19$Distance) #119 m
sd(edge_list_spatial19$Distance) #79 m

edge_list_spatial19 <- subset(edge_list_spatial19, Distance < 50)
edge_list_spatial19 <- subset(edge_list_spatial19, Distance < 100)
edge_list_spatial19 <- subset(edge_list_spatial19, Distance < 200)

#Rename nodes
edge_list_spatial19 <- edge_list_spatial19 %>% rename(from = InputID)
edge_list_spatial19 <- edge_list_spatial19  %>% rename(to = TargetID)

#Filter edge list to include boxes in event_data
edge_list_spatial19 <- edge_list_spatial19 %>%
  filter(from %in% event_data19$id)

edge_list_spatial19 <- edge_list_spatial19 %>%
  filter(to %in% event_data19$id)

#Minimum and maximum distances between boxes
dmin <- min(edge_list_spatial19$Distance)
dmax <- max(edge_list_spatial19$Distance)

#Same trial 
edge_list_spatial19$trial <- 7

#Add spatial weight, standardised from 0 to 1
edge_list_spatial19$spatial_weight <- (dmax - edge_list_spatial19$Distance) / (dmax - dmin)

#Remove Distance and SiteComb
edge_list_spatial19 <- edge_list_spatial19[, c("from", "to", "trial", "spatial_weight")]

#Remove boxes from event data that are not in network
event_data19 <- subset(event_data19, !id == "X01" & !id == "X21" & !id == "Z37")

#Without transmission weights
data_list_social <- import_user_STb(
  event_data = event_data19,
  networks = edge_list_spatial19)


#Data 2021

#Event data 
event_data21 <- nest_checks_start[nest_checks_start$year == 2021, c("BOX", "start")]
event_data21 <- nest_checks_start[nest_checks_start$year == 2021, c("BOX", "lay_day")]

#Rename columns for STBayes
event_data21 <- event_data21  %>% rename(id = BOX)
event_data21 <- event_data21  %>% rename(time = start)
event_data21 <- event_data21  %>% rename(time = lay_day)

event_data21 <- subset(event_data21, !is.na(time))
  
#All boxes in the same trial
event_data21$trial <- 8

#Set end as last initiation + 1 
event_data21$t_end <- max(event_data21$time) + 1

n <- nrow(event_data21)
set.seed(21)

# Simulate truncated normal times
summary(event_data21$time)
sd(event_data21$time)

sim_times <- rtruncnorm(
  n = n,
  a = 61,
  b = 95,
  mean = 74,
  sd = 16   
)

# Round to create repeated values
sim_times <- round(sim_times)

# Assign to event_data21
event_data21_sim <- event_data21
event_data21_sim$time <- sim_times

orig_times <- event_data21$time
set.seed(211)
event_data21_perm <- event_data21
event_data21_perm$time <- sample(orig_times, replace = FALSE)

#Edge list (start with spatial data)

#Spatial distance matrix
edge_list_spatial21 <- distance[, c("InputID", "TargetID", "Distance", "SiteComb")]

#Only consider distances within study sites
edge_list_spatial21 <- subset(edge_list_spatial21, edge_list_spatial21$SiteComb == "XX" | edge_list_spatial21$SiteComb == "YY" | edge_list_spatial21$SiteComb == "ZZ")

#Rename nodes
edge_list_spatial21 <- edge_list_spatial21 %>% rename(from = InputID)
edge_list_spatial21 <- edge_list_spatial21  %>% rename(to = TargetID)

#Filter edge list to include boxes in event_data
edge_list_spatial21 <- edge_list_spatial21 %>%
  filter(from %in% event_data21$id)

edge_list_spatial21 <- edge_list_spatial21 %>%
  filter(to %in% event_data21$id)

#Remove weaker spatial connections (more conservative)
median(edge_list_spatial21$Distance) #100 m
mean(edge_list_spatial21$Distance) #121 m
sd(edge_list_spatial21$Distance) #79 m

edge_list_spatial21 <- subset(edge_list_spatial21, Distance < 25)
edge_list_spatial21 <- subset(edge_list_spatial21, Distance < 50)
edge_list_spatial21 <- subset(edge_list_spatial21, Distance < 100)
edge_list_spatial21 <- subset(edge_list_spatial21, Distance < 200)

#Minimum and maximum distances between boxes
dmin <- min(edge_list_spatial21$Distance)
dmax <- max(edge_list_spatial21$Distance)

#Same trial 
edge_list_spatial21$trial <- 8

#Add spatial weight, standardised from 0 to 1
edge_list_spatial21$spatial_weight <- (dmax - edge_list_spatial21$Distance) / (dmax - dmin)

#Remove Distance and SiteComb
edge_list_spatial21 <- edge_list_spatial21[, c("from", "to", "trial", "spatial_weight")]

orig_times <- event_data21$time
set.seed(211)
event_data21_perm <- event_data21
event_data21_perm$time <- sample(orig_times, replace = FALSE)

#If filtered <100 m, add edge 0 for Y33
edge_list_spatial21[35,] <- NA 
edge_list_spatial21$from[35] <- "Y33"
edge_list_spatial21$to[35] <- "Y04"
edge_list_spatial21$spatial_weight[35] <- 0
edge_list_spatial21$trial[35] <- 8

#If filtered <50 m, add edge 0 for Y18, Y23, Y33, Z32
edge_list_spatial21[17,] <- NA 
edge_list_spatial21$from[17] <- "Y18"
edge_list_spatial21$to[17] <- "Y04"
edge_list_spatial21$spatial_weight[17] <- 0
edge_list_spatial21$trial[17] <- 8

edge_list_spatial21[18,] <- NA 
edge_list_spatial21$from[18] <- "Y23"
edge_list_spatial21$to[18] <- "Y04"
edge_list_spatial21$spatial_weight[18] <- 0
edge_list_spatial21$trial[18] <- 8

edge_list_spatial21[19,] <- NA 
edge_list_spatial21$from[19] <- "Y33"
edge_list_spatial21$to[19] <- "Y04"
edge_list_spatial21$spatial_weight[19] <- 0
edge_list_spatial21$trial[19] <- 8

edge_list_spatial21[20,] <- NA 
edge_list_spatial21$from[20] <- "Z32"
edge_list_spatial21$to[20] <- "Y04"
edge_list_spatial21$spatial_weight[20] <- 0
edge_list_spatial21$trial[20] <- 8

#If filtered <25 m, add edge 0 for X32, X36, Y18, Y23, Y33, Z32
edge_list_spatial21[13,] <- NA 
edge_list_spatial21$from[13] <- "X32"
edge_list_spatial21$to[13] <- "Y04"
edge_list_spatial21$spatial_weight[13] <- 0
edge_list_spatial21$trial[13] <- 8

edge_list_spatial21[14,] <- NA 
edge_list_spatial21$from[14] <- "X36"
edge_list_spatial21$to[14] <- "Y04"
edge_list_spatial21$spatial_weight[14] <- 0
edge_list_spatial21$trial[14] <- 8

edge_list_spatial21[15,] <- NA 
edge_list_spatial21$from[15] <- "Y33"
edge_list_spatial21$to[15] <- "Y04"
edge_list_spatial21$spatial_weight[15] <- 0
edge_list_spatial21$trial[15] <- 8

edge_list_spatial21[16,] <- NA 
edge_list_spatial21$from[16] <- "Z32"
edge_list_spatial21$to[16] <- "Y04"
edge_list_spatial21$spatial_weight[16] <- 0
edge_list_spatial21$trial[16] <- 8

edge_list_spatial21[17,] <- NA 
edge_list_spatial21$from[17] <- "Y18"
edge_list_spatial21$to[17] <- "Y04"
edge_list_spatial21$spatial_weight[17] <- 0
edge_list_spatial21$trial[17] <- 8

edge_list_spatial21[18,] <- NA 
edge_list_spatial21$from[18] <- "Y23"
edge_list_spatial21$to[18] <- "Y04"
edge_list_spatial21$spatial_weight[18] <- 0
edge_list_spatial21$trial[18] <- 8

#Without transmission weights
data_list_social <- import_user_STb(
  event_data = event_data21,
  networks = edge_list_spatial21)


#Data 2023

#Event data 
event_data23 <- nest_checks_start[nest_checks_start$year == 2023, c("BOX", "start")]

#Rename columns for STBayes
event_data23 <- event_data23  %>% rename(id = BOX)
event_data23 <- event_data23  %>% rename(time = start)

#All boxes in the same trial
event_data23$trial <- 9

#Set end as last initiation + 1 
event_data23$t_end <- max(event_data23$time) + 1

#Edge list (start with spatial data)

#Spatial distance matrix
edge_list_spatial23 <- distance[, c("InputID", "TargetID", "Distance", "SiteComb")]

#Only consider distances within study sites
edge_list_spatial23 <- subset(edge_list_spatial23, edge_list_spatial23$SiteComb == "XX" | edge_list_spatial23$SiteComb == "YY" | edge_list_spatial23$SiteComb == "ZZ")

#Remove weaker spatial connections (more conservative)
median(edge_list_spatial23$Distance) #100 m
mean(edge_list_spatial23$Distance) #123 m
sd(edge_list_spatial23$Distance) #79 m

edge_list_spatial23 <- subset(edge_list_spatial23, Distance < 100)
edge_list_spatial23 <- subset(edge_list_spatial23, Distance < 200)

#Rename nodes
edge_list_spatial23 <- edge_list_spatial23 %>% rename(from = InputID)
edge_list_spatial23 <- edge_list_spatial23  %>% rename(to = TargetID)

#Filter edge list to include boxes in event_data
edge_list_spatial23 <- edge_list_spatial23 %>%
  filter(from %in% event_data23$id)

edge_list_spatial23 <- edge_list_spatial23 %>%
  filter(to %in% event_data23$id)

#Minimum and maximum distances between boxes
dmin <- min(edge_list_spatial23$Distance)
dmax <- max(edge_list_spatial23$Distance)

#Same trial 
edge_list_spatial23$trial <- 9

#Add spatial weight, standardised from 0 to 1
edge_list_spatial23$spatial_weight <- (dmax - edge_list_spatial23$Distance) / (dmax - dmin)

#Remove Distance and SiteComb
edge_list_spatial23 <- edge_list_spatial23[, c("from", "to", "trial", "spatial_weight")]

#Without transmission weights
data_list_social <- import_user_STb(
  event_data = event_data23,
  networks = edge_list_spatial23)

#Combined data across years
event_data <- rbind(event_data13, event_data14, event_data15, event_data16, event_data17, event_data18, event_data19, event_data21, event_data23)
edge_list <- rbind(edge_list_spatial13, edge_list_spatial14, edge_list_spatial15, edge_list_spatial16, edge_list_spatial17, edge_list_spatial18, edge_list_spatial19, edge_list_spatial21, edge_list_spatial23)

edge_list1323 <- edge_list

#Without 2019, 2020, 2022, and 2023 (later start of nest checks)
event_data <- rbind(event_data13, event_data14, event_data15, event_data16, event_data17, event_data18, event_data21)
edge_list <- rbind(edge_list_spatial13, edge_list_spatial14, edge_list_spatial15, edge_list_spatial16, edge_list_spatial17, edge_list_spatial18, edge_list_spatial21)

event_data1321 <- event_data
edge_list1321 <- edge_list

#Without 2019, 2020, 2022, and 2023 but with 2024
event_data <- rbind(event_data13, event_data14, event_data15, event_data16, event_data17, event_data18, event_data21, event_data24)
edge_list <- rbind(edge_list_spatial13, edge_list_spatial14, edge_list_spatial15, edge_list_spatial16, edge_list_spatial17, edge_list_spatial18, edge_list_spatial21, edge_list_spatial24)

#Simulated data 2013 - 2021 (acquisition times)
event_data_sim <- rbind(event_data13_sim, event_data14_sim, event_data15_sim, event_data16_sim, event_data17_sim, event_data18_sim, event_data21_sim)
edge_list_sim <- rbind(edge_list_spatial13, edge_list_spatial14, edge_list_spatial15, edge_list_spatial16, edge_list_spatial17, edge_list_spatial18, edge_list_spatial21)

#Permuted data 2013 - 2021 (acquisition times)
event_data_perm <- rbind(event_data13_perm, event_data14_perm, event_data15_perm, event_data16_perm, event_data17_perm, event_data18_perm, event_data21_perm)
edge_list_perm <- rbind(edge_list_spatial13, edge_list_spatial14, edge_list_spatial15, edge_list_spatial16, edge_list_spatial17, edge_list_spatial18, edge_list_spatial21)

#Permuted data 2013 - 2021 (network)
edge_list$site <- substr(edge_list$from, 1,1)

edge_list_perm_function <- function(edge_list) {
  
  # split by site × trial
  groups <- split(edge_list, list(edge_list$site, edge_list$trial), drop = TRUE)
  
  permuted_groups <- lapply(groups, function(g) {
    
    nodes <- unique(c(g$from, g$to))
    perm_nodes <- sample(nodes)
    lookup <- setNames(perm_nodes, nodes)
    
    g$from <- lookup[g$from]
    g$to   <- lookup[g$to]
    
    # leave g$weight as-is (Option A)
    return(g)
  })
  
  permuted <- do.call(rbind, permuted_groups)
  rownames(permuted) <- NULL
  return(permuted)
}

edge_list_perm <- edge_list_perm_function(edge_list)
edge_list_perm$site <- NULL

event_data_perm <- rbind(event_data13, event_data14, event_data15, event_data16, event_data17, event_data18, event_data21)


#Add age as ILV
ILV <- event_data

ILV <- ILV %>% 
  mutate(year = case_when(
    trial == 1 ~ 2013,
    trial == 2 ~ 2014,
    trial == 3 ~ 2015,
    trial == 4 ~ 2016,
    trial == 5 ~ 2017,
    trial == 6 ~ 2018,
    trial == 8 ~ 2021))
    
ILV$box_year <- paste(ILV$id, ILV$year, sep = "_")

ILV$female_JID <- LH_owners_females$ID[match(ILV$box_year, LH_owners_females$box_year)]
ILV$male_JID <- LH_owners_males$ID[match(ILV$box_year, LH_owners_males$box_year)]

ILV$female_age_24 <- Ringed$min_age[match(ILV$female_JID, Ringed$ID)]
ILV$male_age_24 <- Ringed$min_age[match(ILV$male_JID, Ringed$ID)]

ILV$female_age <- ILV$female_age_24 - (2024 - ILV$year)
ILV$male_age <- ILV$male_age_24 - (2024 - ILV$year)

sum(is.na(ILV$female_JID))
sum(is.na(ILV$male_JID))

ILV <- ILV %>% 
  mutate(pair_age = case_when(
    is.na(female_age) & !is.na(male_age) ~ male_age,
    !is.na(female_age) & is.na(male_age) ~ female_age,
    !is.na(female_age) & !is.na(male_age) ~ (female_age + male_age) /2,
    is.na(female_age) & is.na(male_age) ~ NA))

ILV$pair_ID <- paste(ILV$female_JID, ILV$male_JID, sep = "_")

ILV_mean_pair_age <- data.table(tapply(ILV$pair_age, ILV$year, FUN = mean, na.rm = TRUE))
ILV_mean_pair_age <- ILV_mean_pair_age %>% rename(pair_age = V1)
ILV_mean_pair_age$year <- c("2013", "2014", "2015", "2016", "2017", "2018", "2021")

ILV$pair_age <- ifelse(is.na(ILV$pair_age), ILV_mean_pair_age$pair_age[match(ILV$year, ILV_mean_pair_age$year)], ILV$pair_age)
ILV$pair_age_z <- scale(ILV$pair_age)

ILV <- ILV[, c("id", "trial", "pair_age_z")]

ILV2 <- subset(ILV, !is.na(pair_age_z))

ILV2 <- ILV2 %>%
  group_by(id) %>%
  mutate(mean_pair_age_z = mean(pair_age_z))

ILV2 <- ILV2  %>%
  group_by(id) %>% 
  slice(1:1)

event_data$pair_age_z <- ILV$pair_age_z

lm <- lmer(time ~ pair_age + (1|trial) + (1|id), data = event_data)
summary(lm)
Anova(lm)

#Subset of data where pair age is NA?
event_data2 <- subset(event_data, !is.na(pair_age_z))
event_data2$id_trial <- paste(event_data2$id, event_data2$trial, sep = "_")

edge_list2 <- edge_list 
edge_list2$from_trial <- paste(edge_list$from, edge_list$trial, sep = "_")
edge_list2$to_trial <- paste(edge_list$to, edge_list$trial, sep = "_")

edge_list2 <- edge_list2 %>%
  filter(from_trial %in% event_data2$id_trial)

edge_list2 <- edge_list2 %>%
  filter(to_trial %in% event_data2$id_trial)

event_data2 <- event_data2[, c("id", "time", "trial", "t_end")]
edge_list2 <- edge_list2[, c("from", "to", "trial", "spatial_weight")]

#Permuting acquisition times
orig_times <- event_data$time
set.seed(999)
event_data_perm <- event_data
event_data_perm$time <- sample(orig_times, replace = FALSE)

set.seed(1)
event_data$site <- substr(event_data$id,1,1)
event_data_perm <- event_data %>%
  group_by(site, trial) %>%
  mutate(time = sample(time, replace = FALSE)) %>%
  ungroup()
event_data$site <- NULL
event_data_perm$site <- NULL

event_data %>%
  group_by(site, trial) %>%
  summarise(mean_time = mean(time)) -> orig_summary

event_data_perm %>%
  group_by(site, trial) %>%
  summarise(mean_time = mean(time)) -> perm_summary

all.equal(orig_summary$mean_time, perm_summary$mean_time)

edge_list_perm <- edge_list

#Simulating acquisition times
# Number of events
n <- nrow(event_data)
set.seed(123)
install.packages("truncnorm")
library(truncnorm)

# Simulate truncated normal times
sim_times <- rtruncnorm(
  n = n,
  a = 60,
  b = 120,
  mean = 85,
  sd = 10   
)

# Round to create repeated values
sim_times <- round(sim_times)

# Assign to event_data
event_data_sim <- event_data
event_data_sim$time <- sim_times

# Quick checks
range(event_data_sim$time)
mean(event_data_sim$time)

#Create data list for NBDA

#Code uses event_data and edge_list, so event_data1321 and edge_list1321 need to be renamed here 
event_data <- event_data1321
edge_list <- edge_list1321

#Social model without ILV
data_list_social <- import_user_STb(
  event_data = event_data,
  networks = edge_list)

data_list_perm <- import_user_STb(
  event_data = event_data_perm,
  networks = edge_list_perm)

data_list_sim <- import_user_STb(
  event_data = event_data_sim,
  networks = edge_list_sim)

#Social model with ILV
data_list_social_ILV <- import_user_STb(
  event_data = event_data2,
  networks = edge_list2,
  ILV_c = ILV2,
  ILVi = c("mean_pair_age_z"),
  ILVs = c("mean_pair_age_z"),
  ILVm = c("mean_pair_age_z")
)

#Generate social models with and without ILV
model_social <- generate_STb_model(data_list_social, 
                                   gq = T, 
                                   est_acqTime = F, 
                                   data_type = c("discrete_time"))

model_social_perm <- generate_STb_model(data_list_perm, 
                                   gq = T, 
                                   est_acqTime = F, 
                                   data_type = c("discrete_time"))

model_social_sim <- generate_STb_model(data_list_sim, 
                                        gq = T, 
                                        est_acqTime = F, 
                                        data_type = c("discrete_time"))

model_social_ILV <- generate_STb_model(data_list_social_ILV, 
                                       gq = T, 
                                       est_acqTime = F, 
                                       data_type = c("discrete_time"))

#Fit social model
model_social_fit <- fit_STb(data_list_social,
                            model_social,
                            parallel_chains = 4,
                            chains = 4,
                            cores = 4,
                            iter = 4000,
                            refresh=1000
)

model_social_perm_fit <- fit_STb(data_list_perm,
                            model_social_perm,
                            parallel_chains = 4,
                            chains = 4,
                            cores = 4,
                            iter = 4000,
                            refresh=1000
)

model_social_sim_fit <- fit_STb(data_list_sim,
                                 model_social_sim,
                                 parallel_chains = 4,
                                 chains = 4,
                                 cores = 4,
                                 iter = 4000,
                                 refresh=1000
)

#Fit social model with ILV
model_social_ILV_fit <- fit_STb(data_list_social_ILV,
                                model_social_ILV,
                                parallel_chains = 4,
                                chains = 4,
                                cores = 4,
                                iter = 8000,
                                refresh=1000
)

#Save social models
STb_save(model_social_fit, output_dir = "cmdstan_saves", name="model_social_fit")
STb_save(model_social_ILV_fit, output_dir = "cmdstan_saves", name="model_social_ILV_fit")

#Summary for social models 
STb_summary(model_social_fit, digits = 3)
STb_summary(model_social_perm_fit, digits = 3)
STb_summary(model_social_sim_fit, digits = 3)
STb_summary(model_social_ILV_fit, digits = 3)

#Generate asocial models
model_asocial = generate_STb_model(data_list_social, 
                                   model_type="asocial",
                                   gq = T, 
                                   est_acqTime = F, 
                                   data_type = c("discrete_time")
                                   )
model_asocial_perm = generate_STb_model(data_list_perm, model_type="asocial")
model_asocial_sim = generate_STb_model(data_list_sim, model_type="asocial")
model_asocial_ILV = generate_STb_model(data_list_social_ILV, model_type="asocial")

#Run asocial models
model_asocial_fit = fit_STb(data_list_social,
                            model_asocial,
                            parallel_chains = 4,
                            chains = 4,
                            cores = 4,
                            iter = 4000,
                            refresh=1000)

model_asocial_perm_fit = fit_STb(data_list_perm,
                            model_asocial_perm,
                            parallel_chains = 4,
                            chains = 4,
                            cores = 4,
                            iter = 4000,
                            refresh=1000)

model_asocial_sim_fit = fit_STb(data_list_sim,
                                 model_asocial_sim,
                                 parallel_chains = 4,
                                 chains = 4,
                                 cores = 4,
                                 iter = 4000,
                                 refresh=1000)

model_asocial_ILV_fit = fit_STb(data_list_social_ILV,
                                model_asocial_ILV,
                                parallel_chains = 4,
                                chains = 4,
                                cores = 4,
                                iter = 4000,
                                refresh=1000)

#Summary for asocial models
STb_summary(model_asocial_fit, digits = 3)
STb_summary(model_asocial_ILV_fit, digits = 3)

#Compare models via LOO cross validation

#All four models
loo_output = STb_compare(model_social_fit, model_asocial_fit, model_social_ILV_fit, model_asocial_ILV_fit, method="loo-psis")

#Just two models without ILV
loo_output = STb_compare(model_social_fit, model_asocial_fit, method="loo-psis")
loo_output = STb_compare(model_social_perm_fit, model_asocial_perm_fit, method="loo-psis")
loo_output = STb_compare(model_social_sim_fit, model_asocial_sim_fit, method="loo-psis")
loo_output = STb_compare(model_social_fit, model_social_perm_fit, model_social_sim_fit, method="loo-psis")
loo_output = STb_compare(model_social_fit, model_asocial_fit, model_social_sim_fit, model_asocial_sim_fit, method="loo-psis")
loo_output = STb_compare(model_social_sim_fit, model_asocial_sim_fit, method="loo-psis")
loo_output = STb_compare(model_asocial_fit, model_asocial_sim_fit, method="loo-psis")
loo_output = STb_compare(model_social_perm_fit, model_social_sim_fit, method="loo-psis")
loo_output = STb_compare(model_social_fit, model_social_perm_fit, method="loo-psis")

#Print LOO output
print(loo_output$comparison, simplify = FALSE)

#Permuted data 2013 - 2021 (network) ----

#Fit real model first
edge_list$site <- NULL

data_list_social <- import_user_STb(
  event_data = event_data,
  networks = edge_list)

model_social <- generate_STb_model(data_list_social, 
                                   gq = T, 
                                   est_acqTime = F, 
                                   data_type = c("discrete_time"))

model_social_fit <- fit_STb(data_list_social,
                            model_social,
                            parallel_chains = 4,
                            chains = 4,
                            cores = 4,
                            iter = 4000,
                            refresh=1000
)

STb_summary(model_social_fit)


#Prepare permuted data

edge_list$site <- substr(edge_list$from, 1,1)

edge_list_perm_function <- function(edge_list) {
  
  # split by site × trial
  groups <- split(edge_list, list(edge_list$site, edge_list$trial), drop = TRUE)
  
  permuted_groups <- lapply(groups, function(g) {
    
    nodes <- unique(c(g$from, g$to))
    perm_nodes <- sample(nodes)
    lookup <- setNames(perm_nodes, nodes)
    
    g$from <- lookup[g$from]
    g$to   <- lookup[g$to]
    
    # leave g$weight as-is (Option A)
    return(g)
  })
  
  permuted <- do.call(rbind, permuted_groups)
  rownames(permuted) <- NULL
  return(permuted)
}

edge_list_perm <- edge_list_perm_function(edge_list)
edge_list_perm$site <- NULL

event_data_perm <- rbind(event_data13, event_data14, event_data15, event_data16, event_data17, event_data18, event_data21)

# Number of permutations
n_perm <- 100

# Store results
perm_results <- vector("list", n_perm)

for (i in 1:n_perm) {
  
  cat("Permutation:", i, "\n")
  
  # 1. Create permuted network
  edge_list_perm <- edge_list_perm_function(edge_list)
  
  edge_list_perm$site <- NULL
  
  # 2. Import permuted NBDA data
  data_list_perm <- import_user_STb(
    event_data = event_data_perm,
    networks   = edge_list_perm
  )
  
  # 3. Generate social model
  model_social_perm <- generate_STb_model(
    data_list_perm,
    gq = TRUE,
    est_acqTime = FALSE,
    data_type = "discrete_time"
  )
  
  # 4. Fit model (adjust chains/iterations if needed)
  model_social_perm_fit <- fit_STb(
    data_list_perm,
    model_social_perm,
    parallel_chains = 4,
    chains = 4,
    cores = 4,
    iter = 4000,
    refresh = 1000
  )
  
  # 5. Extract summary statistics
  summary_perm <- STb_summary(model_social_perm_fit, digits = 3)
  
  # 6. Save key results (social transmission parameter 's' and log-likelihood)
  perm_results[[i]] <- list(
    s_Median      = summary_perm[summary_perm$Parameter=="s", "Median"],
    s_MAD         = summary_perm[summary_perm$Parameter=="s", "MAD"],
    s_CI_Lower    = summary_perm[summary_perm$Parameter=="s", "CI_Lower"],
    s_CI_Upper    = summary_perm[summary_perm$Parameter=="s", "CI_Upper"],
    
    percentST_Median   = summary_perm[summary_perm$Parameter=="percent_ST[1]", "Median"],
    percentST_MAD      = summary_perm[summary_perm$Parameter=="percent_ST[1]", "MAD"],
    percentST_CI_Lower = summary_perm[summary_perm$Parameter=="percent_ST[1]", "CI_Lower"],
    percentST_CI_Upper = summary_perm[summary_perm$Parameter=="percent_ST[1]", "CI_Upper"]
  )  
}

perm_summary_df <- do.call(rbind, lapply(perm_results, as.data.frame))

quantile(perm_summary_df$s_Median, 0.9999)



#Bayesian simulations ----

event_data <- rbind(event_data13, event_data14, event_data15, event_data16, event_data17, event_data18, event_data21)
edge_list <- rbind(edge_list_spatial13, edge_list_spatial14, edge_list_spatial15, edge_list_spatial16, edge_list_spatial17, edge_list_spatial18, edge_list_spatial21)

sdata_list_social <- import_user_STb(
  event_data = event_data,
  networks = edge_list)

model_social <- generate_STb_model(data_list_social, 
                                   gq = T, 
                                   est_acqTime = F, 
                                   data_type = c("discrete_time"))

model_asocial = generate_STb_model(data_list_social,
                                   gq = T, 
                                   est_acqTime = F, 
                                   data_type = c("discrete_time"),
                                   model_type="asocial")

model_social_fit <- fit_STb(data_list_social,
                            model_social,
                            parallel_chains = 4,
                            chains = 4,
                            cores = 4,
                            iter = 4000,
                            refresh=1000
)

model_asocial_fit <- fit_STb(data_list_social,
                            model_asocial,
                            parallel_chains = 4,
                            chains = 4,
                            cores = 4,
                            iter = 4000,
                            refresh = 1000
)

summary_social <- STb_summary(model_social_fit)
summary_asocial <- STb_summary(model_asocial_fit)

# extract posterior for λ₀
draws <- model_asocial_fit$draws()
draws_df <- as_draws_df(draws)
lambda_samples <- draws_df$lambda_0

#lambda_0 <- summary_asocial[summary_asocial$Parameter=="lambda_0", "Median"]

s_real <- summary_social[summary_social$Parameter=="s", "Median"]
s_real
percent_ST_real <- summary_social[summary_social$Parameter=="percent_ST[1]", "Median"]
percent_ST_real

#constrain time window
#compute the observation window per trial
event_data$site <- substr(event_data$id, 1, 1)

#trial_window <- aggregate(time ~ trial, data = event_data,
#                          FUN = function(x) c(min = min(x), max = max(x)))

trial_site_window <- event_data %>%
  group_by(trial, site) %>%
  summarise(
    min_time = min(time),
    max_time = max(time),
    .groups = "drop"
  )

# convert matrix in 'time' column to separate columns
trial_window <- data.frame(
  trial = trial_window$trial,
  min_time = trial_window$time[, 1],
  max_time = trial_window$time[, 2]
)

simulate_times_constrained <- function(lambda, trial_vec, trial_window_df) {
  
  n <- length(trial_vec)
  times <- numeric(n)
  
  for (i in seq_len(n)) {
    trial_id <- trial_vec[i]
    min_t <- trial_window_df$min_time[trial_window_df$trial == trial_id]
    max_t <- trial_window_df$max_time[trial_window_df$trial == trial_id]
    
    repeat {
      t <- rgeom(1, prob = lambda) + 1  # discrete-time geometric
      if (t >= min_t && t <= max_t) {
        times[i] <- t
        break
      }
    }
  }
  
  return(times)
}

#Used this function
simulate_times_range <- function(lambda, n_ind, min_time = 45, max_time = 130) {
  times <- numeric(n_ind)
  
  for (i in 1:n_ind) {
    repeat {
      # simulate acquisition time from geometric (discrete-time hazard)
      t <- rgeom(1, prob = lambda) + 1
      # check if it falls in the biologically plausible range
      if (t >= min_time && t <= max_time) {
        times[i] <- t
        break
      }
    }
  }
  
  return(times)
}

N_sim <- 100 #number of simulations
n_ind <- nrow(event_data) #number of IDs in NBDA

s_null <- numeric(N_sim)
percent_ST_null <- numeric(N_sim)

for (i in 1:N_sim) {
  # sample lambda from posterior
  lambda_0 <- sample(lambda_samples, 1)
  
  # simulate acquisition times in range 45–130
  sim_times <- simulate_times_range(lambda_0, n_ind, min_time = 45, max_time = 130)
  
  event_data_sim <- data.frame(
    id = event_data$id,
    time = sim_times,
    t_end = event_data$t_end,
    trial = event_data$trial
  )
  
  # import into STbayes with real network
  data_list_sim <- import_user_STb(
    event_data = event_data_sim,
    networks = edge_list
  )
  
  # generate social model for simulated data
  model_social_sim <- generate_STb_model(
    data_list_sim,
    gq = TRUE,
    est_acqTime = FALSE,
    data_type = "discrete_time"
  )
  
  # fit social model to simulated data
  fit_social_sim <- fit_STb(
    data_list_sim,
    model_social_sim,
    parallel_chains = 2,  # can reduce chains for speed
    chains = 2,
    cores = 4,
    iter = 2000,
    refresh = 1000
  )
  
  summary_social_sim <- STb_summary(fit_social_sim)
  
  # store median s and percent_ST
  s_null[i] <- summary_social_sim[summary_social_sim$Parameter=="s", "Median"]
  percent_ST_null[i] <- summary_social_sim[summary_social_sim$Parameter=="percent_ST[1]", "Median"]
}

s_real <- as.numeric(s_real)
percent_ST_real <- as.numeric(percent_ST_real)

# Histogram with red line
hist(s_null, breaks = 5, main = "Null distribution of s under asocial learning",
     xlab = "s (social model fitted to asocial data)",
     xlim = range(c(s_null, s_real)))  # include s_real in plot
abline(v = s_real, col = "red", lwd = 2)

hist(percent_ST_null, breaks = 5, main = "Null distribution of percent_ST under asocial learning",
     xlab = "s (social model fitted to asocial data)",
     xlim = range(c(percent_ST_null, percent_ST_real)))  # include s_real in plot
abline(v = percent_ST_real, col = "red", lwd = 2)

# Compute quantile / percentile
quantile_real <- mean(s_null <= s_real)
quantile_real

# Optional: as percentile
quantile_real_percent <- ecdf(s_null)(s_real) * 100
quantile_real_percent


#Model 2013-2024 ----

boxplot(nest_checks_start$start ~ nest_checks_start$site)
boxplot(nest_checks_start$start ~ nest_checks_start$year)
plot(nest_checks_start$year, nest_checks_start$start)

boxplot(nest_checks_lined$start ~ nest_checks_lined$site)
boxplot(nest_checks_lined$start ~ nest_checks_lined$year)
plot(nest_checks_lined$year, nest_checks_lined$start)

#Remove years when observations started late (2019, 2022, 2023)
nest_checks_start <- subset(nest_checks_start, !year == 2019 & !year == 2022 & !year == 2023)

tapply(nest_checks_start$start, nest_checks_start$site, FUN = mean)
tapply(nest_checks_start$start_first_check, nest_checks_start$site, FUN = mean)
nest_checks_start_m <- lmer(start_first_check ~ site + (1|BOX), data = nest_checks_start)
summary(nest_checks_start_m)
Anova(nest_checks_start_m)
emmeans(nest_checks_start_m, ~ site, type = "response") |> pairs()

tapply(nest_checks_lined$lined, nest_checks_lined$site, FUN = mean)

plot(distanceX$Distance, distanceX$StartDiffAbs)
cor.test(distanceX$Distance, distanceX$StartDiffAbs)

plot(distanceY$Distance, distanceY$StartDiffAbs)
cor.test(distanceY$Distance, distanceY$StartDiffAbs)


#Model on long-term traditions

#Remove years when observations started late (2019, 2022, 2023)
nest_checks_start <- subset(nest_checks_start, !year == 2019 & !year == 2022 & !year == 2023)
nest_checks_start <- subset(nest_checks_start, !nest_checks_start$BOX == "Z46")

tapply(nest_checks_start$start, nest_checks_start$site, FUN = mean, na.rm= T)

#Model just with prior
default_prior()

nests_start_brm2_prior <- brm(start ~ site + start_first_check_z
                              + year_z,   
                              data = nest_checks_start, 
                              family = Gamma(link = "log"),
                              prior = c(
                                prior(normal(4.5, 0.25), class = "Intercept"),        
                                prior(normal(0, 0.25), class = "b"),
                                prior(gamma(16, 1), class = "shape")),
                              sample_prior = "only"
)

#Prior predictive checks 
pp_check(nests_start_brm2_prior, ndraws = 100) +
  scale_x_log10()

#Model  
nests_start_brm2 <- brm(start ~ site + year_z + start_first_check_z 
                              + (1|BOX),   
                              data = nest_checks_start, 
                              family = Gamma(link = "log"),
                              prior = c(
                                prior(normal(4.5, 0.25), class = "Intercept"),        
                                prior(normal(0, 0.25), class = "b"),
                                prior(gamma(16, 1), class = "shape")),
)

#Check collinearity
check_collinearity(nests_start_brm2)

#Model summary
summary(nests_start_brm2)

#Posterior predictive checks
pp_check(nests_start_brm2, type = "dens_overlay", ndraws = 100)

#Plot model
plot(nests_start_brm2)
launch_shinystan(nests_start_brm2)

#Posterior distribution
as_draws_df(nests_start_brm2)
mcmc_areas(nests_start_brm2)
mcmc_intervals(nests_start_brm2)

#Evaluation and interpretation
loo(nests_start_brm2, moment_match = TRUE)
fitted(nests_start_brm2, scale = "response")
conditional_effects(nests_start_brm2)
bayes_R2(nests_start_brm2)

#Sensitivity analysis and prior checks 
prior_summary(nests_start_brm2)

#Extract predictions
fitted(nests_start_brm2)

#Hypothesis 
hypothesis(nests_start_brm2, "start_first_check_z > 0")
hypothesis(nests_start_brm2, "siteY < 0")
hypothesis(nests_start_brm2, "siteZ < 0")
hypothesis(nests_start_brm2, "siteY - siteZ < 0")
hypothesis(nests_start_brm2, "year_z < 0")

hypothesis(nests_start_brm2, "siteY:year_z < 0")
hypothesis(nests_start_brm2, "siteZ:year_z < 0")
hypothesis(nests_start_brm2, "siteY:year_z - siteZ:year_z < 0")

emmeans(nests_start_brm2, ~ site, type = "response") |> pairs()

posterior_marginal <- nest_checks_start %>%
  add_epred_draws(
    object = nests_start_brm2,
    re_formula = NA)

#Posterior summary for text reporting 
posterior_draw_nests_start <- posterior_marginal %>%
  group_by(.draw, site) %>%
  summarise(
    epred = mean(.epred),
    .groups = "drop"
  )

posterior_summary <- posterior_draw_nests_start %>%
  group_by(site) %>%
  median_qi(epred, .width = c(0.5, 0.8, 0.95))
posterior_summary

#Contrasts for text reporting
pairwise_contrasts <- posterior_draw_nests_start %>%
  group_by(.draw) %>%
  compare_levels(epred, by = site) %>%
  rename(
    contrast = site,
    diff = epred
  )

pairwise_contrasts_summary <- pairwise_contrasts %>%
  group_by(contrast) %>%
  summarise(
    median = median(diff),
    mean = mean(diff),
    lower_80 = quantile(diff, 0.1),
    upper_80 = quantile(diff, 0.9),
    lower_95 = quantile(diff, 0.025),
    upper_95 = quantile(diff, 0.975),
    P_gt_0 = mean(diff > 0),
    .groups = "drop"
  ) %>%
  arrange(contrast)
pairwise_contrasts_summary

nests_start_halfeye_plot <- ggplot(posterior_draw_nests_start, aes(y = site, x = epred)) +
  stat_halfeye(.width = c(0.5, 0.8, 0.95), fill = "grey") +
  theme_few(base_size = 14) +
  theme(
    legend.position = "none",
    text = element_text(family = "Garamond")) +
  labs(x = "Day of nest initiation", y = "Study site") 
nests_start_halfeye_plot

ce <- conditional_effects(nests_start_brm2, effects = "site", re_formula = NA)
newdat <- as.data.frame(ce[["site"]]) %>%
  dplyr::select(site)
posterior <- posterior_epred(nests_start_brm2, newdata = ce[["site"]], re_formula = NA)
posterior <- posterior_predict(nests_start_brm2, newdata = ce[["site"]], re_formula = NA)

# Convert to long format
posterior_long <- as.data.frame(posterior) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "draw", values_to = "pred") %>%
  mutate(row = as.numeric(gsub("V", "", draw))) %>%
  left_join(newdat %>% mutate(row = row_number()), by = "row")

initiation_plot <- ggplot(posterior_long, aes(x = site, y = pred, fill= site)) +
  geom_violin(alpha = 0.6, position = position_dodge(width = 0.1), width = 0.5) +
  scale_fill_manual(values =c("white", "white", "white")) +
  theme_few(base_size = 14) +
  theme(legend.position="none", text = element_text(size = 14, family = "Garamond")) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.8)) +
  labs(
    x = "Study site",
    y = "Day of nest initiation") +
  scale_x_discrete(labels=c('X', 'Y', 'Z')) +
  scale_y_continuous(limits = c(75,95))
initiation_plot

initiation_plot <- ggplot(posterior_long, aes(x = site, y = pred, fill = site)) +
  # Posterior distributions
  geom_violin(alpha = 0.8, position = position_dodge(width = 0.3), width = 0.5) +
  # Posterior means
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.8), size = 2, color = "black") +
  # Raw data overlay
  geom_jitter(
    data = nest_checks_start,
    aes(x = site, y = start),   # replace 'day_of_initiation' with your actual response variable
    width = 0.15, 
    alpha = 0.1, 
    color = "black", 
    size = 2
  ) +
  # Colors, labels, themes
  scale_fill_manual(values = c("white", "white", "white")) +
  scale_x_discrete(labels = c('X', 'Y', 'Z')) +
  scale_y_continuous(limits = c(60,120)) +
  labs(x = "Study site", y = "Day of nest initiation") +
  theme_few(base_size = 14) +
  theme(legend.position = "none", text = element_text(size = 14, family = "Garamond"))

initiation_plot

#Model on age-related plasticity

#Model just with prior
default_prior()

nests_start_brm3_prior <- brm(start ~ site + start_first_check_z + year_z +
                              avg_pair_age_z + cent_pair_age_z + (1|BOX),
                              data = nest_checks_start_age, 
                              family = Gamma(link = "log"),
                              prior = c(
                                prior(normal(4.5, 0.25), class = "Intercept"),        
                                prior(normal(0, 0.25), class = "b"),
                                prior(gamma(16, 1), class = "shape")),
                              sample_prior = "only"
)

#Prior predictive checks 
pp_check(nests_start_brm3_prior, ndraws = 100) +
  scale_x_log10()

#Model  
nests_start_brm3 <- brm(start ~ site + start_first_check_z + 
                        avg_pair_age_z + cent_pair_age_z
                        + (cent_pair_age_z|pair_ID) + (1|BOX),   
                        data = nest_checks_start_age, 
                        family = Gamma(link = "log"),
                        prior = c(
                          prior(normal(4.5, 0.25), class = "Intercept"),        
                          prior(normal(0, 0.25), class = "b"),
                          prior(gamma(16, 1), class = "shape")),
)

#Check collinearity
check_collinearity(nests_start_brm3)

#Model summary
summary(nests_start_brm3)

#Posterior predictive checks
pp_check(nests_start_brm3, type = "dens_overlay", ndraws = 100)

#Plot model
plot(nests_start_brm3)
launch_shinystan(nests_start_brm3)

#Posterior distribution
as_draws_df(nests_start_brm3)
mcmc_areas(nests_start_brm3)
mcmc_intervals(nests_start_brm3)

#Evaluation and interpretation
loo(nests_start_brm3, moment_match = TRUE)
fitted(nests_start_brm3, scale = "response")
conditional_effects(nests_start_brm3)
bayes_R2(nests_start_brm3)

#Sensitivity analysis and prior checks 
prior_summary(nests_start_brm3)

#Extract predictions
fitted(nests_start_brm3)

#Hypothesis 
hypothesis(nests_start_brm3, "start_first_check_z > 0")
hypothesis(nests_start_brm3, "siteY < 0")
hypothesis(nests_start_brm3, "siteZ < 0")
hypothesis(nests_start_brm3, "siteY - siteZ < 0")
hypothesis(nests_start_brm3, "year_z < 0")

hypothesis(nests_start_brm3, "pair_age_z < 0")
hypothesis(nests_start_brm3, "avg_pair_age_z < 0")
hypothesis(nests_start_brm3, "cent_pair_age_z < 0")

emmeans(nests_start_brm3, ~ site, type = "response") |> pairs()

#Owner experience ----

owner_exp_m1 <- glmmTMB(start_diff_from_mean_abs ~ male_prev_owner_binary + year_z + (1|male_JID), data = nest_checks_start)
summary(owner_exp_m1)
Anova(owner_exp_m1)


#Residents ----

#Do birds moving sites change their start date?

LH_ringed <- subset(LH, CODE == "RINGED")

nest_checks_start$Female_ringed <- LH_ringed$LOCATION[match(nest_checks_start$Female_ID, LH_ringed$ID)]
nest_checks_start$Male_ringed <- LH_ringed$LOCATION[match(nest_checks_start$Male_ID, LH_ringed$ID)]

nest_checks_start_moved <- subset(nest_checks_start, !site == "X")
nest_checks_start_moved <- subset(nest_checks_start_moved, !is.na(nest_checks_start_moved$Female_ID))
nest_checks_start_moved <- subset(nest_checks_start_moved, !is.na(nest_checks_start_moved$Male_ID))

nest_checks_start_moved$Female_Male_ringed <- paste(nest_checks_start_moved$Female_ringed, nest_checks_start_moved$Male_ringed, sep = "_")

nest_checks_start_moved$resident <- ifelse(nest_checks_start_moved$Female_Male_ringed == "PENCOOSE_PENCOOSE", 1, ifelse(nest_checks_start_moved$Female_Male_ringed == "STITHIANS_STITHIANS", 1,0))

nest_checks_start_moved <- subset(nest_checks_start_moved, resident == 0)
nest_checks_start_moved <- subset(nest_checks_start_moved, site == "Z")

boxplot(nest_checks_start_moved$start ~ nest_checks_start_moved$resident)

LH_owners_summary <- as.data.frame(table(LH_owners$ID, LH_owners$SITE))

LH_owners_summary <- subset(LH_owners_summary, LH_owners_summary$Freq > 0)
LH_owners_summary <- subset(LH_owners_summary, !LH_owners_summary$Var2 == "X")
LH_owners_summary <- subset(LH_owners_summary, !LH_owners_summary$Var2 == "M")

#LH_owners <- LH_owners[duplicated(LH_owners$Var1),]

#Start of building - long-term nest checks database

lm <- lmer(start ~ site * year_z + (1|BOX), data = nest_checks_start)

#Model just with prior
default_prior()

start_brm1_prior <- brm(start ~ site * year_z + (1|BOX), 
                        data = nest_checks_start, 
                        family = gaussian(),
                        prior = c(
                          set_prior("normal(0, 0.5)", class = "b"),        
                          set_prior("normal(85, 10)", class = "Intercept")),
                        sample_prior = "only"
)

#Prior predictive checks 
pp_check(start_brm1_prior, ndraws = 100)

#Model
start_brm1 <- brm(start ~ site * year_z + (1|BOX), 
                  data = nest_checks_start, 
                  family = gaussian(),
                  prior = c(
                    set_prior("normal(0, 0.5)", class = "b"),        
                    set_prior("normal(85, 10)", class = "Intercept"))
)

summary(start_brm1)

check_collinearity(start_brm1)

#Posterior predictive checks
pp_check(start_brm1, type = "dens_overlay", ndraws = 100)

#Plot model
plot(start_brm1)
launch_shinystan(start_brm1)

#Posterior distribution
as_draws_df(start_brm1)
mcmc_areas(start_brm1)
mcmc_intervals(start_brm1)

#Evaluation and interpretation
loo(start_brm1, moment_match = TRUE)
fitted(start_brm1, scale = "response")
conditional_effects(start_brm1)
bayes_R2(start_brm1)

conditional_effects(start_brm1,effects = "year_z:site")

emmeans(start_brm1, ~ year_z * site, type = "response") |> pairs()

hypothesis(start_brm1, "siteZ:year_z - siteY:year_z > 0")
hypothesis(start_brm1, "siteY:year_z < 0")
hypothesis(start_brm1, "siteZ:year_z < 0")
hypothesis(start_brm1, "siteY < 0")
hypothesis(start_brm1, "siteY - siteZ < 0")

#Sensitivity analysis and prior checks 
prior_summary(start_brm1)

#Extract predictions
fitted(start_brm1)

start_brm1 %>%
  spread_draws(b_Intercept, b_degree_z) %>%
  ggplot(aes(y = , x = )) +
  theme_classic(base_size = 28) +
  theme(legend.position="none", text = element_text(size = 28, family = "Garamond")) +
  labs(x = "Est (Degree)", y = "Density") +
  stat_halfeye()

cond <- conditional_effects(start_brm1, effect = "degree_z")
p <- plot(cond, points=F)
patch_binary_degree_plot <- p[[1]] + 
  theme_base(base_size = 14) +
  theme(legend.position="none", text = element_text(size = 14, family = "Garamond")) +
  geom_point(
    aes(x = degree_z, y = patch_binary), 
    data = all_individuals_prepatch_sub, 
    color = "black",
    size = 2,
    alpha = 0.2,
    inherit.aes = FALSE) + 
  geom_line(color="black", size=1) + 
  labs(x = "Degree centrality", y = "Probability of patch discovery")
patch_binary_degree_plot

#Body condition ----

#Add body condition
nest_checks_start$female_body_cond <- LH_body$body_cond[match(nest_checks_start$female_JID, LH_body$ID)]
nest_checks_start$male_body_cond <- LH_body$body_cond[match(nest_checks_start$male_JID, LH_body$ID)]

#Check for effect of body condition on nest initiation
nest_checks_start_body <- subset(nest_checks_start, !is.na(nest_checks_start$female_body_cond))
nest_checks_start_body <- subset(nest_checks_start, !is.na(nest_checks_start$male_body_cond))
nest_checks_start_body$pair_body_cond <- (nest_checks_start_body$female_body_cond + nest_checks_start_body$male_body_cond)/2

plot(nest_checks_start_body$pair_body_cond, nest_checks_start_body$start)
cor.test(nest_checks_start_body$pair_body_cond, nest_checks_start_body$start)

#Is body condition linked to nest initiation 
nest_checks_start_body$pair_body_cond_z <- scale(nest_checks_start_body$pair_body_cond)
nest_checks_start_body$female_body_cond_z <- scale(nest_checks_start_body$female_body_cond)
nest_checks_start_body$male_body_cond_z <- scale(nest_checks_start_body$male_body_cond)

nest_checks_start_body <- subset(nest_checks_start_body, !is.na(nest_checks_start_body$pair_body_cond_z))

nests_start_brm4_prior <- brm(start ~ female_body_cond_z + male_body_cond_z  
                          + (1|BOX),   
                        data = nest_checks_start_body, 
                        family = Gamma(link = "log"),
                        prior = c(
                          prior(normal(4.5, 0.25), class = "Intercept"),        
                          prior(normal(0, 0.25), class = "b"),
                          prior(gamma(16, 1), class = "shape")),
                        sample_prior = "only"
)

#Prior predictive checks 
pp_check(nests_start_brm4_prior, ndraws = 100) +
  scale_x_log10()

#Model
nests_start_brm4 <- brm(start ~ site + female_body_cond_z + male_body_cond_z
                        + year_z + (1|BOX) + (1|female_JID) + (1|male_JID),   
                        data = nest_checks_start_body, 
                        family = Gamma(link = "log"),
                        prior = c(
                          prior(normal(4.5, 0.25), class = "Intercept"),        
                          prior(normal(0, 0.25), class = "b"),
                          prior(gamma(16, 1), class = "shape")),
)

summary(nests_start_brm4)

conditional_effects(nests_start_brm4)

hypothesis(nests_start_brm4, "siteY < 0")
hypothesis(nests_start_brm4, "siteZ < 0")
hypothesis(nests_start_brm4, "siteY - siteZ < 0")

hypothesis(nests_start_brm4, "pair_body_cond_z < 0")
hypothesis(nests_start_brm4, "female_body_cond_z < 0")
hypothesis(nests_start_brm4, "male_body_cond_z < 0")

posterior_marginal <- nest_checks_start_body %>%
  add_epred_draws(
    object = nests_start_brm4,
    re_formula = NA)

#Posterior summary for text reporting 
posterior_draw_nests_start <- posterior_marginal %>%
  group_by(.draw, site) %>%
  summarise(
    epred = mean(.epred),
    .groups = "drop"
  )

posterior_summary <- posterior_draw_nests_start %>%
  group_by(site) %>%
  median_qi(epred, .width = c(0.5, 0.8, 0.95))
posterior_summary

#Contrasts for text reporting
pairwise_contrasts <- posterior_draw_nests_start %>%
  group_by(.draw) %>%
  compare_levels(epred, by = site) %>%
  rename(
    contrast = site,
    diff = epred
  )

pairwise_contrasts_summary <- pairwise_contrasts %>%
  group_by(contrast) %>%
  summarise(
    median = median(diff),
    mean = mean(diff),
    lower_80 = quantile(diff, 0.1),
    upper_80 = quantile(diff, 0.9),
    lower_95 = quantile(diff, 0.025),
    upper_95 = quantile(diff, 0.975),
    P_gt_0 = mean(diff > 0),
    .groups = "drop"
  ) %>%
  arrange(contrast)
pairwise_contrasts_summary

#Are birds with similar body condition more likely to nest close to each other
edge_list24$prospect_weight <- NULL
edge_list24$year <- 2024
edge_list1323$year <- edge_list1323$trial + 2012
edge_list_body <- rbind(edge_list1323, edge_list24)
edge_list_body$box_year1 <- paste(edge_list_body$from, edge_list_body$year, sep = "_")
edge_list_body$box_year2 <- paste(edge_list_body$to, edge_list_body$year, sep = "_")
LH_body$box_year <- paste(LH_body$BOX, LH_body$year, sep = "_")

LH_body_females <- subset(LH_body, SEX == "F")
LH_body_males <- subset(LH_body, SEX == "M")

edge_list_body$female_body_cond1 <- LH_body_females$body_cond[match(edge_list_body$box_year1, LH_body_females$box_year)]
edge_list_body$male_body_cond1 <- LH_body_males$body_cond[match(edge_list_body$box_year1, LH_body_females$box_year)]
edge_list_body$female_body_cond2 <- LH_body_females$body_cond[match(edge_list_body$box_year2, LH_body_females$box_year)]
edge_list_body$male_body_cond2 <- LH_body_males$body_cond[match(edge_list_body$box_year2, LH_body_females$box_year)]

edge_list_body$pair_body_cond1 <- (edge_list_body$female_body_cond1 + edge_list_body$male_body_cond1) / 2
edge_list_body$pair_body_cond2 <- (edge_list_body$female_body_cond2 + edge_list_body$male_body_cond2) / 2

edge_list_body$pair_body_cond_diff <- edge_list_body$pair_body_cond1 - edge_list_body$pair_body_cond2
edge_list_body$pair_body_cond_diff_abs <- abs(edge_list_body$pair_body_cond_diff)

plot(edge_list_body$spatial_weight, edge_list_body$pair_body_cond_diff_abs)
cor.test(edge_list_body$spatial_weight, edge_list_body$pair_body_cond_diff_abs)

edge_list_body_sub <- subset(edge_list_body, !is.na(edge_list_body$pair_body_cond_diff_abs))

body_cond_brm1_prior <- brm(spatial_weight + 0.001 ~ pair_body_cond_diff_abs   
                              + (1|mm(from,to)),   
                              data = edge_list_body, 
                              family = Gamma(link = "log"),
                              prior = c(
                                prior(normal(-0.5, 0.25), class = "Intercept"),        
                                prior(normal(0, 0.25), class = "b"),
                                prior(gamma(16, 1), class = "shape")),
                              sample_prior = "only"
)

#Prior predictive checks 
pp_check(body_cond_brm1_prior, ndraws = 100) + 
  scale_x_log10()

#Model
body_cond_brm1 <- brm(spatial_weight + 0.001 ~ pair_body_cond_diff_abs  
                            + (1|mm(from,to)),   
                            data = edge_list_body_sub, 
                            family = Gamma(link = "log"),
                            prior = c(
                              prior(normal(-0.5, 0.25), class = "Intercept"),        
                              prior(normal(0, 0.25), class = "b"),
                              prior(gamma(16, 1), class = "shape"))
)

summary(body_cond_brm1)

hypothesis(body_cond_brm1, "pair_body_cond_diff_abs > 0")

conditional_effects(body_cond_brm1)

pp_check(body_cond_brm1, ndraws = 100) +
  scale_x_log10()

#Nest environment ----

#Check the role of nest environment 
edge_list_environment <- edge_list_body

edge_list_environment$box_attached1 <- nests_environments$attached[match(edge_list_environment$from, nests_environments$box)]
edge_list_environment$box_attached2 <- nests_environments$attached[match(edge_list_environment$to, nests_environments$box)]

edge_list_environment$box_substrate1 <- nests_environments$substrate[match(edge_list_environment$from, nests_environments$box)]
edge_list_environment$box_substrate2 <- nests_environments$substrate[match(edge_list_environment$to, nests_environments$box)]

edge_list_environment$box_attached_same <- ifelse(edge_list_environment$box_attached1 == edge_list_environment$box_attached2, "same", "different")
edge_list_environment$box_substrate_same <- ifelse(edge_list_environment$box_substrate1 == edge_list_environment$box_substrate2, "same", "different")

edge_list_environment$start1 <- nest_checks_start$start[match(edge_list_environment$box_year1, nest_checks_start$box_year)]
edge_list_environment$start2 <- nest_checks_start$start[match(edge_list_environment$box_year2, nest_checks_start$box_year)]

edge_list_environment$start_diff <- edge_list_environment$start1 - edge_list_environment$start2
edge_list_environment$start_diff_abs <- abs(edge_list_environment$start_diff)

edge_list_environment2 <- subset(edge_list_environment, !start_diff_abs == 0)

nest_checks_start$box_attached <- nests_environments$attached[match(nest_checks_start$BOX, nests_environments$box)]
nest_checks_start$box_substrate <- nests_environments$substrate[match(nest_checks_start$BOX, nests_environments$box)]

nest_checks_start$box_attached <- factor(nest_checks_start$box_attached)
nest_checks_start$box_substrate <- factor(nest_checks_start$box_substrate)

nest_checks_start$box_attached_binary <- ifelse(nest_checks_start$box_attached == "tree", "tree", "building")

#Model just with prior
default_prior()

nests_start_brm5_prior <- brm(start ~ box_attached +
                              box_substrate +
                              (1|BOX),
                              data = nest_checks_start, 
                              family = Gamma(link = "log"),
                              prior = c(
                                prior(normal(4.5, 0.25), class = "Intercept"),        
                                prior(normal(0, 0.25), class = "b"),
                                prior(gamma(16, 1), class = "shape")),
                              sample_prior = "only"
)

#Prior predictive checks 
pp_check(nests_start_brm5_prior, ndraws = 100) +
  scale_x_log10()

#Model  
nests_start_brm5 <- brm(start ~ site + year_z + start_first_check_z +
                              box_attached_binary + (1|BOX),
                              data = nest_checks_start, 
                              family = Gamma(link = "log"),
                              prior = c(
                                prior(normal(4.5, 0.25), class = "Intercept"),        
                                prior(normal(0, 0.25), class = "b"),
                                prior(gamma(16, 1), class = "shape"))
)
    
nests_start_brm5 <- brm(start ~ year_z + start_first_check_z +
                          box_attached_binary + (1|BOX),
                        data = nest_checks_start[nest_checks_start$site == "Z",], 
                        family = Gamma(link = "log"),
                        prior = c(
                          prior(normal(4.5, 0.25), class = "Intercept"),        
                          prior(normal(0, 0.25), class = "b"),
                          prior(gamma(16, 1), class = "shape"))
)

#Check collinearity
check_collinearity(nests_start_brm5)

#Model summary
summary(nests_start_brm5)

#Posterior predictive checks
pp_check(nests_start_brm5, type = "dens_overlay", ndraws = 100)

#Plot model
plot(nests_start_brm5)
launch_shinystan(nests_start_brm5)

#Posterior distribution
as_draws_df(nests_start_brm5)
mcmc_areas(nests_start_brm5)
mcmc_intervals(nests_start_brm5)

#Evaluation and interpretation
loo(nests_start_brm5, moment_match = TRUE)
fitted(nests_start_brm5, scale = "response")
conditional_effects(nests_start_brm5)
bayes_R2(nests_start_brm5)

#Sensitivity analysis and prior checks 
prior_summary(nests_start_brm5)

#Extract predictions
fitted(nests_start_brm5)

#Hypothesis 
hypothesis(nests_start_brm5, "box_attachedtree < 0")
hypothesis(nests_start_brm5, "box_attachedwall < 0")
hypothesis(nests_start_brm5, "box_attachedtree - box_attachedwall < 0")
hypothesis(nests_start_brm5, "box_attached_binarytree < 0")
hypothesis(nests_start_brm5, "box_substratesoft > 0")
hypothesis(nests_start_brm5, "siteY < 0")
hypothesis(nests_start_brm5, "siteZ < 0")
hypothesis(nests_start_brm5, "siteY - siteZ < 0")

emmeans(nests_start_brm5, ~ box_attached, type = "response") |> pairs()

#Climate across sites ----

install.packages("terra")


library(terra)
library(dplyr)

# Open the HadUK-Grid monthly mean temperature file
r <- rast("C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/tas_hadukgrid_uk_1km_mon-30y_199101-202012.nc")
r <- rast("C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/rainfall_hadukgrid_uk_1km_mon-30y_199101-202012.nc")

# See the grid coordinates / CRS
crs(r)
ext(r)

# Coordinates for your three locations
locations <- data.frame(
  place = c("Campus", "Stithians", "Pencoose"),
  lon   = c(-5.118078, -5.181447, -5.170245),
  lat   = c(50.173131, 50.189921, 50.198730)
)


# Convert lon/lat to the grid CRS
pts <- vect(locations, geom = c("lon", "lat"), crs = "EPSG:4326")
pts <- project(pts, crs(r))

# Extract the grid-cell values
values <- extract(r, pts)

values


#Social pedigree ----
social_pedigree$dyad_ID <- ifelse(social_pedigree$Id1 < social_pedigree$Id2, paste(social_pedigree$Id1, social_pedigree$Id2, sep = "_"), paste(social_pedigree$Id2, social_pedigree$Id1, sep = "_"))

edge_list24$prospect_weight <- NULL
edge_list24$year <- 2024
edge_list1323$year <- edge_list1323$trial + 2012
edge_list1323 <- subset(edge_list1323, !edge_list1323$year == 2019 & !edge_list1323$year == 2020)

edge_list_pedigree <- rbind(edge_list1323, edge_list24)
edge_list_pedigree <- subset(edge_list_pedigree, !edge_list_pedigree$year == 2019 & !edge_list_pedigree$year == 2020)

edge_list_pedigree$box_year1 <- paste(edge_list_pedigree$from, edge_list_pedigree$year, sep = "_")
edge_list_pedigree$box_year2 <- paste(edge_list_pedigree$to, edge_list_pedigree$year, sep = "_")

edge_list_pedigree$female1_JID <- LH_owners_females$ID[match(edge_list_pedigree$box_year1, LH_owners_females$box_year)]
edge_list_pedigree$male1_JID <- LH_owners_males$ID[match(edge_list_pedigree$box_year1, LH_owners_males$box_year)]

edge_list_pedigree$female2_JID <- LH_owners_females$ID[match(edge_list_pedigree$box_year2, LH_owners_females$box_year)]
edge_list_pedigree$male2_JID <- LH_owners_males$ID[match(edge_list_pedigree$box_year2, LH_owners_males$box_year)]

edge_list_pedigree$fem1_fem2_dyad_ID <- ifelse(edge_list_pedigree$female1_JID < edge_list_pedigree$female2_JID, paste(edge_list_pedigree$female1_JID, edge_list_pedigree$female2_JID, sep = "_"), paste(edge_list_pedigree$female2_JID, edge_list_pedigree$female1_JID, sep = "_"))
edge_list_pedigree$fem1_male2_dyad_ID <- ifelse(edge_list_pedigree$female1_JID < edge_list_pedigree$male2_JID, paste(edge_list_pedigree$female1_JID, edge_list_pedigree$male2_JID, sep = "_"), paste(edge_list_pedigree$male2_JID, edge_list_pedigree$female1_JID, sep = "_"))
edge_list_pedigree$male1_fem2_dyad_ID <- ifelse(edge_list_pedigree$male1_JID < edge_list_pedigree$female2_JID, paste(edge_list_pedigree$male1_JID, edge_list_pedigree$female2_JID, sep = "_"), paste(edge_list_pedigree$female2_JID, edge_list_pedigree$male1_JID, sep = "_"))
edge_list_pedigree$male1_male2_dyad_ID <- ifelse(edge_list_pedigree$male1_JID < edge_list_pedigree$male2_JID, paste(edge_list_pedigree$male1_JID, edge_list_pedigree$male2_JID, sep = "_"), paste(edge_list_pedigree$male2_JID, edge_list_pedigree$male1_JID, sep = "_"))

edge_list_pedigree$fem1_fem2_relatedness <- social_pedigree$Id2_relatedness[match(edge_list_pedigree$fem1_fem2_dyad_ID, social_pedigree$dyad_ID)]
edge_list_pedigree$fem1_male2_relatedness <- social_pedigree$Id2_relatedness[match(edge_list_pedigree$fem1_male2_dyad_ID, social_pedigree$dyad_ID)]
edge_list_pedigree$male1_fem2_relatedness <- social_pedigree$Id2_relatedness[match(edge_list_pedigree$male1_fem2_dyad_ID, social_pedigree$dyad_ID)]
edge_list_pedigree$male1_male2_relatedness <- social_pedigree$Id2_relatedness[match(edge_list_pedigree$male1_male2_dyad_ID, social_pedigree$dyad_ID)]

edge_list_pedigree$fem1_fem2_kin <- social_pedigree$kin[match(edge_list_pedigree$fem1_fem2_dyad_ID, social_pedigree$dyad_ID)]
edge_list_pedigree$fem1_male2_kin <- social_pedigree$kin[match(edge_list_pedigree$fem1_male2_dyad_ID, social_pedigree$dyad_ID)]
edge_list_pedigree$male1_fem2_kin <- social_pedigree$kin[match(edge_list_pedigree$male1_fem2_dyad_ID, social_pedigree$dyad_ID)]
edge_list_pedigree$male1_male2_kin <- social_pedigree$kin[match(edge_list_pedigree$male1_male2_dyad_ID, social_pedigree$dyad_ID)]

edge_list_pedigree$fem1_fem2_kin_binary <- ifelse(!is.na(edge_list_pedigree$fem1_fem2_kin), 1, 0)
edge_list_pedigree$fem1_male2_kin_binary <- ifelse(!is.na(edge_list_pedigree$fem1_male2_kin), 1, 0)
edge_list_pedigree$male1_fem2_kin_binary <- ifelse(!is.na(edge_list_pedigree$male1_fem2_kin), 1, 0)
edge_list_pedigree$male1_male2_kin_binary <- ifelse(!is.na(edge_list_pedigree$male1_male2_kin), 1, 0)

edge_list_pedigree$kin_prop <- (edge_list_pedigree$fem1_fem2_kin_binary + edge_list_pedigree$fem1_male2_kin_binary + edge_list_pedigree$male1_fem2_kin_binary + edge_list_pedigree$male1_male2_kin_binary) /4
table(edge_list_pedigree$kin_prop)
edge_list_pedigree$kin_binary <- ifelse(edge_list_pedigree$kin_prop == 0, 0, 1)

edge_list_pedigree$start1 <- nest_checks_start$start[match(edge_list_pedigree$box_year1, nest_checks_start$box_year)]
edge_list_pedigree$start2 <- nest_checks_start$start[match(edge_list_pedigree$box_year2, nest_checks_start$box_year)]

edge_list_pedigree$start_diff <- edge_list_pedigree$start1 - edge_list_pedigree$start2
edge_list_pedigree$start_diff_abs <- abs(edge_list_pedigree$start_diff)

boxplot(edge_list_pedigree$start_diff_abs ~ edge_list_pedigree$kin_prop)

edge_list_pedigree_sub <- subset(edge_list_pedigree, !is.na(edge_list_pedigree$female1_JID))
edge_list_pedigree_sub <- subset(edge_list_pedigree_sub, !is.na(edge_list_pedigree_sub$female2_JID))
edge_list_pedigree_sub <- subset(edge_list_pedigree_sub, !is.na(edge_list_pedigree_sub$male1_JID))
edge_list_pedigree_sub <- subset(edge_list_pedigree_sub, !is.na(edge_list_pedigree_sub$male2_JID))

kin_start_m1 <- glmmTMB(start_diff_abs + 0.1 ~ kin_prop + (1|from) + (1|to), data = edge_list_pedigree, family = Gamma(link = "log"))
summary(kin_start_m1)
Anova(kin_start_m1)

kin_nest_m1 <- glmmTMB(spatial_weight + 0.01 ~ kin_prop + (1|from) + (1|to), data = edge_list_pedigree, family = Gamma(link = "log"))
summary(kin_nest_m1)
Anova(kin_nest_m1)

kin_brm1_prior <- brm(spatial_weight + 0.001 ~ kin_prop   
                            + (1|mm(from,to)),   
                            data = edge_list_pedigree_sub, 
                            family = Gamma(link = "log"),
                            prior = c(
                              prior(normal(-0.5, 0.25), class = "Intercept"),        
                              prior(normal(0, 0.25), class = "b"),
                              prior(gamma(16, 1), class = "shape")),
                            sample_prior = "only"
)

#Prior predictive checks 
pp_check(kin_brm1_prior, ndraws = 100) + 
  scale_x_log10()

#Model
kin_brm1 <- brm(spatial_weight + 0.001 ~ kin_prop  
                      + (1|mm(from,to)),   
                      data = edge_list_pedigree_sub, 
                      family = Gamma(link = "log"),
                      prior = c(
                        prior(normal(-0.5, 0.25), class = "Intercept"),        
                        prior(normal(0, 0.25), class = "b"),
                        prior(gamma(16, 1), class = "shape"))
)

summary(kin_brm1)

hypothesis(kin_brm1, "kin_prop > 0")

conditional_effects(kin_brm1)

pp_check(kin_brm1, ndraws = 100) +
  scale_x_log10()


kin_brm1_prior <- brm(start_diff_abs + 0.001 ~ kin_prop   
                      + (1|mm(from,to)),   
                      data = edge_list_pedigree, 
                      family = Gamma(link = "log"),
                      prior = c(
                        prior(normal(2.25, 0.25), class = "Intercept"),        
                        prior(normal(0, 0.25), class = "b"),
                        prior(gamma(16, 1), class = "shape")),
                      sample_prior = "only"
)

#Prior predictive checks 
pp_check(kin_brm1_prior, ndraws = 100) + 
  scale_x_log10()

#Model
kin_brm1 <- brm(start_diff_abs + 0.001 ~ kin_prop + year
                + (1|mm(from,to)),   
                data = edge_list_pedigree_sub, 
                family = Gamma(link = "log"),
                prior = c(
                  prior(normal(2.25, 0.25), class = "Intercept"),        
                  prior(normal(0, 0.25), class = "b"),
                  prior(gamma(16, 1), class = "shape"))
)

summary(kin_brm1)

hypothesis(kin_brm1, "kin_prop > 0")

conditional_effects(kin_brm1)

pp_check(kin_brm1, ndraws = 100) +
  scale_x_log10()

#02. Duration of building ----

#Model on duration

nests_summary$Start_z <- scale(nests_summary$Start)
nests_summary_nosquirrel$Start_z <- scale(nests_summary_nosquirrel$Start)

#Model just with prior
default_prior()

nests_duration_brm1_prior <- brm(Duration ~ Site + Start_z,
                              data = nests_summary_nosquirrel, 
                              family = Gamma(link = "log"),
                              prior = c(
                                prior(normal(3.5, 0.25), class = "Intercept"),        
                                prior(normal(0, 0.25), class = "b"),
                                prior(gamma(16, 1), class = "shape")),
                              sample_prior = "only"
)

#Prior predictive checks 
pp_check(nests_duration_brm1_prior, ndraws = 100) +
  scale_x_log10()

#Model  
nests_duration_brm1 <- brm(Duration ~ Site + Start_z, 
                        data = nests_summary_nosquirrel, 
                        family = Gamma(link = "log"),
                        prior = c(
                          prior(normal(3.5, 0.25), class = "Intercept"),        
                          prior(normal(0, 0.25), class = "b"),
                          prior(gamma(16, 1), class = "shape")),
)

#Check collinearity
check_collinearity(nests_duration_brm1)

#Model summary
summary(nests_duration_brm1)

#Posterior predictive checks
pp_check(nests_duration_brm1, type = "dens_overlay", ndraws = 100)

#Plot model
plot(nests_duration_brm1)
launch_shinystan(nests_duration_brm1)

#Posterior distribution
as_draws_df(nests_duration_brm1)
mcmc_areas(nests_duration_brm1)
mcmc_intervals(nests_duration_brm1)

#Evaluation and interpretation
loo(nests_duration_brm1, moment_match = TRUE)
fitted(nests_duration_brm1, scale = "response")
conditional_effects(nests_duration_brm1)
bayes_R2(nests_duration_brm1)

#Sensitivity analysis and prior checks 
prior_summary(nests_duration_brm1)

#Extract predictions
fitted(nests_duration_brm1)

#Hypothesis 
hypothesis(nests_duration_brm1, "start_first_check_z > 0")
hypothesis(nests_duration_brm1, "SiteY > 0")
hypothesis(nests_duration_brm1, "SiteZ < 0")
hypothesis(nests_duration_brm1, "SiteY - SiteZ < 0")
hypothesis(nests_duration_brm1, "year_z < 0")

hypothesis(nests_duration_brm1, "pair_age_z < 0")
hypothesis(nests_duration_brm1, "avg_pair_age_z < 0")
hypothesis(nests_duration_brm1, "cent_pair_age_z < 0")

emmeans(nests_duration_brm1, ~ Site, type = "response") |> pairs()


#03. Material selection  ----

#Network between nests and materials 
nests_materials <- nest_obs_direct_materials[, c("Box", "Moss_binary", "Fur_binary", "Leaves_binary", "Sticks_binary", "Grass_binary", "Feather_binary", "Bark_binary", "Paper_binary", "Plastic_binary", "Fabric_binary", "Col_wool_binary")]
nests_materials <- nests_materials %>% rename(Box = Box,
                                              Moss = Moss_binary,
                                              Fur = Fur_binary,
                                              Leaves = Leaves_binary,
                                              Sticks = Sticks_binary,
                                              Grass = Grass_binary,
                                              Feather = Feather_binary,
                                              Bark = Bark_binary,
                                              Paper = Paper_binary,
                                              Plastic = Plastic_binary,
                                              Fabric = Fabric_binary,
                                              Col_wool = Col_wool_binary)

nests_materials <- subset(nests_materials, !nests_materials$Box == "Z25" & !nests_materials$Box == "Z46")

m_nests_materials <- as.matrix(nests_materials[, -1])
row.names(m_nests_materials) <- nests_materials$Box

g_nests_materials <- graph_from_biadjacency_matrix(m_nests_materials, weighted = TRUE)

V(g_nests_materials)$type

V(g_nests_materials)$label <- c(rownames(m_nests_materials), colnames(m_nests_materials))

V(g_nests_materials)$class <- ifelse(V(g_nests_materials)$type, "material", "nest")

V(g_nests_materials)$shape <- ifelse(V(g_nests_materials)$type, "circle", "square")

V(g_nests_materials)$color <- ifelse(V(g_nests_materials)$type, "white", "grey")

lay <- layout_as_bipartite(g_nests_materials)
lay[0.1, 0.1] <- scale(lay[0.1, 0.1])
lay[, 1] <- scale(lay[, 1])

plot(
  g_nests_materials,
  layout = lay,
  asp = 0,
  vertex.size = 6,
  vertex.label.cex = 0.5,
  vertex.label.color = "black"
)

is_material <- V(g_nests_materials)$type

layout_ring <- function(g, r_outer = 1, r_inner = 0.5) {
  
  is_material <- V(g)$type
  
  idx_nest <- which(!is_material)
  idx_mat  <- which(is_material)
  
  n_nest <- length(idx_nest)
  n_mat  <- length(idx_mat)
  
  lay <- matrix(0, nrow = vcount(g), ncol = 2)
  
  # angles
  theta_nest <- seq(0, 2*pi, length.out = n_nest + 1)[-1]
  theta_mat  <- seq(0, 2*pi, length.out = n_mat  + 1)[-1]
  
  # assign explicitly by index
  lay[idx_nest, 1] <- r_outer * cos(theta_nest)
  lay[idx_nest, 2] <- r_outer * sin(theta_nest)
  
  lay[idx_mat, 1]  <- r_inner * cos(theta_mat)
  lay[idx_mat, 2]  <- r_inner * sin(theta_mat)
  
  lay
}

lay <- layout_ring(g_nests_materials)

plot(
  g_nests_materials,
  layout = lay,
  vertex.size = ifelse(V(g_nests_materials)$type, 6, 6),
  vertex.color = ifelse(V(g_nests_materials)$type, "white", "darkgrey"),
  vertex.label.cex = ifelse(V(g_nests_materials)$type, 0.9, 0.6),
  vertex.label.color = "black",
  edge.color = "lightgrey",
  asp = 0,
  edge.curved = 0.25,
  edge.width = 1
)

#Sankey diagram
nest_obs_direct_sankey <- nest_obs_direct_noNA[, c("Nest.box", "Study.Day", "Sticks", "Grass", "Moss", "Fur", "Leaves", "Feather", "Bark", "Paper", "Plastic", "Fabric", "Coloured.wool")]

nest_obs_direct_sankey <- nest_obs_direct_sankey %>%
  pivot_longer(
    cols = -c(Nest.box, Study.Day),
    names_to = "material",
    values_to = "present"
  )

first_appearance <- nest_obs_direct_sankey %>%
  filter(present == 1) %>%
  group_by(Nest.box, material) %>%
  summarise(first_time = min(Study.Day), .groups = "drop")

first_appearance <- first_appearance %>%
  mutate(material_group = case_when(
    material %in% c("Moss","Grass","Sticks", "Bark", "Leaves") ~ "Plant",
    material %in% c("Fur", "Feather") ~ "Animal",
    material %in% c("Paper", "Plastic","Fabric", "Coloured.wool") ~ "Anthropogenic"
  ))

ordered_materials <- first_appearance %>%
  group_by(Nest.box) %>%
  arrange(first_time, .by_group = TRUE) %>%
  mutate(order = row_number()) %>%
  ungroup()

ordered_materials <- first_appearance %>%
  group_by(Nest.box) %>%
  mutate(order = dense_rank(first_time)) %>%  #allows ties
  ungroup()

df_wide <- ordered_materials %>%
  filter(order <= 9) %>%
  mutate(order = paste0("step_", order)) %>%
  pivot_wider(
    id_cols = Nest.box,
    names_from = order,
    values_from = material
  )

df_wide <- ordered_materials %>%
  filter(order <= 9) %>%
  mutate(order = paste0("step_", order)) %>%
  pivot_wider(
    id_cols = Nest.box,
    names_from = order,
    values_from = material_group
  )

df_long <- df_wide %>%
  pivot_longer(
    cols = -Nest.box,
    names_to = "order",
    values_to = "material"
  ) %>%
  filter(!is.na(material))

df_long <- df_wide %>%
  pivot_longer(
    cols = -Nest.box,
    names_to = "order",
    values_to = "material_group"
  ) %>%
  filter(!is.na(material_group))

df_long <- df_long %>%
  mutate(material_group = case_when(
    material %in% c("Moss","Grass","Sticks", "Bark", "Leaves") ~ "Plant",
    material %in% c("Fur", "Feather") ~ "Animal",
    material %in% c("Paper", "Plastic","Fabric", "Coloured.wool") ~ "Anthropogenic"
  ))

install.packages("ggalluvial")
library(ggalluvial)

ggplot(df_long,
       aes(x = order,
           stratum = material_group,
           alluvium = Nest.box,
           fill = material_group)) +
  geom_flow(stat = "alluvium",
            lode.guidance = "frontback",
            color = "grey30",
            alpha = 0.35) +
  geom_stratum(color = "grey20") +
  theme_classic() +
  labs(x = "Sequence of nest material incorporation",
       y = "Number of nests")


#1. Material diversity ----

nest_obs_direct_materials_2426$Diversity_max <- 11

nest_obs_direct_materials_2426$Diversity[nest_obs_direct_materials_2426$Year == 2024 & nest_obs_direct_materials_2426$Nestbox == "Y04"] <- 4

nest_obs_direct_materials_2426 <- subset(nest_obs_direct_materials_2426, !nest_obs_direct_materials_2426$Nestbox == "Z25" & !nest_obs_direct_materials_2426$Nestbox == "Z46")
  
nest_obs_direct_materials_2426$Year <- as.factor(nest_obs_direct_materials_2426$Year)

mean(nest_obs_direct_materials$Diversity/nest_obs_direct_materials$Diversity_max)
sd(nest_obs_direct_materials$Diversity/nest_obs_direct_materials$Diversity_max)

mean(nest_obs_direct_materials_2426$Diversity)
sd(nest_obs_direct_materials_2426$Diversity)

tapply(nest_obs_direct_materials_2426$Diversity, nest_obs_direct_materials_2426$Year, mean)
tapply(nest_obs_direct_materials_2426$Diversity, nest_obs_direct_materials_2426$Year, sd)

#Material diversity
#Model just with prior
default_prior()
mat_div_brm1_prior <- brm(Diversity | trials(Diversity_max) ~ Site, 
                          data = nest_obs_direct_materials_2426, 
                          family = binomial(link = "logit"),
                          prior = c(
                            set_prior("normal(0, 0.25)", class = "Intercept"),        
                            set_prior("normal(0, 0.25)", class = "b")),
                          sample_prior = "only"
)

#Prior predictive checks 
pp_check(mat_div_brm1_prior, ndraws = 100)

#Model including
mat_div_brm1 <- brm(Diversity | trials(Diversity_max) ~ Site + Year + (1|Nestbox), 
                    data = nest_obs_direct_materials_2426, 
                    family = binomial(link = "logit"),
                    prior = c(
                      set_prior("normal(0, 0.25)", class = "Intercept"),        
                      set_prior("normal(0, 0.25)", class = "b")),
)

summary(mat_div_brm1)
check_collinearity(mat_div_brm1)

#Posterior predictive checks
pp_check(mat_div_brm1, type = "dens_overlay", ndraws = 100)

#Plot model
plot(mat_div_brm1)

#Posterior distribution
as_draws_df(mat_div_brm1)
mcmc_areas(mat_div_brm1)
mcmc_intervals(mat_div_brm1)

#Evaluation and interpretation
loo(mat_div_brm1)
fitted(mat_div_brm1, scale = "response")
conditional_effects(mat_div_brm1)
bayes_R2(mat_div_brm1)

#Sensitivity analysis and prior checks 
prior_summary(mat_div_brm1)

#Extract predictions
fitted(mat_div_brm1)

emmeans(mat_div_brm1, ~ Site, type = "response") |> pairs()

posterior_marginal <- nest_obs_direct_materials_2426 %>%
  add_epred_draws(
    object = mat_div_brm1,
    re_formula = NA)

#Posterior summary for text reporting 
posterior_draw_nests_div <- posterior_marginal %>%
  group_by(.draw, Site) %>%
  summarise(
    epred = mean(.epred),
    .groups = "drop"
  )

posterior_summary <- posterior_draw_nests_div %>%
  group_by(Site) %>%
  median_qi(epred, .width = c(0.5, 0.8, 0.95))
posterior_summary

#Contrasts for text reporting
pairwise_contrasts <- posterior_draw_nests_div %>%
  group_by(.draw) %>%
  compare_levels(epred, by = Site) %>%
  rename(
    contrast = Site,
    diff = epred
  )

pairwise_contrasts_summary <- pairwise_contrasts %>%
  group_by(contrast) %>%
  summarise(
    median = median(diff),
    mean = mean(diff),
    lower_80 = quantile(diff, 0.1),
    upper_80 = quantile(diff, 0.9),
    lower_95 = quantile(diff, 0.025),
    upper_95 = quantile(diff, 0.975),
    P_gt_0 = mean(diff > 0),
    .groups = "drop"
  ) %>%
  arrange(contrast)
pairwise_contrasts_summary

nests_div_halfeye_plot <- ggplot(posterior_draw_nests_div, aes(y = Site, x = epred)) +
  stat_halfeye(.width = c(0.5, 0.8, 0.95), fill = "grey") +
  theme_few(base_size = 14) +
  theme(
    legend.position = "none",
    text = element_text(family = "Garamond")) +
  labs(x = "Day of nest initiation", y = "Study site") 
nests_div_halfeye_plot


#2. Material similarity ----

#Network between nests 
nest_obs_direct_materials_24$Paper_binary[nest_obs_direct_materials_24$Nestbox == "Y04"] <- 1

nests_materials_24 <- nest_obs_direct_materials_24[, c("Nestbox", "Moss_binary", "Fur_binary", "Leaves_binary", "Sticks_binary", "Grass_binary", "Feathers_binary", "Bark_binary", "Paper_binary", "Plastic_binary", "Fabric_binary", "Col_wool_binary")]
nests_materials_26 <- nest_obs_direct_materials_26[, c("Nestbox", "Moss_binary", "Fur_binary", "Leaves_binary", "Sticks_binary", "Grass_binary", "Feathers_binary", "Bark_binary", "Paper_binary", "Plastic_binary", "Fabric_binary", "Col_wool_binary")]

#nest_obs26 <- nest_obs26 %>%
#  rename(Box = Nestbox)

#nest_obs26 <- nest_obs26 %>%
#  rename(Feather_binary = Feathers_binary)

#nests_materials2 <- nest_obs26[, c("Box", "Moss_binary", "Fur_binary", "Leaves_binary", "Sticks_binary", "Grass_binary", "Feather_binary", "Bark_binary", "Paper_binary", "Plastic_binary", "Fabric_binary", "Col_wool_binary")]

#Remove Z25 and Z46
nests_materials_24 <- subset(nests_materials_24, !Nestbox == "Z25" & !Nestbox == "Z46")

#rownames(nests_materials2) <- nests_materials2[, 1]
nests_materials2 <- nests_materials2 %>% remove_rownames %>% column_to_rownames(var="Box")

nests_materials_24 <- nests_materials_24 %>% remove_rownames %>% column_to_rownames(var="Nestbox")
nests_materials_26 <- nests_materials_26 %>% remove_rownames %>% column_to_rownames(var="Nestbox")

#For specific sites
#nests_materials2 <- subset(nests_materials2, substr(Box,1,1) == "Y")
#nests_materials2 <- subset(nests_materials2, substr(Box,1,1) == "Z")

#rownames(nests_materials2) <- nest_obs_direct_materials[15:47 , 1]
#rownames(nests_materials2) <- nest_obs_direct_materials[48:83 , 1]

nests_materials2 <- as.matrix(nests_materials2[ , -1])

nests_materials_24 <- as.matrix(nests_materials_24[ , -1])
nests_materials_26 <- as.matrix(nests_materials_26[ , -1])

library(vegan)

#Jaccard distance (1 − similarity) & Jaccard similarity
materials_jac_dist <- vegdist(nests_materials2, method = "jaccard", binary = TRUE)
materials_jac_dist
materials_jac_sim <- 1 - as.matrix(materials_jac_dist)

materials_jac_dist_24 <- vegdist(nests_materials_24, method = "jaccard", binary = TRUE)
materials_jac_dist_24
materials_jac_sim_24 <- 1 - as.matrix(materials_jac_dist_24)

materials_jac_dist_26 <- vegdist(nests_materials_26, method = "jaccard", binary = TRUE)
materials_jac_dist_26
materials_jac_sim_26 <- 1 - as.matrix(materials_jac_dist_26)

#Principal coordinates analysis (PCoA)
pcoa <- cmdscale(materials_jac_dist, eig = TRUE, k = 2)

plot(pcoa$points,
     xlab = "PCoA1",
     ylab = "PCoA2",
     pch = 19)

library(reshape2)

jac_sim_mat <- 1 - as.matrix(materials_jac_dist)
diag(jac_sim_mat) <- NA  # remove self-similarity

jac_sim_mat_24 <- 1 - as.matrix(materials_jac_dist_24)
diag(jac_sim_mat_24) <- NA  # remove self-similarity

jac_sim_mat_26 <- 1 - as.matrix(materials_jac_dist_26)
diag(jac_sim_mat_26) <- NA  # remove self-similarity

nests_sim_edges <- reshape2::melt(jac_sim_mat, na.rm = TRUE)
colnames(nests_sim_edges) <- c("nest1", "nest2", "weight")

nests_sim_edges_24 <- reshape2::melt(jac_sim_mat_24, na.rm = TRUE)
colnames(nests_sim_edges_24) <- c("nest1", "nest2", "weight")

nests_sim_edges_26 <- reshape2::melt(jac_sim_mat_26, na.rm = TRUE)
colnames(nests_sim_edges_26) <- c("nest1", "nest2", "weight")

g_nests <- graph_from_data_frame(nests_sim_edges, directed = FALSE)

plot(g_nests)

#Optional: remove weak edges
g_nests <- delete_edges(g_nests, E(g_nests)[weight < 0.85])

lay <- layout_nicely(g_nests)
lay <- layout_components(g_nests)
lay <- layout_ring(g_nests)

plot(g_nests, 
     layout = lay,
     vertex.size = 3,
     vertex.color = "white",
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     edge.color = "lightgrey",
     asp = 0,
     edge.curved = 0.25,
     edge.width = E(g_nests)$weight * 3, 
     
)

#Add site combination
nests_sim_edges$site_comb <- paste(substr(nests_sim_edges$nest1,1,1), substr(nests_sim_edges$nest2,1,1), sep = "")
nests_sim_edges$site_same_other <- ifelse(nests_sim_edges$site_comb == "XX" | nests_sim_edges$site_comb == "YY" | nests_sim_edges$site_comb == "ZZ", "same", "other")
nests_sim_edges$site_same_semi_other <- ifelse(nests_sim_edges$site_comb == "XX" | nests_sim_edges$site_comb == "YY" | nests_sim_edges$site_comb == "ZZ", "same", ifelse(nests_sim_edges$site_comb == "ZY", "semi", "other"))

nests_sim_edges_24$site_comb <- paste(substr(nests_sim_edges_24$nest1,1,1), substr(nests_sim_edges_24$nest2,1,1), sep = "")
nests_sim_edges_24$site_same_other <- ifelse(nests_sim_edges_24$site_comb == "XX" | nests_sim_edges_24$site_comb == "YY" | nests_sim_edges_24$site_comb == "ZZ", "same", "other")
nests_sim_edges_24$site_same_semi_other <- ifelse(nests_sim_edges_24$site_comb == "XX" | nests_sim_edges_24$site_comb == "YY" | nests_sim_edges_24$site_comb == "ZZ", "same", ifelse(nests_sim_edges_24$site_comb == "ZY", "semi", "other"))

nests_sim_edges_26$site_comb <- paste(substr(nests_sim_edges_26$nest1,1,1), substr(nests_sim_edges_26$nest2,1,1), sep = "")
nests_sim_edges_26$site_same_other <- ifelse(nests_sim_edges_26$site_comb == "XX" | nests_sim_edges_26$site_comb == "YY" | nests_sim_edges_26$site_comb == "ZZ", "same", "other")
nests_sim_edges_26$site_same_semi_other <- ifelse(nests_sim_edges_26$site_comb == "XX" | nests_sim_edges_26$site_comb == "YY" | nests_sim_edges_26$site_comb == "ZZ", "same", ifelse(nests_sim_edges_26$site_comb == "ZY", "semi", "other"))

#Nests as character
nests_sim_edges$nest1 <- as.character(nests_sim_edges$nest1)
nests_sim_edges$nest2 <- as.character(nests_sim_edges$nest2)

nests_sim_edges_24$nest1 <- as.character(nests_sim_edges_24$nest1)
nests_sim_edges_24$nest2 <- as.character(nests_sim_edges_24$nest2)

nests_sim_edges_26$nest1 <- as.character(nests_sim_edges_26$nest1)
nests_sim_edges_26$nest2 <- as.character(nests_sim_edges_26$nest2)

#Box dyad ID
nests_sim_edges$dyad_ID <- ifelse(nests_sim_edges$nest1 < nests_sim_edges$nest2, paste(nests_sim_edges$nest1, nests_sim_edges$nest2, sep = "_"), paste(nests_sim_edges$nest2, nests_sim_edges$nest1, sep = "_"))
distance$dyad_ID <- ifelse(distance$InputID < distance$TargetID, paste(distance$InputID, distance$TargetID, sep = "_"), paste(distance$TargetID, distance$InputID, sep = "_"))
edge_list$dyad_ID <- ifelse(edge_list$from < edge_list$to, paste(edge_list$from, edge_list$to, sep = "_"), paste(edge_list$to, edge_list$from, sep = "_"))

nests_sim_edges_24$dyad_ID <- ifelse(nests_sim_edges_24$nest1 < nests_sim_edges_24$nest2, paste(nests_sim_edges_24$nest1, nests_sim_edges_24$nest2, sep = "_"), paste(nests_sim_edges_24$nest2, nests_sim_edges_24$nest1, sep = "_"))
nests_sim_edges_26$dyad_ID <- ifelse(nests_sim_edges_26$nest1 < nests_sim_edges_26$nest2, paste(nests_sim_edges_26$nest1, nests_sim_edges_26$nest2, sep = "_"), paste(nests_sim_edges_26$nest2, nests_sim_edges_26$nest1, sep = "_"))

#Only dyads within sites
nests_sim_edges_within <- subset(nests_sim_edges, nests_sim_edges$site_comb == "XX"|nests_sim_edges$site_comb == "YY"|nests_sim_edges$site_comb == "ZZ")

nests_sim_edges_within_24 <- subset(nests_sim_edges_24, nests_sim_edges_24$site_comb == "XX"|nests_sim_edges_24$site_comb == "YY"|nests_sim_edges_24$site_comb == "ZZ")
nests_sim_edges_within_26 <- subset(nests_sim_edges_26, nests_sim_edges_26$site_comb == "XX"|nests_sim_edges_26$site_comb == "YY"|nests_sim_edges_26$site_comb == "ZZ")

#Add spatial distance
nests_sim_edges_within$spatial <- distance$Distance[match(nests_sim_edges_within$dyad_ID, distance$dyad_ID)]
nests_sim_edges_within$spatial_z <- scale(nests_sim_edges_within$spatial)

nests_sim_edges_within_24$spatial <- distance$Distance[match(nests_sim_edges_within_24$dyad_ID, distance$dyad_ID)]
nests_sim_edges_within_24$spatial_z <- scale(nests_sim_edges_within_24$spatial)

nests_sim_edges_within_26$spatial <- distance$Distance[match(nests_sim_edges_within_26$dyad_ID, distance$dyad_ID)]
nests_sim_edges_within_26$spatial_z <- scale(nests_sim_edges_within_26$spatial)

#Add spatial weight (0 to 1)
dmin <- min(nests_sim_edges_within$spatial)
dmax <- max(nests_sim_edges_within$spatial)

dmin_24 <- min(nests_sim_edges_within_24$spatial)
dmax_24 <- max(nests_sim_edges_within_24$spatial)

dmin_26 <- min(nests_sim_edges_within_26$spatial)
dmax_26 <- max(nests_sim_edges_within_26$spatial)

nests_sim_edges_within$spatial_weight <- (dmax - nests_sim_edges_within$spatial) / (dmax - dmin)

nests_sim_edges_within_24$spatial_weight <- (dmax_24 - nests_sim_edges_within_24$spatial) / (dmax_24 - dmin_26)
nests_sim_edges_within_26$spatial_weight <- (dmax_26 - nests_sim_edges_within_26$spatial) / (dmax_24 - dmin_26)

edge_list_prospect$dyad_ID <- ifelse(edge_list_prospect$from < edge_list_prospect$to, paste(edge_list_prospect$from, edge_list_prospect$to, sep = "_"), paste(edge_list_prospect$to, edge_list_prospect$from, sep = "_"))

nests_sim_edges_within$prospect_weight <- edge_list_prospect$prospect_weight_binary[match(nests_sim_edges_within$dyad_ID, edge_list_prospect$dyad_ID)]
nests_sim_edges_within$prospect_weight <- ifelse(is.na(nests_sim_edges_within$prospect_weight), 0, 1)

nests_sim_edges_within_24$prospect_weight <- edge_list_prospect$prospect_weight_binary[match(nests_sim_edges_within_24$dyad_ID, edge_list_prospect$dyad_ID)]
nests_sim_edges_within_24$prospect_weight <- ifelse(is.na(nests_sim_edges_within_24$prospect_weight), 0, 1)

#Take only one observation per dyad (instead of duplicates)
nests_sim_edges <- nests_sim_edges %>%
  group_by(dyad_ID) %>% 
  slice(1:1)

nests_sim_edges_24 <- nests_sim_edges_24 %>%
  group_by(dyad_ID) %>% 
  slice(1:1)

nests_sim_edges_26 <- nests_sim_edges_26 %>%
  group_by(dyad_ID) %>% 
  slice(1:1)

nests_sim_edges_within <- nests_sim_edges_within %>%
  group_by(dyad_ID) %>% 
  slice(1:1)

nests_sim_edges_within_24 <- nests_sim_edges_within_24 %>%
  group_by(dyad_ID) %>% 
  slice(1:1)

nests_sim_edges_within_26 <- nests_sim_edges_within_26 %>%
  group_by(dyad_ID) %>% 
  slice(1:1)

#Add previous ownership
LH_owners_females24 <- subset(LH_owners_females, year == 2024)
LH_owners_males24 <- subset(LH_owners_males, year == 2024)

nests_sim_edges_within$nest1_fem_prev_owner <- LH_owners_females24$prev_ownership[match(nests_sim_edges_within$nest1, LH_owners_females24$BOX)]
nests_sim_edges_within$nest1_male_prev_owner <- LH_owners_males24$prev_ownership[match(nests_sim_edges_within$nest1, LH_owners_males24$BOX)]

nests_sim_edges_within$nest2_fem_prev_owner <- LH_owners_females24$prev_ownership[match(nests_sim_edges_within$nest2, LH_owners_females24$BOX)]
nests_sim_edges_within$nest2_male_prev_owner <- LH_owners_males24$prev_ownership[match(nests_sim_edges_within$nest2, LH_owners_males24$BOX)]

nests_sim_edges_within$nest1_prev_owner <- ifelse(nests_sim_edges_within$nest1_fem_prev_owner + nests_sim_edges_within$nest1_male_prev_owner == 0, 0, 1)
nests_sim_edges_within$nest2_prev_owner <- ifelse(nests_sim_edges_within$nest2_fem_prev_owner + nests_sim_edges_within$nest2_male_prev_owner == 0, 0, 1)

nests_sim_edges_within$nests_prev_owner <- nests_sim_edges_within$nest1_prev_owner + nests_sim_edges_within$nest2_prev_owner

boxplot(nests_sim_edges_within$weight ~ nests_sim_edges_within$nests_prev_owner)
tapply(nests_sim_edges_within$weight, nests_sim_edges_within$nests_prev_owner, FUN = mean, na.rm = T)

#Scale variables
nests_sim_edges_within$spatial_weight_z <- scale(nests_sim_edges_within$spatial_weight)
nests_sim_edges_within$prospect_weight_z <- scale(nests_sim_edges_within$prospect_weight)

nests_sim_edges_within_24$spatial_weight_z <- scale(nests_sim_edges_within_24$spatial_weight)
nests_sim_edges_within_24$prospect_weight_z <- scale(nests_sim_edges_within_24$prospect_weight)

nests_sim_edges_within_26$spatial_weight_z <- scale(nests_sim_edges_within_26$spatial_weight)

plot(nests_sim_edges$spatial_weight, nests_sim_edges$weight)
boxplot(nests_sim_edges$weight ~ nests_sim_edges$site_comb)

#For beta model: change 0 and 1 minimally 
nests_sim_edges$weight[nests_sim_edges$weight ==1] <- 0.999
nests_sim_edges$weight[nests_sim_edges$weight ==0] <- 0.001

#Data 2024
#nests_sim_edges24 <- nests_sim_edges
#nests_sim_edges24$year <- 2024
#nests_sim_edges_within24 <- nests_sim_edges_within
#nests_sim_edges_within24$year <- 2024

#nests_sim_edges26 <- nests_sim_edges
#nests_sim_edges26$year <- 2026
#nests_sim_edges_within26 <- nests_sim_edges_within
#nests_sim_edges_within26$year <- 2026

nests_sim_edges_24$year <- 2024
nests_sim_edges_within_24$year <- 2024

nests_sim_edges_26$year <- 2026
nests_sim_edges_within_26$year <- 2026

nests_sim_edges <- rbind(nests_sim_edges_24, nests_sim_edges_26)
nests_sim_edges_within <- rbind(nests_sim_edges_within_24, nests_sim_edges_within_26)

#Model across study sites 
nests_sim_edges$site_same_semi_other <- factor(nests_sim_edges$site_same_semi_other, levels = c("same", "semi", "other"))
nests_sim_edges$year <- as.factor(nests_sim_edges$year)

#Model just with prior
default_prior()

nests_sim_brm1_prior <- brm(weight ~ site_same_semi_other + 
                        (1|mm(nest1,nest2)), 
                      data = nests_sim_edges, 
                      family = gaussian(),
                      prior = c(
                        prior(normal(0.6, 0.5), class = "Intercept"),        
                        prior(normal(0, 0.5), class = "b"),
                        prior(normal(0, 0.15), class = "sigma")),
                      iter = 6000, warmup = 2000,
                      sample_prior = "only"
)


#Prior predictive checks 
pp_check(nests_sim_brm1_prior, ndraws = 100)

#Model 
nests_sim_brm1 <- brm(weight ~ site_same_semi_other + year +
                      (1|mm(nest1,nest2)), 
                      data = nests_sim_edges, 
                      family = gaussian(),
                      prior = c(
                        prior(normal(0.6, 0.5), class = "Intercept"),        
                        prior(normal(0, 0.5), class = "b"),
                        prior(normal(0, 0.15), class = "sigma")),
                      iter = 8000, warmup = 2000,
                      save_pars = save_pars(all = TRUE)
)

#Gaussian model better than Beta

nests_sim_brm1

check_collinearity(nests_sim_brm1)

summary(nests_sim_brm1)

#Posterior predictive checks
pp_check(nests_sim_brm1, type = "dens_overlay", ndraws = 100)

#Plot model
plot(nests_sim_brm1)
launch_shinystan(nests_sim_brm1)

#Posterior distribution
as_draws_df(nests_sim_brm1)
mcmc_areas(nests_sim_brm1)
mcmc_intervals(nests_sim_brm1)

#Evaluation and interpretation
loo(nests_sim_brm1, moment_match = TRUE)
nests_sim_brm1_gauss_loo <- loo(nests_sim_brm1_gauss, moment_match = TRUE)
nests_sim_brm1_beta_loo <- loo(nests_sim_brm1_beta, moment_match = TRUE)
loo_compare(nests_sim_brm1_gauss_loo, nests_sim_brm1_beta_loo)

fitted(nests_sim_brm1, scale = "response")
conditional_effects(nests_sim_brm1)
bayes_R2(nests_sim_brm1)

#Sensitivity analysis and prior checks 
prior_summary(nests_sim_brm1)

#Extract predictions
fitted(nests_sim_brm1)

hypothesis(nests_sim_brm1, "site_same_semi_othersemi < 0")
hypothesis(nests_sim_brm1, "site_same_semi_otherother < 0")
hypothesis(nests_sim_brm1, "site_same_semi_othersemi - site_same_semi_otherother > 0")

hypothesis(nests_sim_brm1, "site_same_semi_othersemi > 0")
hypothesis(nests_sim_brm1, "site_same_semi_othersame > 0")
hypothesis(nests_sim_brm1, "site_same_semi_othersame - site_same_semi_othersemi > 0")

emmeans(nests_sim_brm1, ~ site_same_semi_other, type = "response") |> pairs()

posterior_marginal <- nests_sim_edges %>%
  add_epred_draws(
    object = nests_sim_brm1,
    re_formula = NA)

#Posterior summary for text reporting 
posterior_draw_nests_sim <- posterior_marginal %>%
  group_by(.draw, site_same_semi_other) %>%
  summarise(
    epred = mean(.epred),
    .groups = "drop"
  )

posterior_summary <- posterior_draw_nests_sim %>%
  group_by(site_same_semi_other) %>%
  median_qi(epred, .width = c(0.5, 0.8, 0.95))
posterior_summary

#Contrasts for text reporting
pairwise_contrasts <- posterior_draw_nests_sim %>%
  group_by(.draw) %>%
  compare_levels(epred, by = site_same_semi_other) %>%
  rename(
    contrast = site_same_semi_other,
    diff = epred
  )

pairwise_contrasts_summary <- pairwise_contrasts %>%
  group_by(contrast) %>%
  summarise(
    median = median(diff),
    mean = mean(diff),
    lower_80 = quantile(diff, 0.1),
    upper_80 = quantile(diff, 0.9),
    lower_95 = quantile(diff, 0.025),
    upper_95 = quantile(diff, 0.975),
    P_gt_0 = mean(diff > 0),
    .groups = "drop"
  ) %>%
  arrange(contrast)
pairwise_contrasts_summary

nests_sim_halfeye_plot <- ggplot(posterior_draw_nests_sim, aes(y = site_same_semi_other, x = epred)) +
  stat_halfeye(.width = c(0.5, 0.8, 0.95), fill = "grey") +
  theme_few(base_size = 14) +
  theme(
    legend.position = "none",
    text = element_text(family = "Garamond")) +
  labs(x = "Similarity in material selection", y = "Study site") +
  scale_y_discrete(labels=c("Same", "Semi-connected", "Non-connected")) 
nests_sim_halfeye_plot

nests_sim_halfeye_plot <- ggplot(posterior_draw_nests_sim, aes(y = site_same_semi_other, x = epred)) +
  stat_halfeye(.width = c(0.5, 0.8, 0.95), slab_colour = "black", interval_colour = "black", fill = "white") +
  theme_few(base_size = 14) +
  theme(
    legend.position = "none",
    text = element_text(family = "Garamond")) +
  labs(x = "Similarity in material selection", y = "Study site") +
  scale_y_discrete(labels=c("Same", "Semi-connected", "Non-connected")) 
nests_sim_halfeye_plot

ce <- conditional_effects(nests_sim_brm1, effects = "site_same_semi_other", re_formula = NA)
newdat <- as.data.frame(ce[["site_same_semi_other"]]) %>%
  dplyr::select(site_same_semi_other)
posterior <- posterior_epred(nests_sim_brm1, newdata = ce[["site_same_semi_other"]], re_formula = NA)
posterior <- posterior_predict(nests_sim_brm1, newdata = ce[["site_same_semi_other"]], re_formula = NA)

# Convert to long format
posterior_long <- as.data.frame(posterior) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "draw", values_to = "pred") %>%
  mutate(row = as.numeric(gsub("V", "", draw))) %>%
  left_join(newdat %>% mutate(row = row_number()), by = "row")

nests_sim_plot1 <- ggplot(posterior_long, aes(x = site_same_semi_other, y = pred, fill= site_same_semi_other)) +
  geom_violin(alpha = 0.6, position = position_dodge(width = 0.3), width = 0.5) +
  scale_fill_manual(values =c("white", "white", "white")) +
  theme_few(base_size = 14) +
  theme(legend.position="none", text = element_text(size = 14, family = "Garamond")) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.8)) +
  labs(
    x = "Study site",
    y = "Similarity in material selection") +
  scale_x_discrete(labels=c("Same", "Semi-connected", "Non-connected")) +
  scale_y_continuous(limits = c(0.55,0.75))
nests_sim_plot1


#Model within study sites 

#Model just with prior
default_prior()

nests_sim_brm2_prior <- brm(weight ~ spatial_weight + prospect_weight + site_comb + (1|mm(nest1,nest2)), 
                            data = nests_sim_edges_within, 
                            family = gaussian(),
                            prior = c(
                              prior(normal(0.6, 0.5), class = "Intercept"),        
                              prior(normal(0, 0.5), class = "b"),
                              prior(normal(0, 0.15), class = "sigma")),
                            sample_prior = "only"
)

nests_sim_brm2_prior <- brm(weight ~ spatial_weight + prospect_weight +  site_comb + 
                            (1|mm(nest1,nest2)), 
                            data = nests_sim_edges_within, 
                            family = Beta(link = "logit"),
                            prior = c(
                              prior(normal(0.85, 0.2), class = "Intercept"),        
                              prior(normal(0, 0.2), class = "b"),
                              prior(gamma(25, 1), class = "phi")),
                            sample_prior = "only"
)

#Prior predictive checks 
pp_check(nests_sim_brm2_prior, ndraws = 100)

#Model
nests_sim_edges_within$nests_prev_owner <- factor(nests_sim_edges_within$nests_prev_owner, levels = c(0,1,2))

nests_sim_edges_within$year <- factor(nests_sim_edges_within$year)

nests_sim_edges_within$site_comb <- factor(nests_sim_edges_within$site_comb)
nests_sim_edges_within_24$site_comb <- factor(nests_sim_edges_within_24$site_comb)

#prospect_weight
#nests_prev_owner

nests_sim_brm2 <- brm(weight ~ spatial_weight + site_comb + year + 
                      (1|mm(nest1,nest2)), 
                      data = nests_sim_edges_within, 
                      family = gaussian(),
                      prior = c(
                        prior(normal(0.6, 0.5), class = "Intercept"),        
                        prior(normal(0, 0.5), class = "b"),
                        prior(normal(0, 0.15), class = "sigma")),
                      iter = 6000, warmup = 2000,
                      save_pars = save_pars(all = TRUE)
)

nests_sim_brm3 <- brm(weight ~ prospect_weight + site_comb +
                        (1|mm(nest1,nest2)), 
                      data = nests_sim_edges_within_24, 
                      family = gaussian(),
                      prior = c(
                        prior(normal(0.6, 0.5), class = "Intercept"),        
                        prior(normal(0, 0.5), class = "b"),
                        prior(normal(0, 0.15), class = "sigma")),
                      iter = 6000, warmup = 2000,
                      save_pars = save_pars(all = TRUE)
)

#Gaussian model better than Beta

nests_sim_brm2

check_collinearity(nests_sim_brm2)

summary(nests_sim_brm2)
summary(nests_sim_brm3)

#Posterior predictive checks
pp_check(nests_sim_brm2, type = "dens_overlay", ndraws = 100)
pp_check(nests_sim_brm3, type = "dens_overlay", ndraws = 100)

#Plot model
plot(nests_sim_brm2)
launch_shinystan(nests_sim_brm2)

#Posterior distribution
as_draws_df(nests_sim_brm2)
mcmc_areas(nests_sim_brm2)
mcmc_intervals(nests_sim_brm2)

#Evaluation and interpretation
loo(nests_sim_brm2, moment_match = TRUE)
nests_sim_brm2_gauss_loo <- loo(nests_sim_brm2_gauss, moment_match = TRUE)
nests_sim_brm2_beta_loo <- loo(nests_sim_brm2_beta, moment_match = TRUE)
loo_compare(nests_sim_brm2_gauss_loo, nests_sim_brm2_beta_loo)

fitted(nests_sim_brm2, scale = "response")
conditional_effects(nests_sim_brm2)
bayes_R2(nests_sim_brm3)

#Sensitivity analysis and prior checks 
prior_summary(nests_sim_brm2)

#Extract predictions
fitted(nests_sim_brm2)

hypothesis(nests_sim_brm2, "spatial_weight > 0")
hypothesis(nests_sim_brm3, "prospect_weight > 0")
hypothesis(nests_sim_brm2, "site_combYY > 0")
hypothesis(nests_sim_brm2, "site_combZZ > 0")

emmeans(nests_sim_brm2, ~ site_comb, type = "response") |> pairs()
emmeans(nests_sim_brm2, ~ nests_prev_owner, type = "response") |> pairs()

cond <- conditional_effects(nests_sim_brm2, effect = "spatial_weight")
p <- plot(cond, points=F)
nests_sim_plot2 <- p[[1]] + 
  theme_few(base_size = 14) +
  theme(legend.position="none", text = element_text(size = 14, family = "Garamond")) +
  geom_point(
    aes(x = spatial_weight, y = weight), 
    data = nests_sim_edges_within, 
    color = "black",
    size = 2,
    alpha = 0.1,
    inherit.aes = FALSE) + 
  geom_line(color="black", size=1) + 
  labs(x = "Spatial weight", y = "Similarity in material selection")
nests_sim_plot2 

nests_sim_brm2 %>%
  spread_draws(b_Intercept, b_spatial_weight) %>%
  ggplot(aes(y =, x = b_spatial_weight)) +
  theme_classic(base_size = 28) +
  theme(legend.position="none", text = element_text(size = 28, family = "Garamond")) +
  labs(x = "Est (Spatial weight)", y = "Density") +
  stat_halfeye()

nests_sim_plot <- ggarrange(nests_sim_plot1, nests_sim_plot2, ncol = 2, nrow = 1, labels = c("(a)", "(b)"),  widths = c(1.5, 2), font.label = list(
  family = "Garamond",
  face = "plain",   
  size = 14,
  color = "black"
))

nests_sim_plot <- ggarrange(nests_sim_halfeye_plot, nests_sim_plot2, ncol = 2, nrow = 1, labels = c("(a)", "(b)"),  widths = c(1.5, 2), font.label = list(
  family = "Garamond",
  face = "plain",   
  size = 14,
  color = "black"
))

nests_sim_plot

#Network between materials
mat_t <- t(mat)

mat_jac <- 1 - as.matrix(vegdist(mat_t, method = "jaccard", binary = TRUE))


#3. Anthropogenic material ---- 

nest_obs_direct_materials$pair_age_2_z <- scale(nest_obs_direct_materials$pair_age_2, center = T, scale = F)

nest_obs_direct_materials_2426$Year <- factor(nest_obs_direct_materials_2426$Year)

#Model just with prior
default_prior()

anthro_brm1_prior <- brm(Anthropogenic_binary ~ Site,  
                               data = nest_obs_direct_materials, 
                               family = bernoulli(link = "logit"),
                               prior = c(
                                 prior(normal(0, 0.5), class = "b"),        
                                 prior(normal(0, 1), class = "Intercept")),
                               sample_prior = "only"
)

#Prior predictive checks 
pp_check(anthro_brm1_prior, ndraws = 100)

#Model
anthro_brm1 <- brm(Anthropogenic_binary ~ Site + Year + (1|Nestbox),  
                   data = nest_obs_direct_materials_2426, 
                   family = bernoulli(link = "logit"),
                   prior = c(
                     prior(normal(0, 0.5), class = "b"),        
                     prior(normal(0, 1), class = "Intercept"))
)

#what about pair_age_2_z + I(pair_age_2_z ^2)

summary(anthro_brm1)

check_collinearity(anthro_brm1)

#Posterior predictive checks
pp_check(anthro_brm1, type = "dens_overlay", ndraws = 100)

#Plot model
plot(anthro_brm1)
launch_shinystan(anthro_brm1)

#Posterior distribution
as_draws_df(anthro_brm1)
mcmc_areas(anthro_brm1)
mcmc_intervals(anthro_brm1)

#Evaluation and interpretation
loo(anthro_brm1, moment_match = TRUE)
fitted(anthro_brm1, scale = "response")
conditional_effects(anthro_brm1)
bayes_R2(anthro_brm1)

#Sensitivity analysis and prior checks 
prior_summary(anthro_brm1)

#Extract predictions
fitted(anthro_brm1)

emmeans(anthro_brm1, ~ Site, type = "response") |> pairs()

posterior_marginal <- nest_obs_direct_materials_2426 %>%
  add_epred_draws(
    object = anthro_brm1,
    re_formula = NA)

#Posterior summary for text reporting 
posterior_draw_nests_anthro <- posterior_marginal %>%
  group_by(.draw, Site) %>%
  summarise(
    epred = mean(.epred),
    .groups = "drop"
  )

posterior_summary <- posterior_draw_nests_anthro %>%
  group_by(Site) %>%
  median_qi(epred, .width = c(0.5, 0.8, 0.95))
posterior_summary

#Contrasts for text reporting
pairwise_contrasts <- posterior_draw_nests_anthro %>%
  group_by(.draw) %>%
  compare_levels(epred, by = Site) %>%
  rename(
    contrast = Site,
    diff = epred
  )

pairwise_contrasts_summary <- pairwise_contrasts %>%
  group_by(contrast) %>%
  summarise(
    median = median(diff),
    mean = mean(diff),
    lower_80 = quantile(diff, 0.1),
    upper_80 = quantile(diff, 0.9),
    lower_95 = quantile(diff, 0.025),
    upper_95 = quantile(diff, 0.975),
    P_gt_0 = mean(diff > 0),
    .groups = "drop"
  ) %>%
  arrange(contrast)
pairwise_contrasts_summary

#Anthropogenic material: dyadic comparison

nests_sim_edges$nest1_anthro <- nest_obs_direct_materials$Anthropogenic_binary[match(nests_sim_edges$nest1, nest_obs_direct_materials$Box)]
nests_sim_edges$nest2_anthro <- nest_obs_direct_materials$Anthropogenic_binary[match(nests_sim_edges$nest2, nest_obs_direct_materials$Box)]
nests_sim_edges$anthro <- nests_sim_edges$nest1_anthro + nests_sim_edges$nest2_anthro
nests_sim_edges$anthro_binary <- ifelse(nests_sim_edges$anthro == 1, 0, 1)

nests_sim_edges_24$nest1_anthro <- nest_obs_direct_materials$Anthropogenic_binary[match(nests_sim_edges_24$nest1, nest_obs_direct_materials$Box)]
nests_sim_edges_24$nest2_anthro <- nest_obs_direct_materials$Anthropogenic_binary[match(nests_sim_edges_24$nest2, nest_obs_direct_materials$Box)]
nests_sim_edges_24$anthro <- nests_sim_edges_24$nest1_anthro + nests_sim_edges_24$nest2_anthro
nests_sim_edges_24$anthro_binary <- ifelse(nests_sim_edges_24$anthro == 1, 0, 1)

nests_sim_edges_26$nest1_anthro <- nest_obs_direct_materials$Anthropogenic_binary[match(nests_sim_edges_26$nest1, nest_obs_direct_materials$Box)]
nests_sim_edges_26$nest2_anthro <- nest_obs_direct_materials$Anthropogenic_binary[match(nests_sim_edges_26$nest2, nest_obs_direct_materials$Box)]
nests_sim_edges_26$anthro <- nests_sim_edges_26$nest1_anthro + nests_sim_edges_26$nest2_anthro
nests_sim_edges_26$anthro_binary <- ifelse(nests_sim_edges_26$anthro == 1, 0, 1)

nests_sim_edges <- rbind(nests_sim_edges_24, nests_sim_edges_26)

nests_sim_edges_within$nest1_anthro <- nest_obs_direct_materials$Anthropogenic_binary[match(nests_sim_edges_within$nest1, nest_obs_direct_materials$Box)]
nests_sim_edges_within$nest2_anthro <- nest_obs_direct_materials$Anthropogenic_binary[match(nests_sim_edges_within$nest2, nest_obs_direct_materials$Box)]
nests_sim_edges_within$anthro <- nests_sim_edges_within$nest1_anthro + nests_sim_edges_within$nest2_anthro
nests_sim_edges_within$anthro_binary <- ifelse(nests_sim_edges_within$anthro == 1, 0, 1)

nests_sim_edges_within_24$nest1_anthro <- nest_obs_direct_materials$Anthropogenic_binary[match(nests_sim_edges_within_24$nest1, nest_obs_direct_materials$Box)]
nests_sim_edges_within_24$nest2_anthro <- nest_obs_direct_materials$Anthropogenic_binary[match(nests_sim_edges_within_24$nest2, nest_obs_direct_materials$Box)]
nests_sim_edges_within_24$anthro <- nests_sim_edges_within_24$nest1_anthro + nests_sim_edges_within_24$nest2_anthro
nests_sim_edges_within_24$anthro_binary <- ifelse(nests_sim_edges_within_24$anthro == 1, 0, 1)

nests_sim_edges_within_26$nest1_anthro <- nest_obs_direct_materials$Anthropogenic_binary[match(nests_sim_edges_within_26$nest1, nest_obs_direct_materials$Box)]
nests_sim_edges_within_26$nest2_anthro <- nest_obs_direct_materials$Anthropogenic_binary[match(nests_sim_edges_within_26$nest2, nest_obs_direct_materials$Box)]
nests_sim_edges_within_26$anthro <- nests_sim_edges_within_26$nest1_anthro + nests_sim_edges_within_26$nest2_anthro
nests_sim_edges_within_26$anthro_binary <- ifelse(nests_sim_edges_within_26$anthro == 1, 0, 1)

nests_sim_edges_within <- rbind(nests_sim_edges_within_24, nests_sim_edges_within_26)

nests_sim_edges$nest1_long <- box_coordinates$long[match(nests_sim_edges$nest1, box_coordinates$Box)]
nests_sim_edges$nest1_lat <- box_coordinates$lat[match(nests_sim_edges$nest1, box_coordinates$Box)]
nests_sim_edges$nest2_long <- box_coordinates$long[match(nests_sim_edges$nest2, box_coordinates$Box)]
nests_sim_edges$nest2_lat <- box_coordinates$lat[match(nests_sim_edges$nest2, box_coordinates$Box)]

table(nests_sim_edges$anthro)

#Model across sites 

#Model just with prior
default_prior()
anthro_brm2_prior <- brm(anthro | trials(2) ~ spatial_weight + site_comb +
                          (1|mm(nest1,nest2)), 
                          data = nests_sim_edges, 
                          family = binomial(link = "logit"),
                          prior = c(
                            set_prior("normal(0.85, 1)", class = "Intercept"),        
                            set_prior("normal(0, 0.5)", class = "b")),
                          sample_prior = "only"
)

anthro_brm2_prior <- brm(anthro_binary ~ site_same_semi_other + 
                   (1|mm(nest1,nest2)), 
                   data = nests_sim_edges, 
                   family = bernoulli(link = "logit"),
                   prior = c(
                     prior(normal(0, 1), class = "Intercept"),        
                     prior(normal(0, 0.5), class = "b")),
                   sample_prior = "only"
)

#Prior predictive checks 
pp_check(anthro_brm2_prior, ndraws = 100)

#Model including
anthro_brm2 <- brm(anthro | trials(2) ~ spatial_weight + prospect_weight + 
                         site_comb + (1|mm(nest1,nest2)), 
                         data = nests_sim_edges, 
                         family = binomial(link = "logit"),
                         prior = c(
                           set_prior("normal(0.85, 1)", class = "Intercept"),        
                           set_prior("normal(0, 0.5)", class = "b"))
)

anthro_brm2 <- brm(anthro_binary ~ site_same_semi_other + 
                   (1|mm(nest1,nest2)), 
                   data = nests_sim_edges, 
                   family = bernoulli(link = "logit"),
                   prior = c(
                     prior(normal(0, 1), class = "Intercept"),        
                     prior(normal(0, 0.5), class = "b")),
)

#coordinates: gp(nest1_long, nest1_lat) + gp(nest2_long, nest2_lat)

summary(anthro_brm2)
check_collinearity(anthro_brm2)

#Posterior predictive checks
pp_check(anthro_brm2, type = "dens_overlay", ndraws = 100)

#Plot model
plot(anthro_brm2)

#Posterior distribution
as_draws_df(anthro_brm2)
mcmc_areas(anthro_brm2)
mcmc_intervals(anthro_brm2)

#Evaluation and interpretation
loo(anthro_brm2)
fitted(anthro_brm2, scale = "response")
conditional_effects(anthro_brm2)
bayes_R2(anthro_brm2)

#Sensitivity analysis and prior checks 
prior_summary(anthro_brm2)

#Extract predictions
fitted(anthro_brm2)


#Model within sites 

#Model just with prior
default_prior()
anthro_brm3_prior <- brm(anthro | trials(2) ~ spatial_weight + prospect_weight + site_comb +
                           (1|mm(nest1,nest2)), 
                         data = nests_sim_edges_within, 
                         family = binomial(link = "logit"),
                         prior = c(
                           set_prior("normal(0.85, 1)", class = "Intercept"),        
                           set_prior("normal(0, 0.5)", class = "b")),
                         sample_prior = "only"
)

anthro_brm3_prior <- brm(anthro_binary ~ spatial_weight + prospect_weight + 
                           site_comb + (1|mm(nest1,nest2)), 
                         data = nests_sim_edges_within, 
                         family = bernoulli(link = "logit"),
                         prior = c(
                           prior(normal(0, 1), class = "Intercept"),        
                           prior(normal(0, 0.5), class = "b")),
                         sample_prior = "only"
)
#Prior predictive checks 
pp_check(anthro_brm3_prior, ndraws = 100)

#Model including
anthro_brm3 <- brm(anthro | trials(2) ~ spatial_weight + prospect_weight + 
                     site_comb + (1|mm(nest1,nest2)), 
                   data = nests_sim_edges, 
                   family = binomial(link = "logit"),
                   prior = c(
                     set_prior("normal(0.85, 1)", class = "Intercept"),        
                     set_prior("normal(0, 0.5)", class = "b"))
)

#model for manuscript to test for dyadic similarity
nests_sim_edges_within$year <- factor(nests_sim_edges_within$year)
nests_sim_edges_within$site_comb <- factor(nests_sim_edges_within$site_comb)

anthro_brm3 <- brm(anthro_binary ~ spatial_weight + site_comb + 
                   year + (1|mm(nest1,nest2)), 
                   data = nests_sim_edges_within, 
                   family = bernoulli(link = "logit"),
                   prior = c(
                     prior(normal(0, 1), class = "Intercept"),        
                     prior(normal(0, 0.5), class = "b")),
)

anthro_brm3 <- brm(anthro_binary ~ spatial_weight + site_comb + prospect_weight
                     year + (1|mm(nest1,nest2)), 
                   data = nests_sim_edges_within_24, 
                   family = bernoulli(link = "logit"),
                   prior = c(
                     prior(normal(0, 1), class = "Intercept"),        
                     prior(normal(0, 0.5), class = "b")),
)

#coordinates: gp(nest1_long, nest1_lat) + gp(nest2_long, nest2_lat)

summary(anthro_brm3)
check_collinearity(anthro_brm3)

#Posterior predictive checks
pp_check(anthro_brm3, type = "dens_overlay", ndraws = 100)

#Plot model
plot(anthro_brm3)

#Posterior distribution
as_draws_df(anthro_brm3)
mcmc_areas(anthro_brm3)
mcmc_intervals(anthro_brm3)

#Evaluation and interpretation
loo(anthro_brm3)
fitted(anthro_brm3, scale = "response")
conditional_effects(anthro_brm3)
bayes_R2(anthro_brm3)

#Sensitivity analysis and prior checks 
prior_summary(anthro_brm3)

#Extract predictions
fitted(anthro_brm3)

hypothesis(anthro_brm3, "spatial_weight > 0")
hypothesis(anthro_brm3, "prospect_weight > 0")
hypothesis(anthro_brm3, "site_combYY > 0")
hypothesis(anthro_brm3, "site_combZZ > 0")

emmeans(anthro_brm3, ~ site_comb, type = "response") |> pairs()


#4. Diffusion experiment ----

#Load data
nestboxes2026 <- read.csv("Data/nestboxes_2026.csv")
nestboxes2026 <- subset(nestboxes2026, present == 1)

diff_exp <- read.csv("Data/diff_exp.csv")
diff_exp_loc <- read.csv("Data/diff_exp_loc.csv")

#Only include data that is likely from experiment 
#diff_exp <- subset(diff_exp, diff_exp$exp_material == 1)

#Only include data with clear evidence for observation
diff_exp <- subset(diff_exp, diff_exp$evidence == 1)

diff_exp <- diff_exp %>%
  group_by(trial, id) %>%
  slice(1)

diff_exp <- subset(diff_exp, !id == "Z46")

diff_exp1 <- subset(diff_exp, trial == 1)

#diff_exp1[18:86,] <- NA
diff_exp1[21:86,] <- NA
#diff_exp1[22:86,] <- NA
#diff_exp1[23:86,] <- NA

diff_exp2 <- subset(diff_exp, trial == 2)
diff_exp2 [16:65,] <- NA

#Create event data
event_data1 <- diff_exp1[, c("time", "id", "trial", "t_end")]
event_data2 <- diff_exp2[, c("time", "id", "trial", "t_end")]

#event_data <- event_data %>%
#  group_by(id) %>%
#  slice(1)

#event_data <- event_data[-c(15,21),] #remove 2nd obs for Z16 and Z17 
#event_data <- event_data[order(event_data$id),]

nests_summary$diff_exp <- diff_exp1$time[match(nests_summary$Box, diff_exp1$id)]
nests_no_diff1 <- nests_summary$Box[is.na(nests_summary$diff_exp)]

# Start from all boxes
event_data1 <- data.frame(
  id = nests_summary$Box,
  trial = 1
)

# Join observed times
event_data1$time <- diff_exp1$time[
  match(event_data1$id, diff_exp1$id)
]

# Fill non-observed
event_data1$time[is.na(event_data1$time)] <- 57
event_data1$t_end <- 56

#event_data1$id[17:86] <- nests_no_diff
#event_data1$id[21:86] <- nests_no_diff1
#event_data1$id[22:86] <- nests_no_diff
#event_data1$id[23:86] <- nests_no_diff
#event_data1$time <- ifelse(!is.na(event_data1$time), event_data1$time, 54)
#event_data1$trial <- 1
#event_data1$t_end <- 56

event_data1 <- subset(event_data1, !id == "Z25")
event_data1 <- subset(event_data1, !id == "Z46")

nestboxes2026$diff_exp <- diff_exp2$time[match(nestboxes2026$Box, diff_exp2$id)]
nests_no_diff2 <- nestboxes2026$Box[is.na(nestboxes2026$diff_exp)]

# Start from all boxes
event_data2 <- data.frame(
  id = nestboxes2026$Box,
  trial = 2
)

# Join observed times
event_data2$time <- diff_exp2$time[
  match(event_data2$id, diff_exp2$id)
]

# Fill non-observed
event_data2$time[is.na(event_data2$time)] <- 57
event_data2$t_end <- 56

#event_data2$id[16:65] <- nests_no_diff2

#event_data2$time <- ifelse(!is.na(event_data2$time), event_data2$time, 54)
#event_data2$trial <- 2
#event_data2$t_end <- 56

event_data <- rbind(event_data1, event_data2)

event_data_exp <- event_data

event_data <- rbind(event_data1, event_data1)
event_data <- rbind(event_data2, event_data2)
event_data$trial[85:168] <- 2
event_data$trial[1:65] <- 1

rownames(event_data) <- seq_len(nrow(event_data))

trials <- unique(event_data$trial)
all_ids <- unique(event_data$id)

event_data_full <- expand.grid(
  id = all_ids,
  trial = trials
)

event_data_full <- merge(
  event_data_full,
  event_data,
  by = c("id", "trial"),
  all.x = TRUE
)

event_data_full$t_end <- 56

event_data_full$time <- ifelse(
  is.na(event_data_full$time),
  event_data_full$t_end + 1,
  event_data_full$time
)

event_data <- event_data_full

all_ids <- unique(event_data$id)

event_data_fixed <- event_data %>%
  complete(trial, id = all_ids)

event_data_fixed <- event_data_fixed %>%
  group_by(trial) %>%
  mutate(
    t_end = max(t_end, na.rm = TRUE),
    time = ifelse(is.na(time), t_end + 1, time)
  ) %>%
  ungroup()

event_data$time <- as.integer(event_data$time)
event_data$t_end <- as.integer(event_data$t_end)

orig_times <- event_data$time

event_data_perm <- event_data
event_data_perm$time <- sample(orig_times, replace = FALSE)

different_boxes <- setdiff(event_data1$id, event_data2$id)

#All obs with real evidence and likely experimental   

#event_data$time[event_data$id == "Y06"] <- 0
#event_data$time[event_data$id == "Y02"] <- 0
#event_data$time[event_data$id == "Z23"] <- 0
#event_data$time[event_data$id == "Z35"] <- 0

#event_data$time[event_data$id == "Y02"] <- 53
#event_data$time[event_data$id == "Z41"] <- 53

#Edge list for 2024 only (start with spatial data)

#Spatial distance matrix
edge_list_spatial <- distance[, c("InputID", "TargetID", "Distance", "SiteComb")]

#Only consider distances within study sites
edge_list_spatial <- subset(edge_list_spatial, edge_list_spatial$SiteComb == "XX" | edge_list_spatial$SiteComb == "YY" | edge_list_spatial$SiteComb == "ZZ")

#Remove weaker spatial connections (more conservative)
median(edge_list_spatial$Distance) #100 m
mean(edge_list_spatial$Distance) #118 m
sd(edge_list_spatial$Distance) #79 m

#edge_list_spatial <- subset(edge_list_spatial, Distance < 100)
#edge_list_spatial <- subset(edge_list_spatial, Distance < 200)

#Minimum and maximum distances between boxes
dmin <- min(edge_list_spatial$Distance)
dmax <- max(edge_list_spatial$Distance)

#Same trial 
edge_list_spatial$trial <- 1

#Add spatial weight, standardised from 0 to 1
edge_list_spatial$spatial_weight <- (dmax - edge_list_spatial$Distance) / (dmax - dmin)

#Remove Distance and SiteComb
edge_list_spatial <- edge_list_spatial[, c("InputID", "TargetID", "trial", "spatial_weight")]

#Rename nodes
edge_list_spatial <- edge_list_spatial %>% rename(from = InputID)
edge_list_spatial <- edge_list_spatial  %>% rename(to = TargetID)

#Filter edge list to include boxes in event_data
edge_list_spatial <- edge_list_spatial %>%
  filter(from %in% event_data1$id)

edge_list_spatial <- edge_list_spatial %>%
  filter(to %in% event_data1$id)

#Separate networks for both years
edge_list_spatial1 <- edge_list_spatial

edge_list_spatial2 <- edge_list_spatial %>%
  filter(from %in% event_data2$id)

edge_list_spatial2 <- edge_list_spatial2 %>%
  filter(to %in% event_data2$id)

edge_list_spatial2$trial <- 2

edge_list <- rbind(edge_list_spatial1, edge_list_spatial2)

edge_list_exp <- edge_list

edge_list[4115:4133,] <- NA
edge_list$from[4115:4133] <- different_boxes
edge_list$to[4115:4133] <- different_boxes
edge_list$trial[4115:4133] <- 2
edge_list$spatial_weight[4115:4133] <- 0

edge_list <- rbind(edge_list_spatial1, edge_list_spatial1)
edge_list <- rbind(edge_list_spatial2, edge_list_spatial2)

edge_list$trial[2571:5140] <- 2
edge_list$trial[1:1544] <- 1

rownames(edge_list) <- seq_len(nrow(edge_list))

edge_full <- expand.grid(
  trial = trials,
  from = all_ids,
  to   = all_ids
)

edge_list_full <- merge(
  edge_full,
  edge_list,
  by = c("trial", "from", "to"),
  all.x = TRUE
)

edge_list_full$spatial_weight[is.na(edge_list_full$spatial_weight)] <- 0

#Add box dyad ID
edge_list_spatial$box_dyad_ID <- paste(edge_list_spatial$from, edge_list_spatial$to, sep = "_")
edge_list_spatial1$box_dyad_ID <- paste(edge_list_spatial1$from, edge_list_spatial$to, sep = "_")
edge_list$box_dyad_ID <- ifelse(edge_list$from < edge_list$to, paste(edge_list$from, edge_list$to, sep = "_"), paste(edge_list$to, edge_list$from, sep = "_"))

edge_list <- edge_list %>% 
  group_by(box_dyad_ID, trial) %>%
  slice(1)


#Prospecting network 

#Only box owners visiting each other
visits_prospect_owners <- subset(visits_prospect, !is.na(visitor_owned_box))

#Box dyad ID
visits_prospect_owners$box_dyad_ID <- paste(visits_prospect_owners$box, visits_prospect_owners$visitor_owned_box, sep = "_")

#Summary dataset for all dyads
visits_prospect_owners_box_dyads <- as.data.frame(table(visits_prospect_owners$box_dyad_ID))

#Rename variables
visits_prospect_owners_box_dyads <- visits_prospect_owners_box_dyads %>% rename(box_dyad_ID = Var1)
visits_prospect_owners_box_dyads <- visits_prospect_owners_box_dyads %>% rename(prospect_weight = Freq)

#Add nodes
visits_prospect_owners_box_dyads$from <- visits_prospect_owners$box[match(visits_prospect_owners_box_dyads$box_dyad_ID, visits_prospect_owners$box_dyad_ID)]
visits_prospect_owners_box_dyads$to <- visits_prospect_owners$visitor_owned_box[match(visits_prospect_owners_box_dyads$box_dyad_ID, visits_prospect_owners$box_dyad_ID)]

#Edge list for prospecting
edge_list_prospect <- visits_prospect_owners_box_dyads

#Same trial
edge_list_prospect$trial <- 1

#Binary edge list for prospecting
edge_list_prospect$prospect_weight_binary <- 1

#Full edge list 
edge_list <- edge_list_spatial1

#Binary prospecting weight
edge_list$prospect_weight <- edge_list_prospect$prospect_weight_binary[match(edge_list$box_dyad_ID, edge_list_prospect$box_dyad_ID)]

#Alternatively, numeric prospecting weight
edge_list$prospect_weight <- edge_list_prospect$prospect_weight[match(edge_list$box_dyad_ID, edge_list_prospect$box_dyad_ID)]

#Remove box dyad ID
edge_list$box_dyad_ID <- NULL

#Fill missing prospect edges as 0 (i.e., no prospecting)
edge_list$prospect_weight <- ifelse(is.na(edge_list$prospect_weight), 0, edge_list$prospect_weight)

#Remove prospecting network
edge_list <- edge_list[, c("from", "to", "trial", "spatial_weight")]

event_data_exp24 <- event_data1
edge_list_exp24 <- edge_list

#Add age as ILV
ILV <- data.frame(
  id = nests_summary$Box,
  age = nests_summary$pair_age_z,
  site = nests_summary$SiteNum
)

#Remove boxes with no start date
ILV <- subset(ILV, !id == "Z25")
ILV <- subset(ILV, !id == "Z46")

ILV <- ILV  %>%
  filter(id %in% event_data$id)

#Y12 pair age is missing, set as 0 in scaled data
ILV$age[ILV$id =="Y12"] <- 0
ILV$age[ILV$id =="X36"] <- 0
ILV$age[ILV$id =="X38"] <- 0

#Data list for NBDA

event_data <- event_data[order(event_data$trial, event_data$time), ]
edge_list  <- edge_list[order(edge_list$trial), ]

#Code using event_data and edge_list, so rename event_data_exp and edge_list_exp
event_data <- event_data_exp
edge_list <- edge_list_exp

event_data <- event_data_exp24
edge_list <- edge_list_exp24

#Without transmission weights
data_list_social <- import_user_STb(
  event_data = event_data,
  networks = edge_list)

data_list_social <- import_user_STb(
  event_data = event_data,
  networks = edge_list,
  network_type = "undirected")

#Without transmission weights and with ILVs
data_list_social_ILV <- import_user_STb(
  event_data = event_data,
  networks = edge_list,
  ILV_c = ILV,
  ILVi = c("age"),
  ILVs = c("age")
)

#Permuted data
data_list_social_perm <- import_user_STb(
  event_data = event_data_perm,
  networks = edge_list,
  network_type = "directed")

#Generate social models with and without ILV
model_social <- generate_STb_model(data_list_social, 
                                   gq = T, 
                                   est_acqTime = F, 
                                   data_type = c("discrete_time"))

model_social_perm <- generate_STb_model(data_list_social_perm, 
                                   gq = T, 
                                   est_acqTime = T, 
                                   data_type = c("discrete_time"))

model_social_ILV <- generate_STb_model(data_list_social_ILV, 
                                       gq = T, 
                                       est_acqTime = T, 
                                       data_type = c("discrete_time"))

#Fit social model
model_social_fit <- fit_STb(data_list_social,
                            model_social,
                            parallel_chains = 4,
                            chains = 4,
                            cores = 4,
                            iter = 4000,
                            refresh=1000
)

model_social_perm_fit <- fit_STb(data_list_social_perm,
                            model_social_perm,
                            parallel_chains = 4,
                            chains = 4,
                            cores = 4,
                            iter = 4000,
                            refresh=1000
)

#Fit social model with ILV
model_social_ILV_fit <- fit_STb(data_list_social_ILV,
                                model_social_ILV,
                                parallel_chains = 4,
                                chains = 4,
                                cores = 4,
                                iter = 8000,
                                refresh=1000
)

#Save social models
STb_save(model_social_fit, output_dir = "cmdstan_saves", name="model_social_fit")
STb_save(model_social_perm_fit, output_dir = "cmdstan_saves", name="model_social_fit")
STb_save(model_social_ILV_fit, output_dir = "cmdstan_saves", name="model_social_ILV_fit")

#Summary for social models 
STb_summary(model_social_fit, digits = 3)
STb_summary(model_social_perm_fit, digits = 3)
STb_summary(model_social_ILV_fit, digits = 3)

#Generate asocial models
model_asocial = generate_STb_model(data_list_social, 
                                   model_type="asocial", 
                                   data_type = c("discrete_time"),
                                   est_acqTime = F)

model_asocial_ILV = generate_STb_model(data_list_social_ILV, model_type="asocial", data_type = c("discrete_time"))

#Run asocial models
model_asocial_fit = fit_STb(data_list_social,
                            model_asocial,
                            parallel_chains = 4,
                            chains = 4,
                            cores = 4,
                            iter = 4000,
                            refresh=1000)

model_asocial_ILV_fit = fit_STb(data_list_social_ILV,
                                model_asocial_ILV,
                                parallel_chains = 4,
                                chains = 4,
                                cores = 4,
                                iter = 4000,
                                refresh=1000)

#Summary for asocial models
STb_summary(model_asocial_fit, digits = 3)
STb_summary(model_asocial_ILV_fit, digits = 3)

#Compare models via LOO cross validation

#All four models with and without ILV
loo_output = STb_compare(model_social_fit, model_social_ILV_fit, model_asocial_fit, model_asocial_ILV_fit, method="loo-psis")

#Just two models without ILV
loo_output = STb_compare(model_social_fit, model_asocial_fit, method="loo-psis")

#Print LOO output
print(loo_output$comparison, simplify = FALSE)

RT_files <- list.files("D:/PhD/2024 Feeders and Nest Building/Nest Building Experiment", pattern = "RT", recursive = TRUE)
RT_paths <- paste("D:/PhD/2024 Feeders and Nest Building/Nest Building Experiment",RT_files, sep = "/")

#Create sub-strings that contain feeder ID (e.g. "Y1.1") and date
day_arrays <- unique(substr(RT_files,6,16))

#Create empty list to place data into as we go
collapsed_list <- list()

#Run through RT files in list 
for(i in 1:length(day_arrays)) {
  progress(i, max.value = length(day_arrays))
  
  day_array_list <- list()
  
  for (j in 1:length(RT_files[which(substr(RT_files,6,16) == day_arrays[i])])) {
    temp_day_file <- (read.delim(RT_paths[which(substr(RT_files,6,16) == day_arrays[i])][j], header = T, stringsAsFactors = F))[,1:13]
    temp_day_file$feeder <- substr(RT_files[which(substr(RT_files,6,16) == day_arrays[i])][j],6,9)
    day_array_list[[j]] <- temp_day_file
  }
  
  temp_RT <- do.call(rbind, day_array_list)
  
  temp_RT$Time <- strptime(paste(temp_RT$Date,stri_sub(temp_RT$Hmsec/1024,2,5), sep = ""), "%Y-%m-%d %H:%M:%OS")  # Add in miliseconds (1024 in a second) and format time
  
  temp_RT %>% filter(nchar(TagID_hex) == 10) -> temp_tags  #remove times when no tag
  
  
  if(dim(temp_tags)[1] >0){  
    
    visits <- data.frame(Event = temp_tags$Event, Start = temp_tags$Time, End = temp_tags$Time+(0.5*(temp_tags$Reps -1)), tag = temp_tags$TagID_hex, feeder = temp_tags$feeder)  # adds reps to visit length (0.5 seconds for every extra detection as that was resampling speed)
    visits <- arrange(visits, Start)
    visits <- arrange(visits, feeder)
    within_errors <- which(visits$End < lag(visits$End) & visits$tag == lag(visits$tag) & visits$feeder == lag(visits$feeder))
    if(length(within_errors) > 0){
      visits <- visits[-which(visits$End < lag(visits$End) & visits$tag == lag(visits$tag) & visits$feeder == lag(visits$feeder)),]  ## Get rid of reads within bouts - almost always erroneous single reads that are repeated later in the datastream
    }
    visits <- arrange(visits, Start)
    visits$count <- sapply(1:nrow(visits),function(x)sum(visits$tag[x]==visits$tag[1:x]))  ## add individual visit counter - if two bouts don't have sequential counts then bird seen elsewhere in between
    visits <- arrange(visits, feeder)
    
    visits$collapse <- "0"  # Temp column to tell me if this bout is to be collapsed
    
    visit_time <- 10 ## How long between detections before a new bout is classed
    
    # Is the last visit ending within 'visit_time' seconds of this one starting, and with the same tag & in sequence?
    visits$collapse[ which (visits$Start-lag(visits$End) < visit_time & lead(visits$Start) - visits$End < visit_time & visits$feeder == lag(visits$feeder) & visits$feeder == lead(visits$feeder) & visits$tag == lag(visits$tag) & visits$tag == lead(visits$tag) & (visits$count-lag(visits$count)) == 1 & (lead(visits$count)-visits$count) == 1 )] <- "1"  ## What about times they hop in between?!
    
    visits$collapse[which(visits$collapse == 0 & lag(visits$collapse == 1))] <- "End"  ## Can work out the end based on the 0s and 1s
    visits$collapse[which(visits$collapse == 0 & lead(visits$collapse == 1))] <- "Start" ## Can work out the start based on the 0s and 1s
    
    ## Mop up those that only have two potential detections (so no middle values to get assigned 1 above)
    visits$collapse[which(visits$collapse == 0 & lead(visits$Start) - visits$End < visit_time & lead(visits$Start) - visits$End > -2 & visits$tag == lead(visits$tag) & lead(visits$count) - visits$count == 1)] <- "Start"
    visits$collapse[which(visits$collapse == 0 & visits$Start - lag(visits$End) < visit_time & lag(visits$feeder) == visits$feeder & visits$tag == lag(visits$tag) & lag(visits$count) - visits$count == -1)] <- "End"
    
    # Add this file's data to the list
    collapsed_list[[i]] <- data.frame(start = visits$Start[which(visits$collapse %in% c("Start","0"))], end = visits$End[which(visits$collapse %in% c("End","0"))], tag = visits$tag[which(visits$collapse %in% c("Start","0"))], event = visits$Event[which(visits$collapse %in% c("Start","0"))], feeder =  visits$feeder[which(visits$collapse %in% c("Start","0"))], array = rep(stri_sub(day_arrays[i], 8,11), length(which(visits$collapse %in% c("Start","0")))))  }
}

#Turn the list into a data frame
visit_data <- do.call(rbind,collapsed_list)

#Remove instances where JID = NA and test tags
visit_data$JID <- LH$ID[match(visit_data$tag,LH$RFID)]
visit_data <- subset(visit_data, JID != "NA")


#Calculate distance between nests and experimental setups 
install.packages("rSDI")
library(rSDI)

box_coordinates2 <- box_coordinates %>%
  filter(Box %in% nests_summary$Box)

boxes = unique(box_coordinates2$Box)
wool_loc = unique(diff_exp_loc$location)

dist_boxtoexp = data.frame()

for (i in seq_along(boxes)) {
  FocalBox = box_coordinates2[box_coordinates2$Box == boxes[i], ]
  
  for (j in seq_along(wool_loc)){
    FocalWool = diff_exp_loc[diff_exp_loc$location == wool_loc[j], ]
    Distance = haversine(FocalBox$long, FocalBox$lat, FocalWool$lon, FocalWool$lat, R = 6371)
    
    dfr = data.frame( Box = boxes[i],
                      Wool = wool_loc[j],
                      Distance = Distance )
    
    dist_boxtoexp = rbind(dist_boxtoexp, dfr)
    
  }
}

dist_boxtoexp$site <- paste(substr(dist_boxtoexp$Box, 1,1), substr(dist_boxtoexp$Wool, 1,1), sep = "")
dist_boxtoexp <- subset(dist_boxtoexp, site == "YY" | site == "ZZ")

dist_boxtoexp1 <- dist_boxtoexp
dist_boxtoexp2 <- dist_boxtoexp

dist_boxtoexp1$WoolinBox <- diff_exp1$exp_material[match(dist_boxtoexp1$Box, diff_exp1$id)]
dist_boxtoexp1$WoolinBox[is.na(dist_boxtoexp1$WoolinBox)] <- 0
dist_boxtoexp1$Year <- 2024

dist_boxtoexp2$WoolinBox <- diff_exp2$exp_material[match(dist_boxtoexp2$Box, diff_exp2$id)]
dist_boxtoexp2$WoolinBox[is.na(dist_boxtoexp2$WoolinBox)] <- 0
dist_boxtoexp2$Year <- 2026

dist_boxtoexp2 <- dist_boxtoexp2 %>%
  filter(Box %in% nestboxes2026$Box)

dist_boxtoexp <- rbind(dist_boxtoexp1, dist_boxtoexp2)

dist_boxtoexp$Year <- factor(dist_boxtoexp$Year)

#dist_boxtoexp <- dist_boxtoexp[order(dist_boxtoexp$Distance),]

#dist_boxtoexp <- dist_boxtoexp %>% 
#  group_by(Box) %>%
#  slice(1)

boxplot(dist_boxtoexp$Distance ~ dist_boxtoexp$WoolinBox)
t.test(dist_boxtoexp$Distance ~ dist_boxtoexp$WoolinBox)


exp_brm1_prior <- brm(WoolinBox ~ Distance + (1|Box) + Year,  
                         data = dist_boxtoexp, 
                         family = bernoulli(link = "logit"),
                         prior = c(
                           prior(normal(0, 0.5), class = "b"),        
                           prior(normal(0, 1), class = "Intercept")),
                         sample_prior = "only"
)

#Prior predictive checks 
pp_check(exp_brm1_prior, ndraws = 100)

#Model
exp_brm1 <- brm(WoolinBox ~ Distance + Year + (1|mm(Box,Wool)),  
                   data = dist_boxtoexp, 
                   family = bernoulli(link = "logit"),
                   prior = c(
                     prior(normal(0, 0.5), class = "b"),        
                     prior(normal(0, 1), class = "Intercept")),
                iter = 8000, warmup = 2000
)

#what about pair_age_2_z + I(pair_age_2_z ^2)

summary(exp_brm1)

check_collinearity(exp_brm1)

#Posterior predictive checks
pp_check(exp_brm1, type = "dens_overlay", ndraws = 100)

#Plot model
plot(exp_brm1)
launch_shinystan(exp_brm1)

#Posterior distribution
as_draws_df(exp_brm1)
mcmc_areas(exp_brm1)
mcmc_intervals(exp_brm1)

#Evaluation and interpretation
loo(exp_brm1, moment_match = TRUE)
fitted(exp_brm1, scale = "response")
conditional_effects(exp_brm1)
bayes_R2(exp_brm1)

#Sensitivity analysis and prior checks 
prior_summary(exp_brm1)

#Extract predictions
fitted(exp_brm1)

hypothesis(exp_brm1, "Distance > 0")


#5. Architecture ----

#Presence or absence of a barrier 

nests_sim_edges$nest1_barrier <- nest_obs_direct_barrier$Barrier_binary[match(nests_sim_edges$nest1, nest_obs_direct_barrier$Box)]
nests_sim_edges$nest2_barrier <- nest_obs_direct_barrier$Barrier_binary[match(nests_sim_edges$nest2, nest_obs_direct_barrier$Box)]

nests_sim_edges$barrier_binary <- ifelse(nests_sim_edges$nest1_barrier == nests_sim_edges$nest2_barrier, 1, 0)

#Model just with prior
default_prior()
barrier_brm1_prior <- brm(barrier_binary ~ spatial_weight + prospect_weight + site_comb + 
                            (1|mm(nest1,nest2)), 
                          data = nests_sim_edges, 
                          family = bernoulli(link = "logit"),
                          prior = c(
                            set_prior("normal(0, 1)", class = "Intercept"),        
                            set_prior("normal(0, 0.5)", class = "b")),
                          sample_prior = "only"
)

#Prior predictive checks 
pp_check(barrier_brm1_prior, ndraws = 100)

#Model including
barrier_brm1 <- brm(barrier_binary ~ spatial_weight_z + prospect_weight_z + site_comb + 
                      (1|mm(nest1,nest2)), 
                    data = nests_sim_edges, 
                    family = bernoulli(link = "logit"),
                    prior = c(
                      set_prior("normal(0, 1)", class = "Intercept"),        
                      set_prior("normal(0, 0.5)", class = "b"))
)

#coordinates: gp(nest1_long, nest1_lat) + gp(nest2_long, nest2_lat)

summary(barrier_brm1)
check_collinearity(barrier_brm1)

#Posterior predictive checks
pp_check(barrier_brm1, type = "dens_overlay", ndraws = 1000)

#Plot model
plot(barrier_brm1)

#Posterior distribution
as_draws_df(barrier_brm1)
mcmc_areas(barrier_brm1)
mcmc_intervals(barrier_brm1)

#Evaluation and interpretation
loo(barrier_brm1)
fitted(barrier_brm1, scale = "response")
conditional_effects(barrier_brm1)
bayes_R2(barrier_brm1)
hypothesis(barrier_brm1, "prospect_weight > 0")


#Sensitivity analysis and prior checks 
prior_summary(barrier_brm1)

#Extract predictions
fitted(barrier_brm1)


#Model just with prior
default_prior()
barrier_brm2_prior <- brm(Barrier_binary ~ Site + Density25m, 
                          data = nest_obs_direct_barrier, 
                          family = bernoulli(link = "logit"),
                          prior = c(
                            set_prior("normal(0, 1)", class = "Intercept"),        
                            set_prior("normal(0, 0.5)", class = "b")),
                          sample_prior = "only"
)

#Prior predictive checks 
pp_check(barrier_brm2_prior, ndraws = 100)

#Model including
barrier_brm2 <- brm(Barrier_binary ~ Site + Density25m, 
                    data = nest_obs_direct_barrier, 
                    family = bernoulli(link = "logit"),
                    prior = c(
                      set_prior("normal(0, 1)", class = "Intercept"),        
                      set_prior("normal(0, 0.5)", class = "b"))
)

summary(barrier_brm2)
check_collinearity(barrier_brm2)

#Posterior predictive checks
pp_check(barrier_brm2, type = "dens_overlay", ndraws = 1000)

#Plot model
plot(barrier_brm2)

#Posterior distribution
as_draws_df(barrier_brm2)
mcmc_areas(barrier_brm2)
mcmc_intervals(barrier_brm2)

#Evaluation and interpretation
loo(barrier_brm2)
fitted(barrier_brm2, scale = "response")
conditional_effects(barrier_brm2)
bayes_R2(barrier_brm2)
emmeans(barrier_brm2, ~ Site, type = "response") |> pairs()
hypothesis(barrier_brm2, "Density25m < 0") 

#Sensitivity analysis and prior checks 
prior_summary(barrier_brm2)

#Extract predictions
fitted(barrier_brm2)


#(04) FIGURES ----


#Spatial network ----
library(maps)
library(igraph)
library(sf)
library(ggplot2)

#Spatial distance matrix
edge_list_spatial <- distance[, c("InputID", "TargetID", "Distance", "SiteComb")]

#Only consider distances within study sites
edge_list_spatial <- subset(edge_list_spatial, edge_list_spatial$SiteComb == "XX" | edge_list_spatial$SiteComb == "YY" | edge_list_spatial$SiteComb == "ZZ")

#Remove weaker spatial connections (more conservative)
median(edge_list_spatial$Distance) #100 m
mean(edge_list_spatial$Distance) #118 m
sd(edge_list_spatial$Distance) #79 m

#Boxes within 100, 150, or 200m
edge_list_spatial <- subset(edge_list_spatial, Distance < 100)
edge_list_spatial <- subset(edge_list_spatial, Distance < 150)
edge_list_spatial <- subset(edge_list_spatial, Distance < 200)

#Filter edge list to include boxes in event_data
edge_list_spatial <- edge_list_spatial %>%
  filter(InputID %in% nests_summary$Box)

edge_list_spatial <- edge_list_spatial %>%
  filter(TargetID %in% nests_summary$Box)

#Minimum and maximum distances between boxes
dmin <- min(edge_list_spatial$Distance)
dmax <- max(edge_list_spatial$Distance)

#Add spatial weight, standardised from 0 to 1
edge_list_spatial$spatial_weight <- (dmax - edge_list_spatial$Distance) / (dmax - dmin)

#Rename nodes
edge_list_spatial <- edge_list_spatial %>% rename(from = InputID)
edge_list_spatial <- edge_list_spatial  %>% rename(to = TargetID)

#Rename edge
edge_list_spatial <- edge_list_spatial  %>% rename(box_distance = Distance)

#Remove Distance and SiteComb
#edge_list_spatial <- edge_list_spatial[, c("InputID", "TargetID", "Distance", "spatial_weight")]

edge_list_spatial$dyad_ID <- ifelse(edge_list_spatial$from < edge_list_spatial$to, paste(edge_list_spatial$from, edge_list_spatial$to, sep = "_"), paste(edge_list_spatial$to, edge_list_spatial$from, sep = "_"))

edge_list_spatial <- edge_list_spatial %>% 
  group_by(dyad_ID) %>%
  slice(1)

#Add material similarity 
edge_list_spatial$material_weight <- nests_sim_edges$weight[match(edge_list_spatial$dyad_ID, nests_sim_edges$dyad_ID)]


#Only Campus
edge_list_spatial_X <- subset(edge_list_spatial, substr(from, 1,1) == "X")

median(edge_list_spatial_X$box_distance)

g_spatial_X <- graph_from_data_frame(edge_list_spatial_X, directed = FALSE)

V(g_spatial_X)
E(g_spatial_X)$spatial_weight

V(g_spatial_X)$lon <- box_coordinates$long[match(V(g_spatial_X)$name, box_coordinates$Box)]
V(g_spatial_X)$lat <- box_coordinates$lat[match(V(g_spatial_X)$name, box_coordinates$Box)]

location_X <- cbind(
  lon = as.numeric(V(g_spatial_X)$lon),
  lat = as.numeric(V(g_spatial_X)$lat)
)

colfunc <- colorRampPalette(c("black", "white"))
#colfunc <- colorRampPalette(c("black", "lightgreX"))

edge_colours <- colfunc(21) #for all boxes
edge_colours <- colfunc(12) #for boxes within 150 m
edge_colours <- colfunc(8) #for boxes within 100 m

edge_colours

#for all boxes
edge_list_spatial_X <- edge_list_spatial_X %>% 
  rename(box_distance = spatial_weight)

edge_list_spatial_X <- edge_list_spatial_X %>%
  mutate(colour =case_when(
    box_distance < 25  ~ edge_colours[1],
    box_distance >= 25 & box_distance < 50 ~ edge_colours[3],
    box_distance >= 50 & box_distance < 75 ~ edge_colours[5],
    box_distance >= 75 & box_distance < 100 ~ edge_colours[7],
    box_distance >= 100 & box_distance < 150 ~ edge_colours[9],
    box_distance >= 150 & box_distance < 200 ~ edge_colours[11],
    box_distance >= 200 & box_distance < 250 ~ edge_colours[13],
    box_distance >= 250 & box_distance < 300 ~ edge_colours[15],
    box_distance >= 300 & box_distance < 350 ~ edge_colours[17],
    box_distance >= 350 & box_distance < 400 ~ edge_colours[19],
    box_distance > 400 ~ edge_colours[21]))

#for boxes within 150m
edge_list_spatial_X <- edge_list_spatial_X %>%
  mutate(colour =case_when(
    box_distance < 25  ~ edge_colours[1],
    box_distance >= 25 & box_distance < 50 ~ edge_colours[3],
    box_distance >= 50 & box_distance < 75 ~ edge_colours[5],
    box_distance >= 75 & box_distance < 100 ~ edge_colours[7],
    box_distance >= 100 & box_distance < 125 ~ edge_colours[9],
    box_distance >= 125 & box_distance < 150 ~ edge_colours[11]))

#for boxes within 100 m
edge_list_spatial_X <- edge_list_spatial_X %>%
  mutate(colour =case_when(
    box_distance < 25  ~ edge_colours[1],
    box_distance >= 25 & box_distance < 50 ~ edge_colours[3],
    box_distance >= 50 & box_distance < 75 ~ edge_colours[5],
    box_distance >= 75 & box_distance < 100 ~ edge_colours[7]))

g_spatial_X <- set_edge_attr(g_spatial_X, "edge_colours", value = edge_list_spatial_X$colour)

g_spatial_X <- set_vertex_attr(g_spatial_X, "wool_colour", value = diff_exp$colour[match(V(g_spatial_X)$name, diff_exp$id)])
g_spatial_X <- set_vertex_attr(g_spatial_X, "start", value = nests_summary$Start_z[match(V(g_spatial_X)$name, nests_summary$Box)])

V(g_spatial_X)$colour <- ifelse(is.na(V(g_spatial_X)$wool_colour), "white", "red")

colfunc <- colorRampPalette(c("#D55E00", "#FFFFBF"))
vertex_colours <- colfunc(36)

#Current version in manuscript  
nests_summary <- nests_summary %>%
  mutate(start_colour = case_when(
    Start == 1  ~ vertex_colours[1],
    Start == 2  ~ vertex_colours[2],
    Start == 3  ~ vertex_colours[3],
    Start == 4  ~ vertex_colours[4],
    Start == 5  ~ vertex_colours[5],
    Start == 6  ~ vertex_colours[6],
    Start == 7  ~ vertex_colours[7],
    Start == 8  ~ vertex_colours[8],
    Start == 9  ~ vertex_colours[9],
    Start == 10  ~ vertex_colours[10],
    Start == 11  ~ vertex_colours[11],
    Start == 12  ~ vertex_colours[12],
    Start == 13  ~ vertex_colours[13],
    Start == 14  ~ vertex_colours[14],
    Start == 15  ~ vertex_colours[15],
    Start == 16  ~ vertex_colours[16],
    Start == 17  ~ vertex_colours[17],
    Start == 18  ~ vertex_colours[18],
    Start == 19  ~ vertex_colours[19],
    Start == 20  ~ vertex_colours[20],
    Start == 21  ~ vertex_colours[21],
    Start == 22  ~ vertex_colours[22],
    Start == 23  ~ vertex_colours[23],
    Start == 24  ~ vertex_colours[24],
    Start == 25  ~ vertex_colours[25],
    Start == 26 ~ vertex_colours[26],
    Start == 27  ~ vertex_colours[27],
    Start == 28 ~ vertex_colours[28],
    Start == 29  ~ vertex_colours[29],
    Start == 30 ~ vertex_colours[30],
    Start == 31  ~ vertex_colours[31],
    Start == 32  ~ vertex_colours[32],
    Start == 33  ~ vertex_colours[33],
    Start == 34  ~ vertex_colours[34],
    Start == 35  ~ vertex_colours[35]))

#Add edge colour
g_spatial_X <- set_edge_attr(g_spatial_X, "edge_colours", value = edge_list_spatial_X$colour)

#Add vertex attributes
g_spatial_X <- set_vertex_attr(g_spatial_X, "wool", value = diff_exp$colour[match(V(g_spatial_X)$name, diff_exp$id)])
g_spatial_X <- set_vertex_attr(g_spatial_X, "start", value = nests_summary$Start_z[match(V(g_spatial_X)$name, nests_summary$Box)])
g_spatial_X <- set_vertex_attr(g_spatial_X, "start_colour", value = nests_summary$start_colour[match(V(g_spatial_X)$name, nests_summary$Box)])

V(g_spatial_X)$material_colour <- ifelse(is.na(V(g_spatial_X)$wool), "white", "red")
V(g_spatial_X)$material_colour[V(g_spatial_X)$name == "X02"] <- "blue"
V(g_spatial_X)$material_colour[V(g_spatial_X)$name == "X09"] <- "green"

#Network plot for start
plot(g_spatial_X,
     layout = location_X,
     rescale = TRUE,     
     asp = 0,            
     vertex.label = NA, #or V(g_spatial_Y)$name
     vertex.label.size = 0.7,
     vertex.label.color = "black",
     vertex.size = 5, #V(g_spatial_Y)$start + 5 or 5, or 7 with labels
     vertex.color = V(g_spatial_Y)$start_colour,
     #vertex.frame.color = "white",
     edge.width = E(g_spatial_X)$spatial_weight * 5,
     edge.curved = 0.15,
     edge.color = E(g_spatial_X)$edge_colours)

#Network plot for start

#Network plot for material
plot(g_spatial_X,
     layout = location_X,
     rescale = TRUE,
     asp = 0,
     vertex.label =  V(g_spatial_X)$name, #or V(g_spatial_X)$name
     vertex.label.size = 0.7,
     vertex.label.color = "black",
     vertex.size = 5, #or 5, or 7 with labels
     vertex.color = V(g_spatial_X)$colour,
     edge.width = E(g_spatial_X)$spatial_weight * 5,
     edge.curved = 0.15,
     edge.color = E(g_spatial_X)$edge_colours)

#Only Stithians
edge_list_spatial_Y <- subset(edge_list_spatial, substr(from, 1,1) == "Y")

#Experiment locations?
#edge_list_spatial_Y[529, "from"] <- "Y1Exp"
#edge_list_spatial_Y[529, "to"] <- "Y1Exp"
#edge_list_spatial_Y[530, "from"] <- "Y2Exp"
#edge_list_spatial_Y[530, "to"] <- "Y2Exp"
#edge_list_spatial_Y[531, "from"] <- "Y3Exp"
#edge_list_spatial_Y[531, "to"] <- "Y3Exp"

median(edge_list_spatial_Y$box_distance)

g_spatial_Y <- graph_from_data_frame(edge_list_spatial_Y, directed = FALSE)

V(g_spatial_Y)
E(g_spatial_Y)$spatial_weight
E(g_spatial_Y)$material_weight

V(g_spatial_Y)$lon <- box_coordinates$long[match(V(g_spatial_Y)$name, box_coordinates$Box)]
V(g_spatial_Y)$lat <- box_coordinates$lat[match(V(g_spatial_Y)$name, box_coordinates$Box)]

#Experiment locations?
#V(g_spatial_Y)$lon <- ifelse(!is.na(V(g_spatial_Y)$lon), V(g_spatial_Y)$lon, diff_exp_loc$lon[match(V(g_spatial_Y)$name, diff_exp_loc$location)])
#V(g_spatial_Y)$lat <- ifelse(!is.na(V(g_spatial_Y)$lat), V(g_spatial_Y)$lat, diff_exp_loc$lat[match(V(g_spatial_Y)$name, diff_exp_loc$location)])

location_Y <- cbind(
  lon = as.numeric(V(g_spatial_Y)$lon),
  lat = as.numeric(V(g_spatial_Y)$lat)
)

colfunc <- colorRampPalette(c("black", "white"))
#colfunc <- colorRampPalette(c("black", "lightgrey"))

#for spatial network 
edge_colours <- colfunc(21) #for all boxes
edge_colours <- colfunc(12) #for boxes within 150 m
edge_colours <- colfunc(8) #for boxes within 100 m

#for material similarity network
edge_colours <- colfunc(17) #for nest similarity >= 0.6
edge_colours <- colfunc(13) #for nest similarity >= 0.6

edge_colours

#for all boxes
edge_list_spatial_Y <- edge_list_spatial_Y %>%
  mutate(colour =case_when(
    box_distance < 25  ~ edge_colours[1],
    box_distance >= 25 & box_distance < 50 ~ edge_colours[3],
    box_distance >= 50 & box_distance < 75 ~ edge_colours[5],
    box_distance >= 75 & box_distance < 100 ~ edge_colours[7],
    box_distance >= 100 & box_distance < 150 ~ edge_colours[9],
    box_distance >= 150 & box_distance < 200 ~ edge_colours[11],
    box_distance >= 200 & box_distance < 250 ~ edge_colours[13],
    box_distance >= 250 & box_distance < 300 ~ edge_colours[15],
    box_distance >= 300 & box_distance < 350 ~ edge_colours[17],
    box_distance >= 350 & box_distance < 400 ~ edge_colours[19],
    box_distance > 400 ~ edge_colours[21]))

#for boxes within 150m
edge_list_spatial_Y <- edge_list_spatial_Y %>%
  mutate(colour =case_when(
    box_distance < 25  ~ edge_colours[1],
    box_distance >= 25 & box_distance < 50 ~ edge_colours[3],
    box_distance >= 50 & box_distance < 75 ~ edge_colours[5],
    box_distance >= 75 & box_distance < 100 ~ edge_colours[7],
    box_distance >= 100 & box_distance < 125 ~ edge_colours[9],
    box_distance >= 125 & box_distance < 150 ~ edge_colours[11]))
  
#for boxes within 100 m
edge_list_spatial_Y <- edge_list_spatial_Y %>%
  mutate(colour =case_when(
    box_distance < 25  ~ edge_colours[1],
    box_distance >= 25 & box_distance < 50 ~ edge_colours[3],
    box_distance >= 50 & box_distance < 75 ~ edge_colours[5],
    box_distance >= 75 & box_distance < 100 ~ edge_colours[7]))

#material similarity 
edge_list_spatial_Y <- edge_list_spatial_Y %>%
  mutate(colour =case_when(
    material_weight > 0.95  ~ edge_colours[1],
    material_weight <= 0.95 & material_weight > 0.90 ~ edge_colours[3],
    material_weight <= 0.90 & material_weight > 0.85 ~ edge_colours[5],
    material_weight <= 0.85 & material_weight > 0.80 ~ edge_colours[7],
    material_weight <= 0.80 & material_weight > 0.75 ~ edge_colours[9],
    material_weight <= 0.75 & material_weight > 0.70 ~ edge_colours[11],
    material_weight <= 0.70 & material_weight > 0.65 ~ edge_colours[13],
    material_weight <= 0.65 & material_weight >= 0.60 ~ edge_colours[15],
    material_weight < 0.60 ~ NA,
  ))

edge_list_spatial_Y <- edge_list_spatial_Y %>%
  mutate(colour =case_when(
    material_weight > 0.95  ~ edge_colours[1],
    material_weight <= 0.95 & material_weight > 0.90 ~ edge_colours[3],
    material_weight <= 0.90 & material_weight > 0.85 ~ edge_colours[5],
    material_weight <= 0.85 & material_weight > 0.80 ~ edge_colours[7],
    material_weight <= 0.80 & material_weight > 0.75 ~ edge_colours[9],
    material_weight <= 0.75 & material_weight >= 0.70 ~ edge_colours[11],
    material_weight < 0.70 ~ NA,
  ))

#Vertex colours based on start date
colfunc <- colorRampPalette(c("black", "white"))
colfunc <- colorRampPalette(c("darkgrey", "white"))
colfunc <- colorRampPalette(c("darkolivegreen", "white"))
colfunc <- colorRampPalette(c("#A50021", "#0099CC"))
colfunc <- colorRampPalette(c("#A50021", "#FFFFBF"))
colfunc <- colorRampPalette(c("#D55E00", "#56B4E9"))
colfunc <- colorRampPalette(c("#D55E00", "#FFFFBF"))

#colfunc <- colorRampPalette(c("black", "lightgrey"))

vertex_colours <- colfunc(16)
vertex_colours <- colfunc(36)
vertex_colours <- colfunc(66)

vertex_colours

#For legend of colours for nest initiation
par(mar = c(2, 0, 0, 0))

n <- length(vertex_colours)

plot(
  c(0, n),
  c(0, 0.02),  # thin bar
  type = "n",
  axes = FALSE,
  xlab = "",
  ylab = ""
)

rect(
  xleft   = 0:(n - 1),
  ybottom = 0,
  xright  = 1:n,
  ytop    = 0.02,  # thin bar
  col     = vertex_colours,
  border  = NA     # no stripes
)

axis(
  side = 1,
  at = c(0.5, 6.5, 13.5, 20.5, 27.5, 34.5),
  labels = c("1", "7", "14", "21", "28", "35"),
  family = "Garamond",
  cex.axis = 2
)

#Vertex colours for nest initiation 

#for all boxes
nests_summary <- nests_summary %>%
  mutate(start_colour = case_when(
    Start < 5  ~ vertex_colours[1],
    Start >= 5 & Start <10   ~ vertex_colours[3],
    Start >= 10 & Start <15   ~ vertex_colours[5],
    Start >= 15 & Start <20   ~ vertex_colours[7],
    Start >= 20 & Start <25   ~ vertex_colours[9],
    Start >= 25 & Start <30   ~ vertex_colours[11],
    Start >= 30 & Start <35   ~ vertex_colours[13],
    Start >= 35   ~ vertex_colours[15]))

#Current version in manuscript  
nests_summary <- nests_summary %>%
  mutate(start_colour = case_when(
    Start == 1  ~ vertex_colours[1],
    Start == 2  ~ vertex_colours[2],
    Start == 3  ~ vertex_colours[3],
    Start == 4  ~ vertex_colours[4],
    Start == 5  ~ vertex_colours[5],
    Start == 6  ~ vertex_colours[6],
    Start == 7  ~ vertex_colours[7],
    Start == 8  ~ vertex_colours[8],
    Start == 9  ~ vertex_colours[9],
    Start == 10  ~ vertex_colours[10],
    Start == 11  ~ vertex_colours[11],
    Start == 12  ~ vertex_colours[12],
    Start == 13  ~ vertex_colours[13],
    Start == 14  ~ vertex_colours[14],
    Start == 15  ~ vertex_colours[15],
    Start == 16  ~ vertex_colours[16],
    Start == 17  ~ vertex_colours[17],
    Start == 18  ~ vertex_colours[18],
    Start == 19  ~ vertex_colours[19],
    Start == 20  ~ vertex_colours[20],
    Start == 21  ~ vertex_colours[21],
    Start == 22  ~ vertex_colours[22],
    Start == 23  ~ vertex_colours[23],
    Start == 24  ~ vertex_colours[24],
    Start == 25  ~ vertex_colours[25],
    Start == 26 ~ vertex_colours[26],
    Start == 27  ~ vertex_colours[27],
    Start == 28 ~ vertex_colours[28],
    Start == 29  ~ vertex_colours[29],
    Start == 30 ~ vertex_colours[30],
    Start == 31  ~ vertex_colours[31],
    Start == 32  ~ vertex_colours[32],
    Start == 33  ~ vertex_colours[33],
    Start == 34  ~ vertex_colours[34],
    Start == 35  ~ vertex_colours[35]))

nests_summary <- nests_summary %>%
  mutate(start_colour = case_when(
    Start == 1  ~ vertex_colours[1],
    Start == 2  ~ vertex_colours[3],
    Start == 3  ~ vertex_colours[5],
    Start == 4  ~ vertex_colours[7],
    Start == 5  ~ vertex_colours[9],
    Start == 6  ~ vertex_colours[11],
    Start == 7  ~ vertex_colours[13],
    Start == 8  ~ vertex_colours[15],
    Start == 9  ~ vertex_colours[17],
    Start == 10  ~ vertex_colours[19],
    Start == 11  ~ vertex_colours[21],
    Start == 12  ~ vertex_colours[23],
    Start == 13  ~ vertex_colours[25],
    Start == 14  ~ vertex_colours[27],
    Start == 15  ~ vertex_colours[29],
    Start == 16  ~ vertex_colours[31],
    Start == 17  ~ vertex_colours[33],
    Start == 18  ~ vertex_colours[35],
    Start == 19  ~ vertex_colours[37],
    Start == 20  ~ vertex_colours[39],
    Start == 21  ~ vertex_colours[41],
    Start == 22  ~ vertex_colours[43],
    Start == 23  ~ vertex_colours[45],
    Start == 24  ~ vertex_colours[47],
    Start == 25  ~ vertex_colours[49],
    Start == 26 ~ vertex_colours[51],
    Start == 27  ~ vertex_colours[53],
    Start == 28 ~ vertex_colours[55],
    Start == 29  ~ vertex_colours[53],
    Start == 30 ~ vertex_colours[55],
    Start == 31  ~ vertex_colours[57],
    Start == 32  ~ vertex_colours[59],
    Start == 33  ~ vertex_colours[61],
    Start == 34  ~ vertex_colours[63],
    Start == 35  ~ vertex_colours[65]))

#Add edge colour
g_spatial_Y <- set_edge_attr(g_spatial_Y, "edge_colours", value = edge_list_spatial_Y$colour)

#Add vertex attributes
g_spatial_Y <- set_vertex_attr(g_spatial_Y, "wool", value = diff_exp$colour[match(V(g_spatial_Y)$name, diff_exp$id)])
g_spatial_Y <- set_vertex_attr(g_spatial_Y, "start", value = nests_summary$Start_z[match(V(g_spatial_Y)$name, nests_summary$Box)])
g_spatial_Y <- set_vertex_attr(g_spatial_Y, "start_colour", value = nests_summary$start_colour[match(V(g_spatial_Y)$name, nests_summary$Box)])

V(g_spatial_Y)$material_colour <- ifelse(is.na(V(g_spatial_Y)$wool), "white", "red")
V(g_spatial_Y)$material_colour[V(g_spatial_Y)$name == "Y02"] <- "blue"
V(g_spatial_Y)$material_colour[V(g_spatial_Y)$name == "Y09"] <- "green"

#Experimental locations?
#V(g_spatial_Y)$shape <- ifelse(substr(V(g_spatial_Y)$name, 3,3) == "E", "pie", "circle")
#V(g_spatial_Y)$pie <- list(c(1, 1))
#V(g_spatial_Y)$pie_colour[V(g_spatial_Y)$name == "Y1Exp"] <- list(c("red", "green"))
#V(g_spatial_Y)$pie_colour[V(g_spatial_Y)$name == "Y2Exp"] <- list(c("red", "green"))
#V(g_spatial_Y)$pie_colour[V(g_spatial_Y)$name == "Y3Exp"] <- list(c("red", "green"))

#Neutral network 
plot(g_spatial_Y,
     layout = location_Y,
     rescale = TRUE,     
     asp = 0,            
     vertex.label = NA, #or V(g_spatial_Y)$name
     vertex.label.size = 0.7,
     vertex.label.color = "black",
     vertex.size = 5, #V(g_spatial_Y)$start + 5 or 5, or 7 with labels
     vertex.color = "white",
     #vertex.frame.color = "white",
     edge.width = E(g_spatial_Y)$spatial_weight * 5,
     edge.curved = 0.15,
     edge.color = E(g_spatial_Y)$edge_colours)

#Network plot for start
plot(g_spatial_Y,
     layout = location_Y,
     rescale = TRUE,     
     asp = 0,            
     vertex.label = NA, #or V(g_spatial_Y)$name
     vertex.label.size = 0.7,
     vertex.label.color = "black",
     vertex.size = 5, #V(g_spatial_Y)$start + 5 or 5, or 7 with labels
     vertex.color = V(g_spatial_Y)$start_colour,
     #vertex.frame.color = "white",
     edge.width = E(g_spatial_Y)$spatial_weight * 5,
     edge.curved = 0.15,
     edge.color = E(g_spatial_Y)$edge_colours)

#Network for material similarity
plot(g_spatial_Y,
     layout = location_Y,
     rescale = TRUE,     
     asp = 0,            
     vertex.label = NA, #or V(g_spatial_Y)$name
     vertex.label.size = 0.7,
     vertex.label.color = "black",
     vertex.size = 5, #V(g_spatial_Y)$start + 5 or 5, or 7 with labels
     vertex.color = "white",
     #vertex.frame.color = "white",
     edge.width = E(g_spatial_Y)$material_weight * 2.5,
     edge.curved = 0.15,
     edge.color = E(g_spatial_Y)$edge_colours)

#Network plot for experimental material (wool)
plot(g_spatial_Y,
     layout = location_Y,
     rescale = TRUE,     
     asp = 0,            
     vertex.label = NA, #or V(g_spatial_Y)$name
     vertex.label.size = 0.7,
     vertex.label.color = "black",
     vertex.size = 5, #V(g_spatial_Y)$start + 5 or 5, or 7 with labels
     vertex.color = ifelse(V(g_spatial_Y)$shape == "circle", V(g_spatial_Y)$material_colour, V(g_spatial_Y)$pie_colour),
     edge.width = E(g_spatial_Y)$spatial_weight * 5,
     edge.curved = 0.15,
     edge.color = E(g_spatial_Y)$edge_colours)

#Only Pencoose
edge_list_spatial_Z <- subset(edge_list_spatial, substr(from, 1,1) == "Z")

median(edge_list_spatial_Z$box_distance)

g_spatial_Z <- graph_from_data_frame(edge_list_spatial_Z, directed = FALSE)

V(g_spatial_Z)
E(g_spatial_Z)$spatial_weight

V(g_spatial_Z)$lon <- box_coordinates$long[match(V(g_spatial_Z)$name, box_coordinates$Box)]
V(g_spatial_Z)$lat <- box_coordinates$lat[match(V(g_spatial_Z)$name, box_coordinates$Box)]

location_Z <- cbind(
  lon = as.numeric(V(g_spatial_Z)$lon),
  lat = as.numeric(V(g_spatial_Z)$lat)
)

colfunc <- colorRampPalette(c("black", "white"))
#colfunc <- colorRampPalette(c("black", "lightgreZ"))

edge_colours <- colfunc(21) #for all boxes
edge_colours <- colfunc(12) #for boxes within 150 m
edge_colours <- colfunc(8) #for boxes within 100 m

#for material similarity network
edge_colours <- colfunc(17) #for nest similarity >= 0.6
edge_colours <- colfunc(13) #for nest similarity >= 0.75

edge_colours

#for all boxes
edge_list_spatial_Z <- edge_list_spatial_Z %>%
  mutate(colour =case_when(
    box_distance < 25  ~ edge_colours[1],
    box_distance >= 25 & box_distance < 50 ~ edge_colours[3],
    box_distance >= 50 & box_distance < 75 ~ edge_colours[5],
    box_distance >= 75 & box_distance < 100 ~ edge_colours[7],
    box_distance >= 100 & box_distance < 150 ~ edge_colours[9],
    box_distance >= 150 & box_distance < 200 ~ edge_colours[11],
    box_distance >= 200 & box_distance < 250 ~ edge_colours[13],
    box_distance >= 250 & box_distance < 300 ~ edge_colours[15],
    box_distance >= 300 & box_distance < 350 ~ edge_colours[17],
    box_distance >= 350 & box_distance < 400 ~ edge_colours[19],
    box_distance > 400 ~ edge_colours[21]))

#for boxes within 150m
edge_list_spatial_Z <- edge_list_spatial_Z %>%
  mutate(colour =case_when(
    box_distance < 25  ~ edge_colours[1],
    box_distance >= 25 & box_distance < 50 ~ edge_colours[3],
    box_distance >= 50 & box_distance < 75 ~ edge_colours[5],
    box_distance >= 75 & box_distance < 100 ~ edge_colours[7],
    box_distance >= 100 & box_distance < 125 ~ edge_colours[9],
    box_distance >= 125 & box_distance < 150 ~ edge_colours[11]))

#for boxes within 100 m
edge_list_spatial_Z <- edge_list_spatial_Z %>%
  mutate(colour =case_when(
    box_distance < 25  ~ edge_colours[1],
    box_distance >= 25 & box_distance < 50 ~ edge_colours[3],
    box_distance >= 50 & box_distance < 75 ~ edge_colours[5],
    box_distance >= 75 & box_distance < 100 ~ edge_colours[7]))

#material similarity 
edge_list_spatial_Z <- edge_list_spatial_Z %>%
  mutate(colour =case_when(
    material_weight > 0.95  ~ edge_colours[1],
    material_weight <= 0.95 & material_weight > 0.90 ~ edge_colours[3],
    material_weight <= 0.90 & material_weight > 0.85 ~ edge_colours[5],
    material_weight <= 0.85 & material_weight > 0.80 ~ edge_colours[7],
    material_weight <= 0.80 & material_weight > 0.75 ~ edge_colours[9],
    material_weight <= 0.75 & material_weight > 0.70 ~ edge_colours[11],
    material_weight <= 0.70 & material_weight > 0.65 ~ edge_colours[13],
    material_weight <= 0.65 & material_weight >= 0.60 ~ edge_colours[15],
    material_weight < 0.60 ~ NA,
  ))

edge_list_spatial_Z <- edge_list_spatial_Z %>%
  mutate(colour =case_when(
    material_weight > 0.95  ~ edge_colours[1],
    material_weight <= 0.95 & material_weight > 0.90 ~ edge_colours[3],
    material_weight <= 0.90 & material_weight > 0.85 ~ edge_colours[5],
    material_weight <= 0.85 & material_weight > 0.80 ~ edge_colours[7],
    material_weight <= 0.80 & material_weight > 0.75 ~ edge_colours[9],
    material_weight <= 0.75 & material_weight >= 0.70 ~ edge_colours[11],
    material_weight < 0.70 ~ NA,
  ))

#Vertex colours based on start date
colfunc <- colorRampPalette(c("black", "white"))
colfunc <- colorRampPalette(c("darkgrey", "white"))
colfunc <- colorRampPalette(c("darkolivegreen", "white"))
colfunc <- colorRampPalette(c("#D55E00", "#56B4E9"))

#colfunc <- colorRampPalette(c("black", "lightgrey"))

vertex_colours <- colfunc(16)
vertex_colours <- colfunc(36)
vertex_colours <- colfunc(66)

vertex_colours

#for all boxes
nests_summary <- nests_summary %>%
  mutate(start_colour = case_when(
    Start < 5  ~ vertex_colours[1],
    Start >= 5 & Start <10   ~ vertex_colours[3],
    Start >= 10 & Start <15   ~ vertex_colours[5],
    Start >= 15 & Start <20   ~ vertex_colours[7],
    Start >= 20 & Start <25   ~ vertex_colours[9],
    Start >= 25 & Start <30   ~ vertex_colours[11],
    Start >= 30 & Start <35   ~ vertex_colours[13],
    Start >= 35   ~ vertex_colours[15]))

nests_summary <- nests_summary %>%
  mutate(start_colour = case_when(
    Start == 1  ~ vertex_colours[1],
    Start == 2  ~ vertex_colours[2],
    Start == 3  ~ vertex_colours[3],
    Start == 4  ~ vertex_colours[4],
    Start == 5  ~ vertex_colours[5],
    Start == 6  ~ vertex_colours[6],
    Start == 7  ~ vertex_colours[7],
    Start == 8  ~ vertex_colours[8],
    Start == 9  ~ vertex_colours[9],
    Start == 10  ~ vertex_colours[10],
    Start == 11  ~ vertex_colours[11],
    Start == 12  ~ vertex_colours[12],
    Start == 13  ~ vertex_colours[13],
    Start == 14  ~ vertex_colours[14],
    Start == 15  ~ vertex_colours[15],
    Start == 16  ~ vertex_colours[16],
    Start == 17  ~ vertex_colours[17],
    Start == 18  ~ vertex_colours[18],
    Start == 19  ~ vertex_colours[19],
    Start == 20  ~ vertex_colours[20],
    Start == 21  ~ vertex_colours[21],
    Start == 22  ~ vertex_colours[22],
    Start == 23  ~ vertex_colours[23],
    Start == 24  ~ vertex_colours[24],
    Start == 25  ~ vertex_colours[25],
    Start == 26 ~ vertex_colours[26],
    Start == 27  ~ vertex_colours[27],
    Start == 28 ~ vertex_colours[28],
    Start == 29  ~ vertex_colours[29],
    Start == 30 ~ vertex_colours[30],
    Start == 31  ~ vertex_colours[31],
    Start == 32  ~ vertex_colours[32],
    Start == 33  ~ vertex_colours[33],
    Start == 34  ~ vertex_colours[34],
    Start == 35  ~ vertex_colours[35]))

#Edge colours    
g_spatial_Z <- set_edge_attr(g_spatial_Z, "edge_colours", value = edge_list_spatial_Z$colour)

#Add vertex attributes
g_spatial_Z <- set_vertex_attr(g_spatial_Z, "wool", value = diff_exp$colour[match(V(g_spatial_Z)$name, diff_exp$id)])
g_spatial_Z <- set_vertex_attr(g_spatial_Z, "start", value = nests_summary$Start_z[match(V(g_spatial_Z)$name, nests_summary$Box)])
g_spatial_Z <- set_vertex_attr(g_spatial_Z, "start_colour", value = nests_summary$start_colour[match(V(g_spatial_Z)$name, nests_summary$Box)])

V(g_spatial_Z)$start[V(g_spatial_Z)$name == "Z09"] <- 0
V(g_spatial_Z)$start[V(g_spatial_Z)$name == "Z25"] <- 0

V(g_spatial_Z)$material_colour <- ifelse(is.na(V(g_spatial_Z)$wool), "white", "yellow")
V(g_spatial_Z)$material_colour[V(g_spatial_Z)$name == "Z41"] <- "red"

#Network for start
plot(g_spatial_Z,
     layout = location_Z,
     rescale = TRUE,     
     asp = 0,            
     vertex.label = NA, #or V(g_spatial_Z)$name
     vertex.label.size = 0.7,
     vertex.label.color = "black",
     vertex.size = 5, #5 or 7 with labels
     vertex.color = V(g_spatial_Z)$start_colour,
     edge.width = E(g_spatial_Z)$spatial_weight * 5,
     edge.curved = 0.15,
     edge.color = E(g_spatial_Z)$edge_colours)

#Network for material similarity
plot(g_spatial_Z,
     layout = location_Z,
     rescale = TRUE,     
     asp = 0,            
     vertex.label = NA, #or V(g_spatial_Z)$name
     vertex.label.size = 0.7,
     vertex.label.color = "black",
     vertex.size = 5, #5 or 7 with labels
     vertex.color = "white",
     edge.width = E(g_spatial_Z)$material_weight * 2.5,
     edge.curved = 0.15,
     edge.color = E(g_spatial_Z)$edge_colours)

#Network for experimental material (wool)
plot(g_spatial_Z,
     layout = location_Z,
     rescale = TRUE,     
     asp = 0,            
     vertex.label = NA, #or V(g_spatial_Z)$name
     vertex.label.size = 0.7,
     vertex.label.color = "black",
     vertex.size = 5, #5 or 7 with labels
     vertex.color = V(g_spatial_Z)$material_colour,
     edge.width = E(g_spatial_Z)$spatial_weight * 5,
     edge.curved = 0.15,
     edge.color = E(g_spatial_Z)$edge_colours)

dev.off()
par(mfrow = c(1, 3), mar = c(1, 1, 1, 1))
par(mfrow = c(1, 2), mar = c(1, 1, 1, 1))
par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))


#Prospecting network ----

library(maps)
library(igraph)
library(sf)
library(ggplot2)

#Edge list for prospecting
edge_list_prospect <- visits_prospect_owners_box_dyads

#Binary edge list for prospecting
edge_list_prospect$prospect_weight_binary <- 1

#Full edge list 
edge_list <- edge_list_spatial

#Binary prospecting weight
edge_list$prospect_weight <- edge_list_prospect$prospect_weight_binary[match(edge_list$dyad_ID, edge_list_prospect$box_dyad_ID)]

#Remove box dyad ID
edge_list$box_dyad_ID <- NULL

#Fill missing prospect edges as 0 (i.e., no prospecting)
edge_list$prospect_weight <- ifelse(is.na(edge_list$prospect_weight), 0, edge_list$prospect_weight)

edge_list$spatial_weight <- NULL

edge_list_prospect <- edge_list[, c("from", "to", "prospect_weight")]

#Only Campus (X)
edge_list_prospect_X <- subset(edge_list_prospect, substr(from, 1,1) == "X")

g_prospect_X <- graph_from_data_frame(edge_list_prospect_X, directed = TRUE)

V(g_prospect_X)

V(g_prospect_X)$lon <- box_coordinates$long[match(V(g_prospect_X)$name, box_coordinates$Box)]
V(g_prospect_X)$lat <- box_coordinates$lat[match(V(g_prospect_X)$name, box_coordinates$Box)]

location <- cbind(
  lon = as.numeric(V(g_prospect_X)$lon),
  lat = as.numeric(V(g_prospect_X)$lat)
)

plot(g_prospect_X,
     layout = location,
     rescale = TRUE,     # IMPORTANT
     asp = 0,            # IMPORTANT
     vertex.label = V(g_prospect_X)$name,
     vertex.label.size = 0.7,
     vertex.label.color = "black",
     vertex.size = 7,
     vertex.color = "white",
     edge.width = 2,
     edge.curved = 0.35,
     edge.arrow.size = 0.5,
     arrow.width = 0.5)

#Only Stithians (Y)
edge_list_prospect_Y <- subset(edge_list_prospect, substr(from, 1,1) == "Y")

g_prospect_Y <- graph_from_data_frame(edge_list_prospect_Y, directed = TRUE)

g_prospect_Y <- subgraph.edges(g_prospect_Y, E(g_prospect_Y)[E(g_prospect_Y)$prospect_weight == 1], del = FALSE)

V(g_prospect_Y)
E(g_prospect_Y)$prospect_weight

V(g_prospect_Y)$lon <- box_coordinates$long[match(V(g_prospect_Y)$name, box_coordinates$Box)]
V(g_prospect_Y)$lat <- box_coordinates$lat[match(V(g_prospect_Y)$name, box_coordinates$Box)]

location_Y <- cbind(
  lon = as.numeric(V(g_prospect_Y)$lon),
  lat = as.numeric(V(g_prospect_Y)$lat)
  )

plot(g_prospect_Y,
     layout = location_Y,
     rescale = TRUE,     
     asp = 0,            
     vertex.label = NA,
     vertex.label.size = NA,
     vertex.label.color = NA,
     vertex.size = 5,
     vertex.color = "white",
     edge.width = E(g_prospect_Y)$prospect_weight * 2,
     edge.curved = 0.35,
     edge.arrow.size = 0.5,
     arrow.width = 0.5)

edge_list_prospect_Z <- subset(edge_list_prospect, substr(from, 1,1) == "Z")

g_prospect_Z <- graph_from_data_frame(edge_list_prospect_Z, directed = TRUE)

V(g_prospect_Z)

V(g_prospect_Z)$lon <- box_coordinates$long[match(V(g_prospect_Z)$name, box_coordinates$Box)]
V(g_prospect_Z)$lat <- box_coordinates$lat[match(V(g_prospect_Z)$name, box_coordinates$Box)]

location <- cbind(
  lon = as.numeric(V(g_prospect_Z)$lon),
  lat = as.numeric(V(g_prospect_Z)$lat)
)

plot(g_prospect_Z,
     layout = location,
     rescale = TRUE,     # IMPORTANT
     asp = 0,            # IMPORTANT
     vertex.label = V(g_prospect_Z)$name,
     vertex.label.size = 0.7,
     vertex.label.color = "black",
     vertex.size = 7,
     vertex.color = "white",
     edge.width = 2,
     edge.curved = 0.35,
     edge.arrow.size = 0.5,
     arrow.width = 0.5)



g_prospect_Y_edges <- igraph::as_data_frame(g_prospect_Y, what = "edges")
g_prospect_Y_vertices <- igraph::as_data_frame(g_prospect_Y, what = "vertices")

g_prospect_Y_edges$lon_from <- g_prospect_Y_vertices$lon[match(g_prospect_Y_edges$from, g_prospect_Y_vertices$name)]
g_prospect_Y_edges$lat_from <- g_prospect_Y_vertices$lat[match(g_prospect_Y_edges$from, g_prospect_Y_vertices$name)]
g_prospect_Y_edges$lon_to   <- g_prospect_Y_vertices$lon[match(g_prospect_Y_edges$to, g_prospect_Y_vertices$name)]
g_prospect_Y_edges$lat_to   <- g_prospect_Y_vertices$lat[match(g_prospect_Y_edges$to, g_prospect_Y_vertices$name)]

library(ggmap)

register_google(key = "AIzaSyDIvL-XLO1etdpwxUOfviz0wJ84n_et3rA")

mean(g_prospect_Y_vertices$lat)
mean(g_prospect_Y_vertices$lon)

mean(feedercoord$longitude)

location <- c(lon = -5.181846, lat = 50.18967)

# Create bounding box around your nodes
boxesmap <- get_map(location=location,
                     source="google", maptype="satellite", crop=FALSE, zoom = 17, color = "bw")

ggmap(boxesmap)


ggmap(boxesmap) +
  geom_segment(
    data = g_prospect_Y_edges,
    aes(x = lon_from, y = lat_from,
        xend = lon_to, yend = lat_to),
    color = "lightgrey",
    alpha = 1,
    linewidth = 1
  ) +
  geom_point(
    data = g_prospect_Y_vertices,
    aes(x = lon, y = lat),
    color = "white",
    size = 3
  )


#General archive ----
#start_m <- glmmTMB(Start day ~ Site + Cluster + Age + Years together + Ownership this and last year)
#Finish_m <- glmmTMB(Finish day ~ Site + Cluster + Age + Years together)
#Duration_m <- glmmTMB(Start day ~ Site + Cluster + Age + Years together)table(nests$Site, nests$Moss)

#I NEST BUILDING

#(1) Start, > 15, cup, finish, duration

#Start of building
nests_summary$Site <- as.factor(nests_summary$Site)
nests_summary$Cluster <- as.factor(nests_summary$Cluster)

start_m <- glmmTMB(Start ~ pair_age + (1|Cluster/Site), data = nests_summary, family = poisson())

start_m <- glm(Start ~ Site + Ownership2324 + pair_age + pair_age_diff, data = nests_summary)

start_m <- glm(Start ~ Site + male_age, data = nests_summary)

start_m <- glm(Start ~ Cluster, data = nests_summary)

distanceX$StartDiffAbs <- as.numeric(distanceX$StartDiffAbs)
distanceY$StartDiffAbs <- as.numeric(distanceY$StartDiffAbs)
distanceZ$StartDiffAbs <- as.numeric(distanceZ$StartDiffAbs)

start_m <- glmmTMB(StartDiffAbs ~ Distance + (1|InputID) + (1|TargetID), data = distanceX, family = poisson())
start_m <- glmmTMB(StartDiffAbs ~ Distance + (1|InputID) + (1|TargetID), data = distanceY, family = poisson())
start_m <- glmmTMB(StartDiffAbs ~ Distance + (1|InputID) + (1|TargetID), data = distanceZ, family = poisson())

start_m <- brm(StartDiffAbs ~ Distance + (1 | mm(InputID, TargetID)), data = distanceY, family = gaussian())

start_x_m <- glm(Start ~ Cluster, data = nests_summary_X)
start_y_m <- glm(Start ~ Cluster, data = nests_summary_Y)
start_z_m <- glm(Start ~ Cluster, data = nests_summary_Z)

summary(start_m)
Anova(start_m)
emmeans(start_m, list(pairwise ~ Cluster), adjust = "bonferroni")

aggregate(nests_summary$Start, by = list(nests_summary$Cluster), FUN = "mean", na.rm = TRUE)
aggregate(nests_summary$Start, by = list(nests_summary$Cluster), FUN = "sd", na.rm = TRUE)

plot(nests_summary$pair_age_diff, nests_summary$Start)
plot(nests_summary$pair_age, nests_summary$Start)

start_brm <- brm(Start ~ Site, data = nests_summary, family = poisson())
summary(start_brm)
plot(start_brm)

nest_checks_start <- subset(nest_checks_start, nest_checks_start$year < 2019)

lm <- lmer(start ~ site * year_z + start_first_check_z + (1|BOX), data = nest_checks_start)
lm <- lmer(start ~ site + year_z + start_first_check_z + (1|BOX), data = nest_checks_start)
lm <- lmer(start_first_check ~ year + (1|BOX), data = nest_checks_start)

lm <- lmer(start ~ site * year_z + (1|BOX), data = nest_checks_lined)

summary(lm)
Anova(lm)
emmeans(lm, list(pairwise ~ site))
emmeans(lm, list(pairwise ~ year))

plot(nest_checks$year, nest_checks$start)
abline(lm)

interact_plot(model = lm, pred = year_z, modx = site)

#Finishing building (i.e. egg)
egg_m <- glm(Egg ~ Site + Cluster + pair_age + pair_age_diff, data = nests_summary)
summary(egg_m)
Anova(egg_m)
emmeans(egg_m, list(pairwise ~ Cluster), adjust = "bonferroni")

#Duration of building (start to egg)
duration_site_m <- glm(Duration ~ Site + pair_age, data = nests_summary)
summary(duration_m)
Anova(duration_m)
emmeans(duration_m, list(pairwise ~ Cluster), adjust = "bonferroni")

#(2) Materials 

#Sites
table(nest_obs_direct_test$Site, nest_obs_direct_test$Diversity)
diversity_m <- glmmTMB(Diversity ~ Site + Cluster + Study.Day + pair_age + pair_feeder + (1|Nest.box), data = nest_obs_direct_test, family = poisson())
diversity_m  <- glm(Anthropogenic ~ Site, data = nests, family = binomial)
summary(diversity_m)
Anova(diversity_m)
emmeans(diversity_m, list(pairwise ~ Site), adjust = "bonferroni")
emmeans(diversity_m, list(pairwise ~ Cluster), adjust = "bonferroni")

table(nest_obs_direct$Site, nest_obs_direct$Anthropogenic)
anthro_m <- glmmTMB(Anthropogenic ~ Cluster + pair_age + pair_feeder + (1|Nest.box), data = nest_obs_direct, family = binomial)
anthro_m <- glm(Anthropogenic ~ Site, data = nests, family = binomial)
summary(anthro_m)
Anova(anthro_m)
emmeans(anthro_m, list(pairwise ~ Site), adjust = "bonferroni")
emmeans(anthro_m, list(pairwise ~ Cluster), adjust = "bonferroni")

table(nests_direct$Site, nests_direct$Paper)
paper_m <- glmmTMB(Paper ~ Site + (1|Nest.box), data = nests_direct, family = binomial)
paper_m <- glm(Paper ~ Site, data = nests, family = binomial)
summary(paper_m)
Anova(paper_m)
emmeans(paper_m, list(pairwise ~ Site), adjust = "bonferroni")

table(nests_direct$Site, nests_direct$Plastic)
plastic_m <- glmmTMB(Plastic ~ Site + (1|Nest.box), data = nests_direct, family = binomial)
plastic_m <- glm(Plastic ~ Site, data = nests, family = binomial)
summary(plastic_m)
Anova(plastic_m)
emmeans(plastic_m, list(pairwise ~ Site), adjust = "bonferroni")

table(nests_direct$Site, nests_direct$Moss)
moss_m <- glmmTMB(Moss ~ Site + (1|Nest.box), data = nest_obs_direct, family = binomial)
moss_m <- glm(Moss ~ Site, data = nests)
summary(moss_m)
Anova(moss_m)
emmeans(moss_m, list(pairwise ~ Site), adjust = "bonferroni")

table(nests_direct$Site, nests_direct$Leaves)
leaves_m <- glmmTMB(Leaves ~ Site + (1|Nest.box), data = nest_obs_direct, family = binomial)
leaves_m  <- glm(Leaves ~ Site, data = nests)
summary(leaves_m )
Anova(leaves_m )
emmeans(leaves_m, list(pairwise ~ Site), adjust = "bonferroni")

table(nests_direct$Site, nests_direct$Fur)
fur_m <- glmmTMB(Fur ~ Site + (1|Nest.box), data = nests_direct, family = binomial)
fur_m <- glm(Fur ~ Site, data = nests)
summary(fur_m)
Anova(fur_m)
emmeans(fur_m, list(pairwise ~ Site), adjust = "bonferroni")

table(nests_direct$Site, nests_direct$Barrier)
nests_direct_barrier <- subset(nests_direct, !nests_direct$Barrier == "")
table(nests_direct_barrier$Site, nests_direct_barrier$Barrier)
nests_direct_barrier$Barrier <- as.numeric(nests_direct_barrier$Barrier)

barrier_m <- glmmTMB(Barrier ~ Site + (1|Nest.box), data = nests_direct_barrier, family = binomial)
barrier_m <- glm(Barrier ~ Site, data = nests_direct_barrier, family = binomial)
summary(barrier_m)
Anova(barrier_m)
emmeans(barrier_m, list(pairwise ~ Site), adjust = "bonferroni")

#Clusters
table(nests_direct$Cluster, nests_direct$Anthropogenic)
anthro_m <- glmmTMB(Anthropogenic ~ Cluster + (1|Nest.box), data = nests_direct, family = binomial)
anthro_m <- glm(Anthropogenic ~ Site, data = nests, family = binomial)
summary(anthro_m)
Anova(anthro_m)
emmeans(anthro_m, list(pairwise ~ Cluster), adjust = "bonferroni")

table(nests_direct$Cluster, nests_direct$Moss)
moss_m <- glmmTMB(Moss ~ Cluster + (1|Nest.box), data = nests_direct, family = binomial)
moss_m <- glm(Moss ~ Site, data = nests, family = binomial)
summary(moss_m)
Anova(moss_m)
emmeans(moss_m, list(pairwise ~ Cluster), adjust = "bonferroni")

table(nests_direct$Cluster, nests_direct$Fur)
fur_m <- glmmTMB(Fur ~ Cluster + (1|Nest.box), data = nest_obs_direct, family = binomial)
fur_m <- glm(Anthropogenic ~ Site, data = nests, family = binomial)
summary(fur_m)
Anova(fur_m)
emmeans(fur_m, list(pairwise ~ Cluster), adjust = "bonferroni")

#3 Nest visit rates
nests_summary$logger_duration <- logger_duration$logger_duration_days[match(nests_summary$Box, logger_duration$nest_id)]

nests_summary$female_visits <- per_indiv_per_box_owner_f$n[match(nests_summary$Box, per_indiv_per_box_owner_f$box)]
nests_summary$male_visits <- per_indiv_per_box_owner_m$n[match(nests_summary$Box, per_indiv_per_box_owner_m$box)]

nests_summary$pair_visits <- nests_summary$female_visits + nests_summary$male_visits 

nests_summary$pair_visits_mean <- (nests_summary$female_visits + nests_summary$male_visits) / 2 

nests_summary <- nests_summary %>% 
  mutate(pair_visits_mean = case_when(
    is.na(female_visits) & is.na(male_visits) ~ NA,
    is.na(female_visits) & !is.na(male_visits) ~ male_visits,
    !is.na(female_visits) & is.na(male_visits) ~ female_visits,
    !is.na(female_visits) & !is.na(male_visits) ~ (female_visits + male_visits) / 2))

nests_summary$pair_visits_stand <- nests_summary$pair_visits_mean / nests_summary$logger_duration

nests_summary$Female_Tag <- as.Date(nests_summary$Female_Tag, format = "%Y/%m/%d") # convert to date
nests_summary$Female_Tag <- yday(nests_summary$Female_Tag) #day of year

nests_summary$Male_Tag <- as.Date(nests_summary$Male_Tag, format = "%Y/%m/%d") # convert to date
nests_summary$Male_Tag <- yday(nests_summary$Male_Tag) #day of year

#Subtract by study duration
nests_summary$Female_Tag <- nests_summary$Female_Tag - 41
nests_summary$Male_Tag <- nests_summary$Male_Tag - 41

mean(nests_summary$female_visits, na.rm = TRUE)
mean(nests_summary$male_visits, na.rm = TRUE)
mean(nests_summary$pair_visits_stand, na.rm = TRUE)

plot(nests_summary$pair_visits_stand, nests_summary$Duration)
plot(nests_summary$pair_visits_stand, nests_summary$Start)

nests_summary_2 <- nests_summary

nests_summary_2 <- nests_summary  %>% pivot_longer(
  cols = c("female_visits", "male_visits"))

nests_summary_2$visits <- nests_summary_2$value
nests_summary_2$visits_stand <- nests_summary_2$visits/nests_summary_2$logger_duration
nests_summary_2$sex <- nests_summary_2$name
nests_summary_2 <- subset(nests_summary_2, nests_summary_2$logger_duration > 5)
nests_summary_2 <- subset(nests_summary_2, nests_summary_2$Female_Tag == 65)
nests_summary_2 <- subset(nests_summary_2, nests_summary_2$Male_Tag == 65)

nests_summary_3 <- subset(nests_summary_2, !is.na(nests_summary_2$visits_stand))
table(nests_summary_3$sex)
owners_ID = unique(nests_summary_2$JID)

boxes = unique(nests_summary_3$Box)

write.csv(boxes,"C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/nest_logger_boxes.csv", row.names = FALSE)

mean(nests_summary_2$logger_duration)
sd(nests_summary_2$logger_duration)
mean(nests_summary_2$visits_stand, na.rm = TRUE)
sd(nests_summary_2$visits_stand, na.rm = TRUE)

visit_nest_duration_m <- glmmTMB(Duration ~ visits_stand * sex + (1|Box), data = nests_summary_2, family = poisson)
summary(visit_nest_duration_m)
Anova(visit_nest_duration_m)

visit_nest_duration_plot <- ggplot(nests_summary_2, aes(x = visits_stand, y = Duration)) + 
  labs(x = "Visits per day", y = "Duration of building (days of study period)") +
  geom_point() +
  theme_bw(base_size = 18)
visit_nest_duration_plot 

#II PROSPECTING
perindivpersite2
perindivpersite3 <- perindivpersite2[,c(1,2)]
perindivpersite4 <- perindivpersite2[,c(3:69)]
perindivpersite4 <- ifelse(is.na(perindivpersite4), 0, 1)

perindivpersite5 <- cbind(perindivpersite3, perindivpersite4)

perindivpersite5$n.boxes <- rowSums(perindivpersite5[,c(3:69)])

visits_prospect$day <- yday(visits_prospect$Datetime) #day of year
visits_prospect$study_day <- visits_prospect$day - 65 #day of study period 

visits_prospect$first_egg <- nests_summary$Egg[match(visits_prospect$box, nests_summary$Box)] - 6
visits_prospect$days_to_egg <- visits_prospect$first_egg - visits_prospect$study_day

visits_prospect$site <- substr(visits_prospect$box, 1,1)
visits_prospect$visitor_site <- substr(visits_prospect$visitor_owned_box, 1,1)

ggplot(visits_prospect, aes(x = study_day)) + geom_point(stat = "count") 

visits_prospect2 <- visits_prospect %>% count(box, study_day)
visits_prospect2$days_to_egg <- visits_prospect$days_to_egg[match(visits_prospect2$box, visits_prospect$box)]
visits_prospect2$visits <- visits_prospect2$n

visit_prospect_m <- glmmTMB(visits ~ days_to_egg + (1|box), data = visits_prospect2, family = poisson)
summary(visit_prospect_m)
Anova(visit_prospect_m)

unique_ID = unique(visits_prospect$JID)
unique_box = unique(nests_summary_2$Box)

individuals = data.frame(JID = unique_ID)
individuals2 <- visits_prospect %>% count(JID)

individuals$known_age = Ringed$known_age[match(individuals$JID,Ringed$ID)]
individuals$min_age = Ringed$min_age[match(individuals$JID,Ringed$ID)]
individuals$sex = Ringed$SEX[match(individuals$JID,Ringed$ID)]
individuals$box = visits$visitor_owned_box[match(individuals$JID,visits$JID)]

individuals$visits <- individuals2$n[match(individuals$JID, individuals2$JID)]

individuals$box_ownership <- ifelse(is.na(individuals$box), "no", "yes")

individuals$owner_visits <- per_indiv_per_box_owner$n[match(individuals$JID, per_indiv_per_box_owner$JID)]

individuals3 <- visits_prospect %>% count(JID, box)

individuals4 <- as.data.frame(table(individuals3$JID))

individuals$boxes_visited <- individuals4$Freq[match(individuals$JID, individuals4$Var1)]

individuals_subset <- subset(individuals, individuals$visits < 500)

individual_prospect_m1 <- glm(visits ~ min_age + box_ownership + sex, data =  individuals_subset, family = poisson)
summary(individual_prospect_m1)
Anova(individual_prospect_m1)
vif(individual_prospect_m1)
emmeans(individual_prospect_m1, list(pairwise ~ box_ownership), adjust = "bonferroni")

individual_prospect_m2 <- glm(boxes_visited ~ min_age + box_ownership + sex, data =  individuals, family = poisson)
summary(individual_prospect_m2)
Anova(individual_prospect_m2)
vif(individual_prospect_m)
emmeans(individual_prospect_m2, list(pairwise ~ sex), adjust = "bonferroni")

plot(individuals$min_age, individuals$visits)
boxplot(individuals$visits ~ individuals$sex)
boxplot(individuals$visits ~ individuals$box_ownership)

boxplot(individuals$boxes_visited ~ individuals$sex)
boxplot(individuals$visits ~ individuals$box_ownership)

plot(individuals$boxes_visited, individuals$visits)
plot(individuals$owner_visits, individuals$visits)

prospect_owner_visit_plot <- ggplot(individuals, aes(x = owner_visits, y = visits)) + 
  geom_point() +
  labs(x="Number of owner visits", y = "Number of prospecting visits") +
  theme_bw(base_size = 12) +
  ylim(0, 50)
prospect_owner_visit_plot

individuals$min_age <- as.numeric(individuals$min_age)

prospect_indiv_age_plot <- ggplot(individuals, aes(x = min_age, y = visits)) + 
  geom_point() +
  geom_jitter(shape = 16, width = 0.2, height = 0, size = 1.5) +
  labs(x="Age", y = "No of prospecting visits") +
  theme_classic(base_size = 25)
prospect_indiv_age_plot

prospect_indiv_ownership_plot <- ggplot(individuals, aes(x = box_ownership, y = visits)) + 
  geom_boxplot(width = 0.75) +
  geom_point() +
  labs(x="Box ownership", y = "No of prospecting visits") +
  theme_classic(base_size = 25)
prospect_indiv_ownership_plot

prospect_indiv_sex_plot <- ggplot(individuals, aes(x = sex, y = visits)) + 
  geom_boxplot(width = 0.75) +
  geom_point() +
  labs(x="Sex", y = "No of prospecting visits") +
  theme_classic(base_size = 25)
prospect_indiv_sex_plot

prospect_indiv_age_plot2 <- ggplot(individuals, aes(x = min_age, y = boxes_visited)) + 
  geom_point() +
  geom_jitter(shape = 16, width = 0.2, height = 0, size = 1.5) +
  labs(x="Age", y = "No of boxes visited") +
  theme_classic(base_size = 25)
prospect_indiv_age_plot2

prospect_indiv_ownership_plot2 <- ggplot(individuals, aes(x = box_ownership, y = boxes_visited)) + 
  geom_boxplot(width = 0.75) +
  geom_point() +
  labs(x="Box ownership", y = "No of boxes visited") +
  theme_classic(base_size = 25)
prospect_indiv_ownership_plot2

prospect_indiv_sex_plot2 <- ggplot(individuals, aes(x = sex, y = boxes_visited)) + 
  geom_boxplot(width = 0.75) +
  geom_point() +
  labs(x="Sex", y = "No of boxes visited") +
  theme_classic(base_size = 25)
prospect_indiv_sex_plot2

prospect_indiv_plot <- ggarrange(prospect_indiv_age_plot, prospect_indiv_sex_plot, prospect_indiv_ownership_plot, prospect_indiv_age_plot2, prospect_indiv_sex_plot2, prospect_indiv_ownership_plot2, ncol = 3, nrow = 2, labels = c("a", "b", "c", "d", "e", "f"),  widths = c(1, 1.75),font.label = list(size = 20))
prospect_indiv_plot


#Use directory where you want to look for RT files, concatenate paths
RT_files <- list.files("E:/PhD/2024 Feeders and Nest Building/Nest Building Experiment", pattern = "RT", recursive = TRUE)
RT_paths <- paste("E:/PhD/2024 Feeders and Nest Building/Nest Building Experiment",RT_files, sep = "/")

#Load saved life history csv file 
LH <- read.csv("C:/Users/lh868/OneDrive - University of Exeter/Corvid Connections PhD/Chapter 5/Data/PhD Ch 5 Social Bonds and Animal Culture/Data/LH20231004.csv", header = T, stringsAsFactors = F)
LH$DATE <- strptime(LH$DATE,format="%d/%m/%Y")
LH$DATE <- as.Date(LH$DATE, format = "%d/%m/%Y") # convert to date

LH_ring <- subset(LH, !LH$COMBINATION == "")

#Create sub-strings that contain feeder ID (e.g. "Y1.1") and date
day_arrays <- unique(substr(RT_files,6,16))

#Create empty list to place data into as we go
collapsed_list <- list()

#Run through RT files in list 
for(i in 1:length(day_arrays)) {
  progress(i, max.value = length(day_arrays))
  
  day_array_list <- list()
  
  for (j in 1:length(RT_files[which(substr(RT_files,6,16) == day_arrays[i])])) {
    temp_day_file <- (read.delim(RT_paths[which(substr(RT_files,6,16) == day_arrays[i])][j], header = T, stringsAsFactors = F))[,1:13]
    temp_day_file$feeder <- substr(RT_files[which(substr(RT_files,6,16) == day_arrays[i])][j],6,9)
    day_array_list[[j]] <- temp_day_file
  }
  
  temp_RT <- do.call(rbind, day_array_list)
  
  temp_RT$Time <- strptime(paste(temp_RT$Date,stri_sub(temp_RT$Hmsec/1024,2,5), sep = ""), "%Y-%m-%d %H:%M:%OS")  # Add in miliseconds (1024 in a second) and format time
  
  temp_RT %>% filter(nchar(TagID_hex) == 10) -> temp_tags  #remove times when no tag
  
  
  if(dim(temp_tags)[1] >0){  
    
    visits <- data.frame(Event = temp_tags$Event, Start = temp_tags$Time, End = temp_tags$Time+(0.5*(temp_tags$Reps -1)), tag = temp_tags$TagID_hex, feeder = temp_tags$feeder)  # adds reps to visit length (0.5 seconds for every extra detection as that was resampling speed)
    visits <- arrange(visits, Start)
    visits <- arrange(visits, feeder)
    within_errors <- which(visits$End < lag(visits$End) & visits$tag == lag(visits$tag) & visits$feeder == lag(visits$feeder))
    if(length(within_errors) > 0){
      visits <- visits[-which(visits$End < lag(visits$End) & visits$tag == lag(visits$tag) & visits$feeder == lag(visits$feeder)),]  ## Get rid of reads within bouts - almost always erroneous single reads that are repeated later in the datastream
    }
    visits <- arrange(visits, Start)
    visits$count <- sapply(1:nrow(visits),function(x)sum(visits$tag[x]==visits$tag[1:x]))  ## add individual visit counter - if two bouts don't have sequential counts then bird seen elsewhere in between
    visits <- arrange(visits, feeder)
    
    visits$collapse <- "0"  # Temp column to tell me if this bout is to be collapsed
    
    visit_time <- 10 ## How long between detections before a new bout is classed
    
    # Is the last visit ending within 'visit_time' seconds of this one starting, and with the same tag & in sequence?
    visits$collapse[ which (visits$Start-lag(visits$End) < visit_time & lead(visits$Start) - visits$End < visit_time & visits$feeder == lag(visits$feeder) & visits$feeder == lead(visits$feeder) & visits$tag == lag(visits$tag) & visits$tag == lead(visits$tag) & (visits$count-lag(visits$count)) == 1 & (lead(visits$count)-visits$count) == 1 )] <- "1"  ## What about times they hop in between?!
    
    visits$collapse[which(visits$collapse == 0 & lag(visits$collapse == 1))] <- "End"  ## Can work out the end based on the 0s and 1s
    visits$collapse[which(visits$collapse == 0 & lead(visits$collapse == 1))] <- "Start" ## Can work out the start based on the 0s and 1s
    
    ## Mop up those that only have two potential detections (so no middle values to get assigned 1 above)
    visits$collapse[which(visits$collapse == 0 & lead(visits$Start) - visits$End < visit_time & lead(visits$Start) - visits$End > -2 & visits$tag == lead(visits$tag) & lead(visits$count) - visits$count == 1)] <- "Start"
    visits$collapse[which(visits$collapse == 0 & visits$Start - lag(visits$End) < visit_time & lag(visits$feeder) == visits$feeder & visits$tag == lag(visits$tag) & lag(visits$count) - visits$count == -1)] <- "End"
    
    # Add this file's data to the list
    collapsed_list[[i]] <- data.frame(start = visits$Start[which(visits$collapse %in% c("Start","0"))], end = visits$End[which(visits$collapse %in% c("End","0"))], tag = visits$tag[which(visits$collapse %in% c("Start","0"))], event = visits$Event[which(visits$collapse %in% c("Start","0"))], feeder =  visits$feeder[which(visits$collapse %in% c("Start","0"))], array = rep(stri_sub(day_arrays[i], 8,11), length(which(visits$collapse %in% c("Start","0")))))  }
}

#Turn the list into a data frame
visit_data <- do.call(rbind,collapsed_list)

#Find instances in which time is still NA
visit_data_NA <- subset(visit_data, is.na(visit_data$start))

#Filter instances in which time is not NA
visit_data <- subset(visit_data, !is.na(visit_data$start))

#Remove instances where JID = NA and test tags
visit_data$JID <- LH$ID[match(visit_data$tag,LH$RFID)]
visit_data <- subset(visit_data, JID != "NA")

distance_startdiff_m1 <- lmer(sqrt(StartDiffAbs) ~ Distance + (1|InputID) + (1|TargetID), data = distanceY)
distance_startdiff_m1 <- lmer(sqrt(StartDiffAbs) ~ Distance + (1|InputID) + (1|TargetID), data = distanceZ)
distance_startdiff_m1 <- lmer(sqrt(StartDiffAbs) ~ Distance + (1|InputID) + (1|TargetID), data = distanceX)

distance_startdiff_m1 <- glmmTMB(StartDiffAbs + 1 ~ Distance + (1|InputID) + (1|TargetID), data = distanceY, family = poisson)
distance_startdiff_m1 <- brm(StartDiffAbs ~ Distance, data = distanceY, family = poisson())

summary(distance_startdiff_m1)
Anova(distance_startdiff_m1)

plot(distanceZ$Distance, distanceZ$StartDiffAbs)
cor.test(distanceZ$Distance, distanceZ$StartDiffAbs)



#NBDA Archive ----


#Campus
event_data_x <- nests_summary_X[c("Box", "Start")]

event_data_x <- event_data_x  %>% rename(id = Box)
event_data_x <- event_data_x  %>% rename(time = Start)

event_data_x$t_end <- 31 

event_data_x$trial = 1

edge_list_x <- distanceX[, c("InputID", "TargetID", "Distance")]

dmin <- min(edge_list_x$Distance)
dmax <- max(edge_list_x$Distance)

edge_list_x$weight <- (dmax - edge_list_x$Distance) / (dmax - dmin)
edge_list_x$Distance <- NULL

edge_list_x <- edge_list_x  %>% rename(from = InputID)
edge_list_x <- edge_list_x  %>% rename(to = TargetID)

edge_list_x$trial <- 1


data_list <- import_user_STb(event_data = event_data, 
                             networks = edge_list,
                             network_type = "undirected")

model_full <- generate_STb_model(data_list, gq = T, est_acqTime = T, data_type = c("continuous"))

full_fit <- fit_STb(data_list,
                    model_full,
                    parallel_chains = 4,
                    chains = 4,
                    cores = 4,
                    iter = 4000,
                    refresh=1000
)

STb_save(full_fit, output_dir = "cmdstan_saves", name="my_first_fit")

STb_summary(full_fit, digits = 3)

model_asoc = generate_STb_model(data_list, model_type="asocial")

asocial_fit = fit_STb(data_list,
                      model_asoc,
                      parallel_chains = 4,
                      chains = 4,
                      cores = 4,
                      iter = 4000,
                      refresh=1000)

STb_save(asocial_fit, output_dir = "cmdstan_saves", name="my_first_fit")

loo_output = STb_compare(full_fit, asocial_fit, method="loo-psis")

print(loo_output$comparison, simplify = FALSE)

#Stithians
event_data_y <- nests_summary_Y[c("Box", "Start")]

event_data_y <- event_data_y  %>% rename(id = Box)
event_data_y <- event_data_y  %>% rename(time = Start)

event_data_y$t_end <- 31 

event_data_y$trial = 1

edge_list_y <- distanceY[, c("InputID", "TargetID", "Distance")]

dmin <- min(edge_list_y$Distance)
dmax <- max(edge_list_y$Distance)

edge_list_y$weight <- (dmax - edge_list_y$Distance) / (dmax - dmin)
edge_list_y$Distance <- NULL

edge_list_y <- edge_list_y  %>% rename(from = InputID)
edge_list_y <- edge_list_y  %>% rename(to = TargetID)

edge_list_y$trial <- 1


data_list <- import_user_STb(event_data = event_data, 
                             networks = edge_list,
                             network_type = "undirected")

model_full <- generate_STb_model(data_list, gq = T, est_acqTime = T, data_type = c("continuous"))

full_fit <- fit_STb(data_list,
                    model_full,
                    parallel_chains = 4,
                    chains = 4,
                    cores = 4,
                    iter = 4000,
                    refresh=1000
)

STb_save(full_fit, output_dir = "cmdstan_saves", name="my_first_fit")

STb_summary(full_fit, digits = 3)

model_asoc = generate_STb_model(data_list, model_type="asocial")

asocial_fit = fit_STb(data_list,
                      model_asoc,
                      parallel_chains = 4,
                      chains = 4,
                      cores = 4,
                      iter = 4000,
                      refresh=1000)

STb_save(asocial_fit, output_dir = "cmdstan_saves", name="my_first_fit")

loo_output = STb_compare(full_fit, asocial_fit, method="loo-psis")

print(loo_output$comparison, simplify = FALSE)

#Pencoose
event_data_z <- nests_summary_Z[c("Box", "Start")]

event_data_z <- event_data_z  %>% rename(id = Box)
event_data_z <- event_data_z  %>% rename(time = Start)

event_data_z$t_end <- 31 

event_data_z <- subset(event_data_z, !id == "Z25")
event_data_z <- subset(event_data_z, !id == "Z46")

event_data_z$trial = 1


edge_list_z <- distanceZ[, c("InputID", "TargetID", "Distance")]
edge_list_z <- subset(edge_list_z, Distance < 50)

dmin <- min(edge_list_z$Distance)
dmax <- max(edge_list_z$Distance)

edge_list_z$weight <- (dmax - edge_list_z$Distance) / (dmax - dmin)
edge_list_z$Distance <- NULL

edge_list_z <- edge_list_z  %>% rename(from = InputID)
edge_list_z <- edge_list_z  %>% rename(to = TargetID)

edge_list_z <- subset(edge_list_z, !from == "Z09")
edge_list_z <- subset(edge_list_z, !to == "Z09")
edge_list_z <- subset(edge_list_z, !from == "Z25")
edge_list_z <- subset(edge_list_z, !to == "Z25")

edge_list_z$trial <- 1


data_list <- import_user_STb(event_data = event_data_z, 
                             networks = edge_list_z,
                             network_type = "undirected")

model_full <- generate_STb_model(data_list, gq = T, est_acqTime = T, data_type = c("continuous"))

full_fit <- fit_STb(data_list,
                    model_full,
                    parallel_chains = 4,
                    chains = 4,
                    cores = 4,
                    iter = 4000,
                    refresh=1000
)

STb_save(full_fit, output_dir = "cmdstan_saves", name="my_first_fit")

STb_summary(full_fit, digits = 3)

model_asoc = generate_STb_model(data_list, model_type="asocial")

asocial_fit = fit_STb(data_list,
                      model_asoc,
                      parallel_chains = 4,
                      chains = 4,
                      cores = 4,
                      iter = 4000,
                      refresh=1000)

STb_save(asocial_fit, output_dir = "cmdstan_saves", name="my_first_fit")

loo_output = STb_compare(full_fit, asocial_fit, method="loo-psis")

print(loo_output$comparison, simplify = FALSE)


#All sites
event_data <- nests_summary[c("Box", "Start")]

event_data <- event_data  %>% rename(id = Box)
event_data <- event_data  %>% rename(time = Start)

event_data$t_end <- 36 

event_data <- subset(event_data, !id == "Z25")
event_data <- subset(event_data, !id == "Z46")

#pair age unknown
event_data <- subset(event_data, !id == "X36")
event_data <- subset(event_data, !id == "X38")
event_data <- subset(event_data, !id == "Y12")

event_data$trial <- 1

#different trials per site
#event_data$trial <- ifelse(substr(event_data$id, 1,1) == "X", 1, ifelse(substr(event_data$id, 1,1) == "Y", 2, 3))

event_data$time <- as.numeric(event_data$time)

#edge_list <- rbind(edge_list_x, edge_list_y, edge_list_z)

#Inverted weighted edge weights
edge_list <- distance[, c("InputID", "TargetID", "Distance", "SiteComb")]
edge_list <- subset(edge_list, edge_list$SiteComb == "XX" | edge_list$SiteComb == "YY" | edge_list$SiteComb == "ZZ")

dmin <- min(edge_list$Distance)
dmax <- max(edge_list$Distance)

edge_list$weight <- (dmax - edge_list$Distance) / (dmax - dmin)
edge_list$Distance <- NULL
edge_list$SiteComb <- NULL

edge_list$trial <- 1

edge_list <- edge_list  %>% rename(from = InputID)
edge_list <- edge_list  %>% rename(to = TargetID)

#threshold binary edge weights
edge_list <- distance[, c("InputID", "TargetID", "Distance", "SiteComb")]
edge_list <- subset(edge_list, edge_list$SiteComb == "XX" | edge_list$SiteComb == "YY" | edge_list$SiteComb == "ZZ")

edge_list <- subset(edge_list, Distance < 50)
edge_list$weight <- 1
edge_list$Distance <- NULL
edge_list$SiteComb <- NULL

edge_list <- edge_list  %>% rename(from = InputID)
edge_list <- edge_list  %>% rename(to = TargetID)

edge_list$trial <- 1

#threshold inverted edge weights
edge_list <- distance[, c("InputID", "TargetID", "Distance", "SiteComb")]
edge_list <- subset(edge_list, edge_list$SiteComb == "XX" | edge_list$SiteComb == "YY" | edge_list$SiteComb == "ZZ")

edge_list <- subset(edge_list, Distance < 50)

edge_list <- edge_list  %>% rename(from = InputID)
edge_list <- edge_list  %>% rename(to = TargetID)

dmin <- min(edge_list$Distance)
dmax <- max(edge_list$Distance)

edge_list$weight <- (dmax - edge_list$Distance) / (dmax - dmin)
edge_list$Distance <- NULL

edge_list$trial <- 1
edge_list$SiteComb <- NULL

#different trials per site
#edge_list$trial <- ifelse(substr(edge_list$SiteComb, 1,1) == "X", 1, ifelse(substr(edge_list$SiteComb, 1,1) == "Y", 2, 3))

#remove
edge_list <- subset(edge_list, !from == "Z09")
edge_list <- subset(edge_list, !to == "Z09")
edge_list <- subset(edge_list, !from == "Z25")
edge_list <- subset(edge_list, !to == "Z25")

#remove when pair age fitted as ILV
edge_list <- subset(edge_list, !from == "X36")
edge_list <- subset(edge_list, !to == "X36")
edge_list <- subset(edge_list, !from == "X38")
edge_list <- subset(edge_list, !to == "X38")
edge_list <- subset(edge_list, !from == "Y12")
edge_list <- subset(edge_list, !to == "Y12")

#add missing boxes with no edges to edge list when subsetting
# X18, X20, Y12, Y14, Y31, Y32, Y33, Z03, Z41, Z42, Z43

edge_list[223,1] <- "X18"
edge_list[224,1] <- "X20"
edge_list[225,1] <- "Y12"
edge_list[226,1] <- "Y14"
edge_list[227,1] <- "Y31"
edge_list[228,1] <- "Y32"
edge_list[229,1] <- "Y33"
edge_list[230,1] <- "Z03"
edge_list[231,1] <- "Z41"
edge_list[232,1] <- "Z42"
edge_list[233,1] <- "Z43"

edge_list$SiteComb <- NULL

edge_list$trial <- 1

edge_list <- edge_list[c("from", "to", "trial", "weight")]

edge_list_spatial <- edge_list
edge_list_spatial$dyad_ID <- paste(edge_list_spatial$from, edge_list_spatial$to, sep = "_")
edge_list_spatial <- edge_list_spatial  %>% rename(spatial = weight)


#Data for NBDA
data_list <- import_user_STb(event_data = event_data, 
                             networks = edge_list,
                             network_type = "undirected")

#Including ILVs
nests_summary$SiteNum <- ifelse(nests_summary$Site == "X", 1, ifelse(nests_summary$Site == "Y",2,3))

ILV_c <- data.frame(
  id = nests_summary$Box,
  age = nests_summary$pair_age,
  site = nests_summary$SiteNum
)

ILV_c <- subset(ILV_c, !id == "Z25")
ILV_c <- subset(ILV_c, !id == "Z46")
ILV_c <- subset(ILV_c, !id == "X36")
ILV_c <- subset(ILV_c, !id == "X38")
ILV_c <- subset(ILV_c, !id == "Y12")

data_list <- import_user_STb(
  event_data = event_data,
  networks = edge_list,
  ILV_c = ILV_c,
  ILVi = c("age"),
  ILVs = c("age")
)

model_full <- generate_STb_model(data_list, 
                                 gq = T, 
                                 est_acqTime = T, 
                                 data_type = c("discrete"))

full_fit <- fit_STb(data_list,
                    model_full,
                    parallel_chains = 4,
                    chains = 4,
                    cores = 4,
                    iter = 4000,
                    refresh= 1000
)

STb_save(full_fit, output_dir = "cmdstan_saves", name="my_first_fit")

STb_summary(full_fit, digits = 3)

model_asoc = generate_STb_model(data_list, 
                                model_type="asocial")

asocial_fit = fit_STb(data_list,
                      model_asoc,
                      parallel_chains = 4,
                      chains = 4,
                      cores = 4,
                      iter = 4000,
                      refresh=1000)

STb_save(asocial_fit, output_dir = "cmdstan_saves", name="my_first_fit")

loo_output = STb_compare(full_fit, asocial_fit, method="loo-psis")

print(loo_output$comparison, simplify = FALSE)



#Prospecting networks and start of building

visits_prospect_owners <- subset(visits_prospect, !is.na(visitor_owned_box))
visits_prospect_owners$box_dyad_ID <- paste(visits_prospect_owners$box, visits_prospect_owners$visitor_owned_box, sep = "_")
visits_prospect_owners$dyad_ID <- ifelse(visits_prospect_owners$box < visits_prospect_owners$visitor_owned_box, paste(visits_prospect_owners$box, visits_prospect_owners$visitor_owned_box, sep = "_"), paste(visits_prospect_owners$visitor_owned_box, visits_prospect_owners$box, sep = "_"))
visits_prospect_owners$box_distance <- distance$Distance[match(visits_prospect_owners$dyad_ID, distance$dyad_ID)]
mean(visits_prospect_owners$box_distance)
sd(visits_prospect_owners$box_distance)

visits_prospect_owners_box_dyads <- as.data.frame(table(visits_prospect_owners$box_dyad_ID))

visits_prospect_owners_box_dyads <- visits_prospect_owners_box_dyads %>% rename(box_dyad_ID = Var1)
visits_prospect_owners_box_dyads <- visits_prospect_owners_box_dyads %>% rename(weight = Freq)

visits_prospect_owners_box_dyads$other <- visits_prospect_owners$box[match(visits_prospect_owners_box_dyads$box_dyad_ID, visits_prospect_owners$box_dyad_ID)]
visits_prospect_owners_box_dyads$focal <- visits_prospect_owners$visitor_owned_box[match(visits_prospect_owners_box_dyads$box_dyad_ID, visits_prospect_owners$box_dyad_ID)]

edge_list <- visits_prospect_owners_box_dyads
edge_list$box_dyad_ID <- NULL

edge_list <- edge_list %>% rename(from = other)
edge_list <- edge_list %>% rename(to = focal)

edge_list$trial <- 1
edge_list$weight <- 1

edge_list <- edge_list[c("from", "to", "trial", "weight")]

edge_list_prospect <- edge_list
edge_list_prospect$dyad_ID <- paste(edge_list_prospect$from, edge_list_prospect$to, sep = "_")
edge_list_prospect <- edge_list_prospect  %>% rename(prospect = weight)

unique_ids <- unique(c(edge_list$from, edge_list$to))
event_data <- data.frame(id = unique_ids)

event_data$logger_duration <- logger_duration$logger_duration_days[match(event_data$id, logger_duration$nest_id)]
event_data$logger_duration <- ifelse(is.na(event_data$logger_duration), 0, event_data$logger_duration)

event_data <- subset(event_data, logger_duration > 5)

event_data$trial <- 1
event_data$t_end <- 36
event_data$time <- nests_summary$Start[match(event_data$id, nests_summary$Box)]
event_data$time <- ifelse(event_data$time < 6, 0, event_data$time)

data_list <- import_user_STb(event_data = event_data, 
                             networks = edge_list,
                             network_type = "directed")

model_full <- generate_STb_model(data_list, 
                                 gq = T, 
                                 est_acqTime = T, 
                                 data_type = c("discrete"))

full_fit <- fit_STb(data_list,
                    model_full,
                    parallel_chains = 4,
                    chains = 4,
                    cores = 4,
                    iter = 4000,
                    refresh=1000
)

STb_save(full_fit, output_dir = "cmdstan_saves", name="my_first_fit")

STb_summary(full_fit, digits = 3)

model_asoc = generate_STb_model(data_list, model_type="asocial")

asocial_fit = fit_STb(data_list,
                      model_asoc,
                      parallel_chains = 4,
                      chains = 4,
                      cores = 4,
                      iter = 4000,
                      refresh=1000)

STb_save(asocial_fit, output_dir = "cmdstan_saves", name="my_first_fit")

loo_output = STb_compare(full_fit, asocial_fit, method="loo-psis")

print(loo_output$comparison, simplify = FALSE)


#Multi-network NDBDA: spatial and prospecting networks

#event data

#event_data <- nests_summary[c("Box", "Start")]
event_data <- nests_summary[c("Box", "Start", "squirrel")]

event_data <- subset(event_data, !event_data$squirrel == 1)

event_data <- event_data  %>% rename(id = Box)
event_data <- event_data  %>% rename(time = Start)

event_data$trial <- 1
event_data$t_end <- 31

event_data$t_end <- 36 

event_data$squirrel <- NULL

event_data$time <- ifelse(event_data$time < 6, 0, event_data$time)

event_data <- subset(event_data, !is.na(event_data$time))

event_data <- subset(event_data, !event_data$id == "Z46")


edge_list <- distance[, c("InputID", "TargetID", "Distance", "SiteComb")]
edge_list <- subset(edge_list, edge_list$SiteComb == "XX" | edge_list$SiteComb == "YY" | edge_list$SiteComb == "ZZ")

dmin <- min(edge_list$Distance)
dmax <- max(edge_list$Distance)

edge_list$trial <- 1

edge_list$spatial_weight <- (dmax - edge_list$Distance) / (dmax - dmin)
edge_list$Distance <- NULL
edge_list$SiteComb <- NULL

edge_list <- edge_list  %>% rename(from = InputID)
edge_list <- edge_list  %>% rename(to = TargetID)

edge_list_spatial <- edge_list

edge_list_spatial <- edge_list_spatial %>%
  filter(from %in% event_data$id)

edge_list_spatial <- edge_list_spatial %>%
  filter(to %in% event_data$id)

edge_list_spatial$box_dyad_ID <- paste(edge_list_spatial$from, edge_list_spatial$to, sep = "_")

edge_list_prospect <- visits_prospect_owners_box_dyads

edge_list_prospect <- edge_list_prospect %>% rename(from = other)
edge_list_prospect <- edge_list_prospect %>% rename(to = focal)

edge_list_prospect$trial <- 1
edge_list_prospect$prospect_weight <- 1

edge_list_prospect <- edge_list_prospect[c("box_dyad_ID", "from", "to", "trial", "prospect_weight")]


edge_list <- edge_list_spatial
edge_list$prospect_weight <- edge_list_prospect$prospect_weight[match(edge_list$box_dyad_ID, edge_list_prospect$box_dyad_ID)]

edge_list$box_dyad_ID <- NULL


edge_list$prospect_weight <- ifelse(is.na(edge_list$prospect_weight), 0, 1)



nests_start_logger <- nests_summary[, c("Box", "Site", "Start")]
nests_start_logger$Logger <- logger_duration$logger_duration_days[match(nests_start_logger$Box, logger_duration$nest_id)]

nests_start_logger <- subset(nests_start_logger, !is.na(nests_start_logger$Start))

event_data

edge_list <- edge_list_spatial
edge_list$prospect <- edge_list_prospect$prospect[match(edge_list$dyad_ID, edge_list_prospect$dyad_ID)]
edge_list$dyad_ID <- NULL
edge_list$box_dyad_ID <- NULL

edge_list$prospect <- ifelse(!is.na(edge_list$prospect), 1,0)
edge_list$prospect_weight <- ifelse(!is.na(edge_list$prospect_weight), 1,0)

data_list <- import_user_STb(
  event_data = event_data,
  networks = edge_list)

model_full <- generate_STb_model(data_list, 
                                 gq = T, 
                                 est_acqTime = T, 
                                 data_type = c("discrete"))

full_fit <- fit_STb(data_list,
                    model_full,
                    parallel_chains = 4,
                    chains = 4,
                    cores = 4,
                    iter = 4000,
                    refresh=1000
)

STb_save(full_fit, output_dir = "cmdstan_saves", name="my_first_fit")

STb_summary(full_fit, digits = 3)

model_asoc = generate_STb_model(data_list, model_type="asocial")

asocial_fit = fit_STb(data_list,
                      model_asoc,
                      parallel_chains = 4,
                      chains = 4,
                      cores = 4,
                      iter = 4000,
                      refresh=1000)

STb_save(asocial_fit, output_dir = "cmdstan_saves", name="my_first_fit")

loo_output = STb_compare(full_fit, asocial_fit, method="loo-psis")

print(loo_output$comparison, simplify = FALSE)

set.seed(12)

### PARAMETERS
n1 <- 84
n2 <- 65
t_end <- 56

ids <- paste0("I", sprintf("%03d", 1:n1))

# 3 groups (roughly balanced)
groups <- sample(rep(1:3, length.out = n1))

# trial 1 full population
trial1_ids <- ids

# trial 2 is a subset of trial 1
trial2_ids <- sample(ids, n2, replace = FALSE)

