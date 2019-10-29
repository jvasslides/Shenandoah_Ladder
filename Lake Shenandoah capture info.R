#################
##Lake Shenandoah tagging project##
################

#rm(list=ls())

#setwd("Z:/Jim/Long term juvenile sampling/analysis")
#library(reshape2)
library(dplyr)
#library(tidyr)
library("xlsx")

######################info on all herring captured in fyke###################################
library(RODBC)
db<- "Z:/BBP Public/BBP Research Data/Barnegat Bay Partnership Research Database.accdb"
con <- odbcConnectAccess2007(db)
  #con <- odbcConnect("BBPData")
collections <- sqlFetch(con, "tblCollections")
lengths <- sqlFetch(con, "tblSpecimens")
species <- sqlFetch(con, "tblTowSpecies")
tows <- sqlFetch(con, "tblTows")

combo1 <- merge(tows, collections, by.x= "CollectionID", by.y="CollectionID", all.x=TRUE)


fyke <- subset(combo1, combo1$`Project Name` == "Lake Shenandoah Fish Ladder")
fyke <- droplevels.data.frame(fyke)

merged <- merge (fyke, species, by.x = "TowID", by.y = "TowID", all.x = TRUE, sort = TRUE)
merged <- filter(merged, Genus == "Alosa") 
merged <- droplevels(merged)
merged$'Total Number'[is.na(merged$'Total Number')] <- 0

captured<- select(merged, CollectionDt, Genus, Species, "Total Number")


#####################################tagged fish only##################################

fish <-read.xlsx("Z:/BBP Projects/Herring work/Lake Shenandoah ladder/Analysis/tagged fish.xlsx", sheetName = "Sheet1", as.data.frame=TRUE, header=TRUE)
fish$Year <- as.factor(format(fish$Date, format = "%Y"))


fish_by_year <-fish %>% 
  group_by(Year,Species) %>% 
  summarise(
    number=n(),
    maxSize=max(Length..mm.),
    minSize=min(Length..mm.),
    avgSize=mean(Length..mm.),
    maxAge=max(Age, na.rm = TRUE),
    minAge=min(Age, na.rm = TRUE),
    avgAge=mean(Age, na.rm = TRUE)
    )

mod1 <- wilcox.test(Length..mm. ~ Year, data=fish, exact=FALSE)
mod1

fish_size_year <-fish %>% 
  group_by(Year) %>% 
  summarise(
    number=n(),
    maxSize=max(Length..mm.),
    minSize=min(Length..mm.),
    avgSize=mean(Length..mm.)
    )

mod2 <- wilcox.test(Length..mm. ~ Species, data=fish, exact=FALSE)
mod2

fish_size_species <-fish %>% 
  group_by(Species) %>% 
  summarise(
    number=n(),
    maxSize=max(Length..mm.),
    minSize=min(Length..mm.),
    avgSize=mean(Length..mm.)
  )
mod3 <- wilcox.test(Age~Year, data=fish, exact=FALSE)
mod3
fish_age_yr <-fish %>% 
  group_by(Year) %>% 
  summarise(
    number=n(),
    avgAge=mean(Age, na.rm = TRUE)
  )

mod4 <- wilcox.test(Age~Species, data=fish, exact=FALSE)
mod4
fish_age_species <-fish %>% 
  group_by(Species) %>% 
  summarise(
    number=n(),
    avgAge=mean(Age, na.rm = TRUE)
  )

Table1 <-fish %>% 
    group_by(Date,Species) %>% 
    summarise(
      number=n(),
      avgSize=mean(Length..mm.),
      avgAge=mean(Age, na.rm=TRUE)
    )


