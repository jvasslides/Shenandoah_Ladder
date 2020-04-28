###########################
#####Survival analysis#####
###########################

library(PITR)
#library("xlsx")
library(dplyr)
library(lubridate)

#######################################################
######    bring in detection data from         ########
######    from tag readers using PIT           ########
#######################################################

#####2013 files are from firmware earlier than V5 (April 2014)
old <- "C:/Users/jvasslides/Documents/R_Projects/Shenandoah_Ladder/array_old"

######2014 files are from current firmware
new <- "C:/Users/jvasslides/Documents/R_Projects/Shenandoah_Ladder/array_new"

######assign text tag numbers#####
tt <- c("0000_0000000174764544","0000_0000000174764573", "0000_0000000180573686", "0000_0000000181177608", "0000_0000000174764573", "0000_00000181177608")

####collate the data####
old_dat <- old_pit(data=old, test_tags = tt, print_to_file = FALSE)

alldat <- new_pit(data=new, test_tags = tt, print_to_file = FALSE, old_format_data = old_dat)

detections<- alldat$all_det ####dataframe with only detections of tagged fish
detections<- filter(detections, tag_code != "900_226000135123") ##tag used for test on exit antenna
detections<- filter(detections, tag_code != "900_226000135118") ##tag used for test on exit antenna

###########################################################
###  read in tagging information from outside file      ###
###########################################################

fish <-read.csv("C:/Users/jvasslides/Documents/R_Projects/Shenandoah_Ladder/tagged fish.csv",header=TRUE)
#fish <-read.xlsx("C:/Users/jvasslides/Documents/R_Projects/Shenandoah_Ladder/tagged fish.xlsx", sheetName = "Sheet1", as.data.frame=TRUE, header=TRUE)
fish$tag <- '900_'
fish$Tag..<- as.character((fish$Tag..))
fish$tag_code <- paste(fish$tag, fish$Tag.., sep = "")

fish$Date <- as.character(fish$Date)
fish$tag_time <- "12:00:00" #assign a time of noon to all tagging events
fish$Tag_date <-as.POSIXct (paste(fish$Date, fish$tag_time, sep = " "))

fish<- fish %>%
    select (-'tag', -'Tag..', -'Date', -'tag_time') 
    

#################################################################################
### split dataset into fish that get picked up on an antenna and those that don't###
#################################################################################

detects<- detections%>%   #############generates a list of tag codes recorded on A1 and A2
  filter (antenna != 3) %>%
  distinct (tag_code)

detects$dummy <-1

foo <- left_join(fish,detects, by = "tag_code")  #######bind the detected codes onto the list of all fish
foo$dummy [is.na(foo$dummy)] <- 0

#undetected <- filter(foo, dummy != 1)  #filter only to fish that were never detected

##########################################################################################
###create dataframe that contains an entry for each day during the season for each undetected fish###
##########################################################################################
interval2013<- interval(ymd(20130415), ymd(20130619)) #figure out how long the antenna was active in 2013
int_length(interval2013)/(60*60*24) #int_length is in seconds; divide by 60*60*24 to determine days
interval2014<- interval(ymd(20140411), ymd(20140701)) #figure out how long the antenna was active in 20014
int_length(interval2014)/ (60*60*24) #int_length is in seconds; divide by 60*60*24 to determine days

fullset <-bind_rows(replicate(82, foo, simplify = FALSE)) #replicate each tag code the max number of days in the study
fullset <-arrange(fullset, tag_code)

dummy2 <- (c(rep(c(0:81),nrow(fish))))
dummy2 <- data.frame(dummy2)
dummy2$dummy3 <- dummy2$dummy2*(60*60*24) #number of seconds in each day (dummy2)
fullset <- bind_cols(fullset,dummy2)
rm(dummy2)

#####################################################
###assign a date to each day the fish was at large###
#####################################################
n <- nrow(fullset)

fullset$exposure_date <- fullset$Tag_date

for (i in 1:n) {  
  
    if(fullset[i,]$dummy3 == 0) { 
        fullset[i,]$exposure_date <- fullset[i,]$Tag_date 
  } else {
    fullset[i,]$exposure_date <- fullset[i,]$Tag_date + fullset[i,]$dummy3 
  }
}


fullset <- fullset %>% #cuts out extra dates from when the system was not on
  filter(exposure_date < "2013-06-20" | exposure_date > "2014-04-11") %>%
  filter (exposure_date < "2014-07-02") 
  
  
##################################
###calculate exposure intervals###
##################################

#assign a time of midnight to the start of each exposure event other than the day of tagging
z <- nrow(fullset)

for (i in 1:z) {  
  
  if(fullset[i,]$dummy3 == 0) { 
    fullset[i,]$exposure_date <- fullset[i,]$exposure_date 
  } else {
    hour(fullset[i,]$exposure_date) <- 0 
  }
}

#number of hours since release that exposure began
fullset$exposure_begin <-(int_length (fullset$Tag_date %--% fullset$exposure_date))/(60*60) 

#number of hours since release that exposure ends 
#BRUTE FORCE, SHOULD BE A MORE ELEGANT ANSWER USING A LOOP AND ASSIGNING THE BEGINNING OF THE NEXT EXPOSURE
#BUT NEEDS TO BE DONE BY TAG CODE, AND WHAT HAPPENS ON THE LAST DAY

fullset$exposure_end <- fullset$exposure_begin

for (i in 1:z) {  
  
  if(fullset[i,]$dummy3 == 0) { 
    fullset[i,]$exposure_end <- fullset[i,]$exposure_begin + 12 #new exposure begins 12 hrs after tagging
  } else {
    fullset[i,]$exposure_end <- fullset[i,]$exposure_begin + 24 #new exposure begins each subsequent day
  }
}

fullset$exposure_duration <- fullset$exposure_end - fullset$exposure_begin

##################
### Censor for Undetected Fish ###
##################
undetected <- filter(fullset, dummy != 1)  #filter only to fish that were never detected

undetected$Dam <- 1 #number of exposures to the dam - only 1 becuase a change in covariates does not count as a new exposure
undetected$Ladder <- "" #number of exposures to the ladder - unused for this group as they never try to get up the ladder
undetected$Ladder<- as.numeric(undetected$Ladder)
undetected$censor_approach <- 2 # censor code for the approach; 1 = enter ladder, 2 = change of covariates
undetected$censor_ladder <- "" #censor code for the ladder; 0= fails to pass, 1 = passes, 2= change of covariates 
undetected$censor_ladder<- as.numeric(undetected$censor_ladder)


###add in exposure events for detected fish###

  #2 - create record line for each detection event - maybe assign ladde exposures here
  #3 - "join" events to make sure there is a complete record for each day - no time gaps
  #4 - figure out how to assign dam Exposures -


#clean up and reorder dataframe
#undetected_final <- select (undetected2, Species, tag_code, Tag_date, exposure_date:censor_ladder, Length..mm., Weight..g.)

###need to add covariates once all of the exposure events are in the dataframe









