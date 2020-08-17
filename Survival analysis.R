###########################
#####Survival analysis#####
###########################

library(PITR)
library("xlsx")
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

######assign test tag numbers#####
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

detectlist<- detections%>%   #############generates a list of tag codes recorded on A1 and A2
  filter (antenna != 3) %>%
  distinct (tag_code)

detectlist$dummy <-1

foo <- left_join(fish,detectlist, by = "tag_code")  #######bind the detected codes onto the list of all fish
foo$dummy [is.na(foo$dummy)] <- 0


##########################################################################################
###create dataframe that contains an entry for each day during the season              ###
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
###assign a date to each day a fish was at large###
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
###       for all fish         ###  
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

################################################
### Split out fish that were never detected  ###
## and Add Censor Values for Undetected Fish ###
################################################
undetected <- filter(fullset, dummy != 1)  #filter only to fish that were never detected

undetected$Dam <- 1 #number of exposures to the dam - only 1 because a change in covariates does not count as a new exposure
undetected$Ladder <- "" #number of exposures to the ladder - unused for this group as they never try to get up the ladder
undetected$Ladder<- as.numeric(undetected$Ladder)
undetected$censor_approach <- 2 # censor code for the approach; 1 = enter ladder, 2 = change of covariates
undetected$censor_ladder <- "" #censor code for the ladder; 0= fails to pass, 1 = passes, 2= change of covariates 
undetected$censor_ladder<- as.numeric(undetected$censor_ladder)


##############################################
###Create A Record For Each Detection Event###
##############################################

####generates a df of detection records from A1 and A2
detected<- detections%>%   
  filter (antenna != 3) %>%
  select (tag_code, antenna, date_time) %>%
  arrange(tag_code, date_time, -antenna)

####Identify which detections should count as a single exposure 
####(where the interval between detections < 2 minutes)


#calculate the interval between two detections for the same tag code; 
#if different tag codes set interval to 0 

detected$interval <- 0

for (i in 2:nrow(detected)) {  
  
  if(detected[i-1,]$tag_code == detected[i,]$tag_code) {
    detected[i,]$interval <-int_length (detected[i-1,]$date_time %--% detected[i,]$date_time) 
  } else {
    detected[i,]$interval <- 0
  }
}

# if interval is less than 2 minutes set dummy =1, if interval is 0 or >2 set dummy =0

detected <- detected %>%
  mutate (dummy = case_when (
    interval == 0 ~ 0,
    interval <= 120 ~ 1,
    interval > 120 ~ 0
  ))


####Remove intermediate detections within a single exposure
##group by tag code and antenna; if interval is < 2 min, assign dummy2=1;
##if interval is > 2 min, assign dummy =2

detected$dummy_lead<- lead(detected$dummy,1) #creates a new variable that is basically dummy[i+1]

detected <- detected %>%
  group_by(tag_code, antenna) %>%
  mutate (dummy2 = case_when(
    dummy == 0 & dummy_lead ==0 ~ 2,
    dummy ==1 & dummy_lead ==1 ~1,
    TRUE ~ 2))
detected<- ungroup(detected)

detected<- filter (detected, dummy2 != 1)

##############there are too many peculiarities in the dataframe##################### 
########to set up conditional statements - had to spit out and massage by hand
    #removed exposures < 10 seconds
    #removed single detections at A2 (swim bys)
    #fish only detected at A1 #135167 exposure is total time recorded at A1
    #fish only detected at A1 #137664 beginning exposure set at 12pm
    #removed downstream detection (June 4) for #137665

#write.csv(detected,"C:/Users/jvasslides/Documents/R_Projects/Shenandoah_Ladder/detected.csv")

################################################################################################

detected2 <- read.xlsx("C:/Users/jvasslides/Documents/R_Projects/Shenandoah_Ladder/detected2.xlsx",
                      sheetName = "detected2", as.data.frame = TRUE, header=TRUE) 

detected2$censor_ladder[is.na(detected2$censor_ladder)] <-0
detected2$tag_code <- as.character(detected2$tag_code)

detected2 <- rename(detected2, exposure_start_date = date_time) 
detected2 <- left_join(detected2,fish, by = "tag_code")

attr(detected2$exposure_start_date, "tzone") <- "US/Eastern" #was having funky timeshifts when rbinding below
attr(detected2$exposure_end_date, "tzone") <- "US/Eastern" #was having funky timeshifts when rbinding below

#calculate number of hours since taggging that exposure began and ended, and exposure_duration
detected2$exposure_begin <-(int_length (detected2$Tag_date %--% detected2$exposure_start_date))/(60*60)
detected2$exposure_end <- (int_length (detected2$Tag_date %--% detected2$exposure_end_date))/(60*60)
detected2$exposure_duration <- detected2$exposure_end - detected2$exposure_begin

detected2$censor_approach <- "" #blank as these records are for fish that found the ladder
detected2$censor_approach <- as.numeric(detected2$censor_approach)

#drop unneeded columns and reorder
detected2 <- select(detected2, tag_code, Tag_date, exposure_start_date, exposure_end_date,
                    exposure_begin, exposure_end, exposure_duration, censor_approach, censor_ladder,
                    Species:Age)

################################################
###Combine Detection Events with the rest    ###
###of the monitoring period for detected fish###                                               
################################################

detects <- filter(fullset, dummy == 1)  #filter the full monitoring period for fish that were detected
detects[,"exposure_end_date"] <- as.POSIXct(NA)
attr(detects$exposure_end_date, "tzone") <- "US/Eastern" #was having funky timeshifts when rbinding below
detects <- rename(detects, exposure_start_date = exposure_date)
attr(detects$exposure_start_date, "tzone") <- "US/Eastern" #was having funky timeshifts when rbinding below
detects$censor_ladder <- "" #blank because they never find the ladder (censored approach)  
detects$censor_ladder<- as.numeric(detects$censor_ladder)
detects$censor_approach <- "" #fill in after combining with detections
detects$censor_approach <- as.numeric(detects$censor_approach)


detects <- select(detects, tag_code, Tag_date, exposure_start_date, exposure_end_date,
                    exposure_begin, exposure_end, exposure_duration, censor_ladder, censor_approach,
                    Species:Age)

alldetects <- bind_rows(detected2, detects)

alldetects <- arrange(alldetects, tag_code, exposure_begin)

#write.csv(alldetects,"C:/Users/jvasslides/Documents/R_Projects/Shenandoah_Ladder/alldetects.csv")

#############################################################################
### kick alldetects into excel to make the following changes :( to much of a pain to figure out in R
### 1. adjust overlapping exposures
### 2. delete false exposures (i.e. after a fish passes the ladder)
### 3. change censor_approach values
### 4. set exposure # for dam and ladder
### 5. for fish tagged in 2013 and picked up on the system in 2014 (135103, 135167,137634, 137678)
###     a. set 2014 tag_date as 4/11/14 and calculate times from there
###     b. first day of 2014 begins new exposure_dam
###     c. assume fish are in the system beginning 4/11 

#alldetects2 <-read.csv ("C:/Users/jvasslides/Documents/R_Projects/Shenandoah_Ladder/alldetects2.csv")


############################################################################
####reformat/reorder detect and non-detect datasets to match and combine####
############################################################################
alldetects2$Tag_date <- as.character(alldetects2$Tag_date)
alldetects2$Tag_date <- as.POSIXct(alldetects2$Tag_date, tz = "US/Eastern", "%m/%d/%Y %H:%M")
alldetects2$exposure_start_date <- as.character(alldetects2$exposure_start_date)
alldetects2$exposure_start_date <- as.POSIXct(alldetects2$exposure_start_date, tz = "US/Eastern", "%m/%d/%Y %H:%M")

alldetects2$tag_code <- as.character(alldetects2$tag_code)

alldetects2 <- alldetects2 %>%
  rename(Length = Length..mm.) %>%
  rename(Weight = Weight..g.) %>%
  select(tag_code, Tag_date, exposure_dam, exposure_ladder, exposure_start_date, exposure_begin, exposure_end, exposure_duration, censor_approach, censor_ladder, Species, Length, Weight, Age)

undected2 <- undetected %>%
  rename(exposure_dam = Dam) %>%
  rename(exposure_ladder = Ladder) %>%
  rename(exposure_start_date = exposure_date) %>%
  rename(Length = Length..mm.) %>%
  rename(Weight = Weight..g.) %>%
  select(tag_code, Tag_date, exposure_dam, exposure_ladder, exposure_start_date, exposure_begin, exposure_end, exposure_duration, censor_approach, censor_ladder, Species, Length, Weight, Age)

tagdata <- bind_rows(alldetects2,undected2)


##############################################################################
###############bring in covariate data and combine with tag data##############
##############################################################################

tempdata <-read.csv ("C:/Users/jvasslides/Documents/R_Projects/Shenandoah_Ladder/Temp_Corrected.csv")

tempdata$Date<- as.Date(tempdata$Date)
tempdata <- select(tempdata, Date, avgTemp)
  
discharge <-read.csv ("C:/Users/jvasslides/Documents/R_Projects/Shenandoah_Ladder/USGS discharge data.csv")
#############THERE ARE MISSING DAYS FROM THE USGS DATA RECORD IN 2013

discharge$Date_Time <- as.character(discharge$Date_Time)
discharge$Date_Time <- as.POSIXct(discharge$Date_Time, tz = "US/Eastern", "%m/%d/%Y %H:%M")

discharge$Date <- date(discharge$Date_Time)

discharge <- discharge %>%
  group_by(Date) %>%
  summarise(avg_discharge = mean(Discharge))

covariates <- full_join(tempdata, discharge, by = "Date")

tagdata$Date <- date(tagdata$exposure_start_date)

data_final <- left_join(tagdata, covariates, by = "Date")


########################################time for the actual analysis############











