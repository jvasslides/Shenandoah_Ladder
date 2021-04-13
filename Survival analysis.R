###########################
#####Survival analysis#####
###########################

library(PITR)
#library("xlsx")
library(tidyverse)
library(lubridate)
library(survival)

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
old_dat$all_det$date_time <- force_tz(old_dat$all_det$date_time, tzone = "America/New_York") #timezone was set as UTC but the clock time was actually New York based on marker tags

new_dat <- new_pit(data=new, test_tags = tt, print_to_file = FALSE)

detections<- rbind(old_dat$all_det, new_dat$all_det) ####dataframe with only detections of tagged fish (monitoring tags removed)

#########removes tags used to test the system (picked up on array prior to being inserted in a fish)
detections <- detections[!(detections$tag_code == "900_226000135123" & detections$date_time < "2013-04-30"),]
detections <- detections[!(detections$tag_code == "900_226000135142" & detections$date_time < "2013-04-30"),]
detections <- detections[!(detections$tag_code == "900_226000135121" & detections$date_time < "2013-04-30"),]
detections <- detections[!(detections$tag_code == "900_226000135144" & detections$date_time < "2013-04-30"),]
detections <- detections[!(detections$tag_code == "900_226000135148" & detections$date_time < "2013-04-30"),]

###########################################################
###  read in tagging information from outside file      ###
###########################################################

fish <-read.csv("C:/Users/jvasslides/Documents/R_Projects/Shenandoah_Ladder/tagged fish.csv",header=TRUE)
#fish <-read.xlsx("C:/Users/jvasslides/Documents/R_Projects/Shenandoah_Ladder/tagged fish.xlsx", sheetName = "Sheet1", as.data.frame=TRUE, header=TRUE)
fish$tag <- '900_'
fish$Tag..<- as.character((fish$Tag..))
fish$tag_code <- paste(fish$tag, fish$Tag.., sep = "")

fish$Date <- as.character(fish$Date)
fish$tag_time <- "12:00:00" 
fish$tmp <-as.POSIXct (paste(fish$Date, fish$tag_time, sep = " "))
fish$Tag_date <- with_tz(fish$tmp, tzone = "America/New_York")#assign a time of noon to all tagging events

fish<- fish %>%
    select (-'tag', -'Tag..', -'Date', -'tag_time', -'tmp') 
    

####################################################
############Basic analysis on tag returns###########
####################################################

b<- left_join(detections, fish, by = "tag_code")
b$tal <- (int_length (b$Tag_date %--% b$date_time))/(60*60) #time (in hours) between when a fish was tagged and when it is detected on the array
b$year <- as.factor(year(b$date_time))

################time to return after tagging 2013 only##############
#fish were tagged in 2013 and detected on any antenna during 2013
Time_to_return <- b %>%
  filter (year ==2013) %>%
  group_by(tag_code) %>%
  summarise(minttr = min(tal)) %>%
  summarise(ttr_min = min(minttr), ttr_max = max(minttr), ttr_mean = mean(minttr), ttr_sd = sd(minttr))
Time_to_return

###############2013 Duration of migratory effort #################
#(i.e. hours from tag to last record)
Duration_Migratory_Effort <- b %>%
  filter (year ==2013) %>%
  group_by(tag_code) %>%
  summarise(indDME = max(tal)) %>%
  summarise(DME_min = min(indDME), DME_max = max(indDME), DME_mean = mean(indDME), DME_sd = sd(indDME)) #median time from tagging to last record is 15.0 days
Duration_Migratory_Effort

################Proportion returning 2013 only##################
tags_2013<- fish %>%
  mutate(Year = year(Tag_date)) %>%
  filter(Year == 2013)

detected_2013 <- detections %>%
  mutate (Year = year(date_time)) %>%
  filter(Year ==2013) %>%
  distinct(tag_code)

Prop_return_2013<- (nrow(detected_2013)/nrow(tags_2013))*100

################Proportion of returners entering -  2013 only############
detected_entry_2013 <- detections %>%
  mutate (Year = year(date_time)) %>%
  filter (Year == 2013 & antenna == 2) %>%
  count(tag_code)

Prop_return_enter_2013 <- (nrow(detected_entry_2013)/nrow(detected_2013))*100


############################2014 detections###########################
detected_2014 <- detections %>%
  mutate (Year = year(date_time)) %>%
  filter (Year == 2014) %>%
  left_join(fish, by="tag_code") %>%
  mutate (Year_tagged = year(Tag_date)) 

detected_2014_A3 <- detected_2014 %>%
  filter(antenna ==3) %>%
  distinct(tag_code, .keep_all = TRUE) 
  
detected_2014_A2 <- detected_2014 %>%
  filter(antenna ==2) %>%
  distinct(tag_code, .keep_all = TRUE) 

A2andA3<- semi_join(detected_2014_A2,detected_2014_A3, by = "tag_code") #fish recorded at both A3 and A2
A2notA3 <-anti_join(detected_2014_A2,detected_2014_A3, by = "tag_code") #fish recorded at A2 and not A3

detected_2014_A1<- detected_2014 %>%
  filter(antenna ==1) %>%
  distinct(tag_code, .keep_all = TRUE) 

###############Proportion of tagged fish entering the ladder#################

Prop_tag_enter_2013 <- (nrow(detected_entry_2013)/nrow(tags_2013))*100

tags_2014<- fish %>%
  mutate(Year = year(Tag_date)) %>%
  filter(Year == 2014)

detected_entry_2014 <- detections %>%
  mutate (Year = year(date_time)) %>%
  filter (Year == 2014 & antenna == 2) 

#remove fish tagged in 2013
detected_entry_2014 <- semi_join(detected_entry_2014, tags_2014, by="tag_code") %>% 
  count(tag_code)

Prop_tag_enter_2014 <- (nrow(detected_entry_2014)/nrow(tags_2014))*100


#################################################################################
###################Kaplan-Meir estimate of migration duration for 2013###########
#################################################################################

#build datasets:  time = tag date-time of last detection  

#dataset for fish tagged in 2013 and detected in the array in 2013
km2013det <-b %>%
  filter(year ==2013 & tal >0) %>%
  group_by(tag_code) %>%
  arrange(tag_code, desc(tal)) %>%
  top_n(1,tal)%>%
  mutate (event= 1) %>%
  select(tag_code, tal, event)

#dataset for fish tagged in 2013 and NOT DETECTED in the array
fish2013<- fish %>%
  mutate(year= year(Tag_date)) %>%
  filter(year == 2013)

km2013nodet <- anti_join(fish2013,detected_2013, by = 'tag_code') %>%  #create a df retaining fish that were tagged, but not detected in 2013
  mutate(censor_date = "2013-06-06") %>%                #add a censor date=last day that a tag was detected in 2013
  mutate(tal = (int_length (Tag_date %--% censor_date))/(60*60*24))  %>% #creates a 'time at large'
  mutate (event=0) %>%
  select (tag_code, tal, event)

km2013 <-bind_rows(km2013det, km2013nodet)

surv.obj<- Surv(km2013$tal, km2013$event, type = 'right')
KM<-survfit(surv.obj~1)
plot(KM, xlab = "Time in Days", ylab = "Survivial function")


surv.obj2 <- Surv(km2013det$tal, km2013det$event, type = 'right')
KM2<-survfit(surv.obj2~1)
plot(KM2, xlab = "Time in Days", ylab = "Survivial function")


#############################################################################################
###################################Cox PH models#############################################
#############################################################################################

###this is for entry/rejection rates only - so they either enter the ladder or fail to enter


detectlist<- detections%>%   #############generates a list of tag codes in the array
  distinct (tag_code)
detectlist$ping <-1         #####sets a marker - 1 if tag code detected

foo <- left_join(fish,detectlist, by = "tag_code")  #######bind the detected codes onto the list of all fish
foo$ping [is.na(foo$ping)] <- 0 ###sets a marker - 0 if tag not detected in the array

####create dataframe that contains an entry for each day during the season ####
interval2013<- interval(ymd(20130415), ymd(20130619)) #figure out how long the antenna was active in 2013
int_length(interval2013)/(60*60*24) #int_length is in seconds; divide by 60*60*24 to determine days
interval2014<- interval(ymd(20140411), ymd(20140701)) #figure out how long the antenna was active in 20014
int_length(interval2014)/ (60*60*24) #int_length is in seconds; divide by 60*60*24 to determine days

fullset <- bind_rows(replicate(82, foo, simplify = FALSE)) %>% #replicate each tag code the max number of days in the study
  arrange(tag_code)

dummy2 <- (c(rep(c(0:81),nrow(fish))))
dummy2 <- data.frame(dummy2)
dummy2$dummy3 <- dummy2$dummy2*(60*60*24) #number of seconds in each day (dummy2)
fullset <- bind_cols(fullset,dummy2)
rm(list = c("dummy2", "foo"))

#####################################################
###assign a date to each day during the season###
#####################################################
n <- nrow(fullset)

fullset$Exposure_Start <- fullset$Tag_date

for (i in 1:n) {  
  
  if(fullset[i,]$dummy3 == 0) { 
    fullset[i,]$Exposure_Start <- fullset[i,]$Tag_date 
  } else {
    fullset[i,]$Exposure_Start <- fullset[i,]$Tag_date + fullset[i,]$dummy3 
  }
}


fullset <- fullset %>% #cuts out extra dates from when the system was not on
  filter(Exposure_Start < "2013-06-20" | Exposure_Start > "2014-04-11") %>%
  filter (Exposure_Start < "2014-07-02") 

########################################################
###calculate exposure intervals for each day############  
########################################################

#assign a time of midnight to the start of each exposure event other than the day of tagging
z <- nrow(fullset)

for (i in 1:z) {  
  
  if(fullset[i,]$dummy3 == 0) { 
    fullset[i,]$Exposure_Start <- fullset[i,]$Exposure_Start 
  } else {
    hour(fullset[i,]$Exposure_Start) <- 0 
  }
}


#assign a date/time for exposer end (the end of each day) 
#BRUTE FORCE, SHOULD BE A MORE ELEGANT ANSWER USING A LOOP AND ASSIGNING THE BEGINNING OF THE NEXT EXPOSURE
#BUT NEEDS TO BE DONE BY TAG CODE, AND WHAT HAPPENS ON THE LAST DAY

fullset$Exposure_End <- fullset$Exposure_Start

for (i in 1:z) {  
  
  if(fullset[i,]$dummy3 == 0) { 
    fullset[i,]$Exposure_End <- fullset[i,]$Exposure_Start + (60*60*11.9999) #new exposure begins 12 hrs after tagging
  } else {
    fullset[i,]$Exposure_End <- fullset[i,]$Exposure_Start + (60*60*23.9999) #new exposure begins each subsequent day
  }
}

fullset$Exposure_Duration <- difftime(fullset$Exposure_End, fullset$Exposure_Start, units="secs")


####Identify which detections on the array should count as a single exposure#### 

options(digits.secs=2)
options(digits=16)

###downstream antenna cutoffs####
down<-detections[detections$antenna==3,]#pull out  just the downstream antenna data
down$PIT<-as.numeric(substr(down$tag_code,11,16))

down<-down%>%
  arrange(PIT,date_time)%>%
  group_by(PIT)%>%
  mutate(TimeLag=as.numeric(ifelse(row_number()==1,0,
                                   as.numeric(difftime(date_time,lag(date_time),units="secs")))),#calculate lags
         Switch=ifelse(row_number()==1,1,#if it's the first row put a 1, otherwise...
                       ifelse(TimeLag<2000,0,1)),#IF THE CONTACT IS WITHIN THE TIME THRESHOLD FOR A NEW PRESENCE, PUT A ZERO, OTHERWISE A 1
         Presence=cumsum(Switch))%>%
  ungroup()

###Plot####
#down$TimeLag<-ifelse(down$TimeLag>10000,10000,down$TimeLag)#chop it to look at the distribution
#bins<-c(0:167)
#bins<-as.vector(bins*60)#create X second bins
#HistOut<-hist(down$TimeLag,breaks=bins)
#HistOut$breaks<-HistOut$breaks[1:167]#This removes an extra line from the histogram output and allows plotting
#plot(HistOut$count~HistOut$breaks,log="y",type='h',main="Lags between detections (60 second bins)")
#####
#group dataframe so we have one line per attempt
down<-group_by(down,tag_code,Presence)

#Astart and AEnd indicate start and end times for each attempt, with a 3000s threshold
a<-summarize(down, AStart=min(date_time), AEnd=max(date_time))#,NDets=sum(consec_det))

#unique((a$tag_code))#about 41 fish staging 636 attempts♠

#down_2013 <- a %>%
#  filter(AStart < "2014-04-11")


###entrance antenna cutoffs####
enter<-detections[detections$antenna==2,]#pull out  just the entrance antenna data
enter$PIT<-as.numeric(substr(enter$tag_code,11,16))

enter<-enter%>%
  arrange(PIT,date_time)%>%
  group_by(PIT)%>%
  mutate(TimeLag=as.numeric(ifelse(row_number()==1,0,
                                   as.numeric(difftime(date_time,lag(date_time),units="secs")))),#calculate lags
         Switch=ifelse(row_number()==1,1,#if it's the first row put a 1, otherwise...
                       ifelse(TimeLag<180,0,1)),#IF THE CONTACT IS WITHIN THE TIME THRESHOLD FOR A NEW PRESENCE, PUT A ZERO, OTHERWISE A 1
         Presence=cumsum(Switch))%>%
  ungroup()

###Plot####
#enter$TimeLag<-ifelse(enter$TimeLag>10000,10000,enter$TimeLag)#chop it to look at the distribution
#enter_trunc<- subset(enter, TimeLag<600) #remove detections more than 10 min apart; obviously separate attempts
#bins<-c(0:20)
#bins<-as.vector(bins*30)#create X second bins
#HistOut<-hist(enter_trunc$TimeLag,breaks=bins)
#HistOut$breaks<-HistOut$breaks[1:20]#This removes an extra line from the histogram output and allows plotting
#plot(HistOut$count~HistOut$breaks,log="y",type='h',main="Lags between detections (30 second bins)")

#####look for A2-A1-A2 to see if there are any "false" successes ####
#pattern1<- detected%>%   
#  arrange(tag_code, date_time) %>%
#  group_by (tag_code) %>%
#  mutate(Trouble = ifelse (row_number()==1,0,
#                           ifelse(antenna==2 & lag(antenna)==1, 1,0)))%>%
#  ungroup

#filter(pattern1, Trouble==1) #tag 137665 travelled downstream through ladder in June (A1-A2)
#####

#group dataframe so we have one line per attempt
enter<-group_by(enter,tag_code,Presence)

#Astart and AEnd indicate start and end times for each attempt, with a 180s threshold
c<-summarize(enter, Exposure_Start=min(date_time), Exposure_End=max(date_time)) #,NDets=sum(consec_det))

#unique((c$tag_code))#about X fish staging X attempts♠

###exit antenna cutoffs####
exit<-detections[detections$antenna==1,]#pull out  just the entrance antenna data
exit$PIT<-as.numeric(substr(exit$tag_code,11,16))

exit<-exit%>%
  arrange(PIT,date_time)%>%
  group_by(PIT)%>%
  mutate(TimeLag=as.numeric(ifelse(row_number()==1,0,
                                   as.numeric(difftime(date_time,lag(date_time),units="secs")))),#calculate lags
         Switch=ifelse(row_number()==1,1,#if it's the first row put a 1, otherwise...
                       ifelse(TimeLag<120,0,1)),#IF THE CONTACT IS WITHIN THE TIME THRESHOLD FOR A NEW PRESENCE, PUT A ZERO, OTHERWISE A 1
         Presence=cumsum(Switch))%>%
  ungroup()

#group dataframe so we have one line per attempt
exit<-group_by(exit,tag_code,Presence)

#Astart and AEnd indicate start and end times for each attempt, with a 120s threshold
d<-summarize(exit, Exposure_Start=min(date_time), Exposure_End=max(date_time))

#############################FOR 2013 only ANALYSES#########################################
returnset_2013 <- fullset %>% #cut dataset to only 2013 "returners" (were recorded on the array)
  filter (ping ==1 & Exposure_Start < "2014-04-11") %>%
  select (-c("ping", "dummy2", "dummy3")) %>%
  mutate(Exposure_Dam = 1) %>% #will number once I integrate the ladder approaches
  mutate(Exposure_Ladder = as.numeric("")) %>% #will be blank for this part of the df
  mutate(Censor_Approach = 2) %>% #(2= change of covariates; adjust 1= enters ladder later)
  mutate(Censor_Ladder = as.numeric("")) %>% #(blank for this df)
  relocate(Species:Age, .after = Censor_Ladder) %>%
  relocate(Exposure_Dam:Exposure_Ladder, .after = Tag_date)

enter2013 <- c %>%
  filter(Exposure_Start < "2014-04-11") %>%
  mutate(Exposure_Duration = difftime(Exposure_End, Exposure_Start, units="secs")) %>%
  rename(Exposure_Ladder = Presence) %>%
  mutate(Exposure_Dam = as.numeric("")) %>%
  mutate(Censor_Approach = as.numeric("")) %>% #blank for this df
  mutate(Censor_Ladder = 0) %>% #(0 = fails to pass; adjust to 1 = passes or 2= change of covariate later)
  left_join(fish, by= "tag_code") %>%
  relocate(c("Tag_date","Exposure_Dam"), .before = Exposure_Ladder)

exit2013 <- d %>%
  filter(Exposure_Start < "2014-04-11") %>%
  mutate(Exposure_Duration = difftime(Exposure_End, Exposure_Start, units="secs")) %>%
  rename(Exposure_Ladder = Presence) %>%
  mutate(Exposure_Dam = as.numeric("")) %>%
  mutate(Censor_Approach = as.numeric("")) %>% #blank for this df
  mutate(Censor_Ladder = as.numeric("")) %>% #blank for this df
  left_join(fish, by= "tag_code") %>%
  relocate(c("Tag_date","Exposure_Dam"), .before = Exposure_Ladder)

###combine datasets together into 1, assign Censor codes, fix Exposure times
df_2013<- bind_rows(returnset_2013, enter2013, exit2013, .id= "id") %>% #id column assigns a 1,2,or 3 to each of the individual dfs
  group_by(tag_code) %>%
  arrange(tag_code,Exposure_Start) %>%
  slice(seq_len(min(which(id == "3"), n()))) #removes rows after a fish exits the ladder

####### Export df_2013 into a .csv to make changes#### 
######I can't figure out how to do in R :( and then bring back in ####

#write.csv(df_2013,"C:/Users/jvasslides/Documents/R_Projects/Shenandoah_Ladder/df_2013.csv")

#need to
# 1. add a row after a ladder exposure to account for the new dam exposure 
# 2. change the Exposure_End times for when a tagged fish approaches the ladder
# 3. increment the dam exposures after a ladder exposure(s)
# 4. adjust Censor_ladder for passage (change to 1)

###adjusts the Censor approach for ladder entry  
#df_2013$Censor_Approach = case_when (
#    df_2013$id == "2" ~ "",
#    df_2013$id == "1" & lead(df_2013$id) == "2" ~ "1",
#    TRUE ~ "2"
#  )

#library(readr)
#df_2013b <-read_csv ("C:/Users/jvasslides/Documents/R_Projects/Shenandoah_Ladder/df_2013.csv",
#                     col_names = TRUE, col_types = cols(.default = "?", Age = "i"))  
 
#  df_2013b$Tag_date <- mdy_hms(df_2013b$Tag_date, tz = 'America/New_York')
#  df_2013b$Exposure_Start <- mdy_hms(df_2013b$Exposure_Start, tz = "America/New_York")
#  df_2013b$Exposure_End <- mdy_hms(df_2013b$Exposure_End, tz = "America/New_York")
#  df_2013b$Exposure_Duration <-difftime(df_2013b$Exposure_End, df_2013b$Exposure_Start, units="secs")
#####
  
###############bring in covariate data and combine with tag data##############

tempdata <-read.csv ("C:/Users/jvasslides/Documents/R_Projects/Shenandoah_Ladder/Temp_Corrected.csv")

tempdata$Date<- as.Date(tempdata$Date)
tempdata <- select(tempdata, Date, avgTemp)

discharge <-read.csv ("C:/Users/jvasslides/Documents/R_Projects/Shenandoah_Ladder/USGS discharge data.csv")
##THERE ARE MISSING DAYS FROM THE USGS DATA RECORD IN 2013

discharge$Date_Time <- as.character(discharge$Date_Time)
discharge$Date_Time <- as.POSIXct(discharge$Date_Time, tz = "US/Eastern", "%m/%d/%Y %H:%M")

discharge$Date <- date(discharge$Date_Time)

discharge <- discharge %>%
  group_by(Date) %>%
  summarise(avg_discharge = mean(Discharge))

covariates <- full_join(tempdata, discharge, by = "Date")

df_2013b<- df_2013b %>%
  mutate(Date = date(Exposure_Start)) %>%
  mutate(Exposure_Start_Seconds = as.numeric(difftime(Exposure_Start, Tag_date, units = "secs"))) %>%
  mutate(Exposure_End_Seconds = as.numeric(difftime(Exposure_End, Tag_date, units = "secs"))) %>%
  rename(Length = Length..mm.) %>%
  rename(Weight = Weight..g.)

df_2013b[952, 17] = 423433
df_2013b[1481, 17] = 512900

final_2013 <- left_join(df_2013b, covariates, by = "Date")  

#survcheck(Surv(Exposure_Start_Seconds, Exposure_End_Seconds, Censor_Approach) ~ 1, id=tag_code, data = final_2013)


coxtest <- coxph(Surv(Exposure_Start_Seconds, Exposure_End_Seconds, Censor_Approach) ~ 
                   Species + Length + avgTemp + avg_discharge, data=final_2013)

summary(coxtest)



###############Previous script#############################################################
##########################################################################################
detectlist<- detections%>%   #############generates a list of tag codes recorded on A1 and A2
  filter (antenna != 3) %>%
  distinct (tag_code)

detectlist$dummy <-1

foo <- left_join(fish,detectlist, by = "tag_code")  #######bind the detected codes onto the list of all fish
foo$dummy [is.na(foo$dummy)] <- 0


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

#########################################################
### Split out fish that were never detected at ladder ###
### and Add Censor Values for Undetected Fish ###
#########################################################
undetected <- filter(fullset, dummy != 1)  #filter only to fish that were never detected on ladder

undetected$Dam <- 1 #number of exposures to the dam - only 1 because a change in covariates does not count as a new exposure
undetected$Ladder <- "" #number of exposures to the ladder - unused for this group as they never try to get up the ladder
undetected$Ladder<- as.numeric(undetected$Ladder)
undetected$censor_approach <- 2 # censor code for the approach; 1 = enter ladder, 2 = change of covariates
undetected$censor_ladder <- "" #censor code for the ladder; 0= fails to pass, 1 = passes, 2= change of covariates 
undetected$censor_ladder<- as.numeric(undetected$censor_ladder)


#####################################################
###Create A Record For Each Ladder Detection Event###
#####################################################

####generates a df of detection records from A1 and A2
detected<- detections%>%   
  filter (antenna != 3) %>%
  select (tag_code, antenna, date_time) %>%
  arrange(tag_code, date_time, -antenna)


###Identify which detections should count as a single exposure 
####(where the interval between detections < 2 minutes)


options(digits.secs=2)
options(digits=16)

###downstream antenna cutoffs####
down<-detections[detections$antenna==3,]#pull out  just the downstream antenna data
down$PIT<-as.numeric(substr(down$tag_code,11,16))

down<-down%>%
  arrange(PIT,date_time)%>%
  group_by(PIT)%>%
  mutate(TimeLag=as.numeric(ifelse(row_number()==1,0,
                        as.numeric(difftime(date_time,lag(date_time),units="secs")))),#calculate lags
         Switch=ifelse(row_number()==1,1,#if it's the first row put a 1, otherwise...
                       ifelse(TimeLag<3000,0,1)),#IF THE CONTACT IS WITHIN THE TIME THRESHOLD FOR A NEW PRESENCE, PUT A ZERO, OTHERWISE A 1
         Presence=cumsum(Switch))%>%
  ungroup()


down$TimeLag<-ifelse(down$TimeLag>10000,10000,down$TimeLag)#chop it to look at the distribution
bins<-c(0:167)
bins<-as.vector(bins*60)#create X second bins
HistOut<-hist(down$TimeLag,breaks=bins)
HistOut$breaks<-HistOut$breaks[1:167]#This removes an extra line from the histogram output and allows plotting
plot(HistOut$count~HistOut$breaks,log="y",type='h',main="Lags between detections (60 second bins)")


#group dataframe so we have one line per attempt
down<-group_by(down,PIT,Presence)
#Astart and AEnd indicate start and end times for each attempt, with a 3000s threshold
a<-summarize(down, AStart=min(date_time), AEnd=max(date_time),NDets=sum(consec_det))

unique((a$PIT))#about 41 fish staging 636 attempts♠

###entrance antenna cutoffs####
enter<-detections[detections$antenna==2,]#pull out  just the entrance antenna data
enter$PIT<-as.numeric(substr(enter$tag_code,11,16))

enter<-enter%>%
  arrange(PIT,date_time)%>%
  group_by(PIT)%>%
  mutate(TimeLag=as.numeric(ifelse(row_number()==1,0,
                                   as.numeric(difftime(date_time,lag(date_time),units="secs")))),#calculate lags
         Switch=ifelse(row_number()==1,1,#if it's the first row put a 1, otherwise...
                       ifelse(TimeLag<0.5,0,1)),#IF THE CONTACT IS WITHIN THE TIME THRESHOLD FOR A NEW PRESENCE, PUT A ZERO, OTHERWISE A 1
         Presence=cumsum(Switch))%>%
  ungroup()

enter$TimeLag<-ifelse(enter$TimeLag>10000,10000,enter$TimeLag)#chop it to look at the distribution
enter_trunc<- subset(enter, TimeLag<600) #remove detections more than 10 min apart; obviously separate attempts
bins<-c(0:20)
bins<-as.vector(bins*30)#create X second bins
HistOut<-hist(enter_trunc$TimeLag,breaks=bins)
HistOut$breaks<-HistOut$breaks[1:20]#This removes an extra line from the histogram output and allows plotting
plot(HistOut$count~HistOut$breaks,log="y",type='h',main="Lags between detections (30 second bins)")

#####look for A2-A1-A2 to see if there are any "false" successes
pattern1<- detected%>%   
  arrange(tag_code, date_time) %>%
  group_by (tag_code) %>%
  mutate(Trouble = ifelse (row_number()==1,0,
            ifelse(antenna==2 & lag(antenna)==1, 1,0)))%>%
  ungroup

filter(pattern1, Trouble==1) #tag 137665 travelled downstream through ladder in June (A1-A2)


######
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


#####Remove intermediate detections within a single exposure####
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

library(survival)









