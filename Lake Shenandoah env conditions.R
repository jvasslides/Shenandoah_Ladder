#################
##Lake Shenandoah tagging project environmental info##
################

#rm(list=ls())

#library(reshape2)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(stringr)
library(tidyr)
#library(RColorBrewer)
#library(tidyr)
#library("xlsx")

setwd("Z:/BBP Projects/Herring work/Lake Shenandoah ladder/Analysis")

###################Bring in BBP discrete sampling data#################
library(RODBC)
db<- "Z:/BBP Public/BBP Research Data/Barnegat Bay Partnership Research Database.accdb"
con <- odbcConnectAccess2007(db)
#con <- odbcConnect("BBPData")
collections <- sqlFetch(con, "tblCollections")

BBP <- subset(collections, collections$`Project Name` == "Lake Shenandoah Fish Ladder")
BBP <- droplevels.data.frame(BBP)

#####################################Temperature data##################################

temp <-read.csv ("Lake_Shenandoah HOBO temps.csv", header = TRUE)
temp$Temp_C <- (temp$Temp_F - 32)*(5/9)
temp$Date_Time <- as.POSIXct(temp$Date_Time, tz = "", format = "%m/%d/%Y %H:%M")
temp$Date <- as.POSIXct(format(temp$Date_Time, format = "%Y/%m/%d"))

avg_temp <-temp %>% 
  group_by(Date) %>% 
  summarise(
    avgTemp=mean(Temp_C, na.rm = TRUE)
    )

avg_temp$Year <- as.factor(format(avg_temp$Date, format = "%Y"))
avg_temp$date_x <- avg_temp$Date %>% as.character %>%
  str_replace("^\\d{4}","2020") %>% as_date

            ###################BBP discrete temperatures #################
#BBPtemp <- BBP %>%
#  select('CollectionDt', 'Temperature (°C)') %>%
#  rename('avgTemp' = 'Temperature (°C)')
#BBPtemp$Year <- as.factor(format(BBPtemp$CollectionDt, format = "%Y"))
#BBPtemp$date_x <- BBPtemp$CollectionDt %>% as.character %>%
#  str_replace("^\\d{4}","2020") %>% as_date  


#Fig3 <-avg_temp %>%
#  ggplot(aes(x = date_x, y = avgTemp, colour = Year)) +
#  geom_line() +
#  geom_point(data = BBPtemp) +
#  scale_color_manual (values=c("#1b9e77","#7570b3")) +
#  theme_bw() +
#  xlab("Date") + ylab("Temperature °C") +
#  scale_x_date(labels = date_format("%b"))

####################################################################################
################temperature profile looks high; possible hobo dislodged#############
################check against USGS TR data and air temps############################
####################################################################################

###############Bring in USGS Toms River data###################
tempTR <-read.csv ("USGS TR Temp.csv", header = TRUE)
tempTR$Date_Time <- as.POSIXct(tempTR$datetime, tz = "", format = "%m/%d/%Y %H:%M")
tempTR$Date <- as.POSIXct(format(tempTR$Date_Time, format = "%Y/%m/%d"))

avg_tempTR <-tempTR %>% 
  group_by(Date) %>% 
  summarise(
    avgTemp=mean(Temp_C, na.rm = TRUE)
  )

avg_tempTR$Year <- as.factor(format(avg_tempTR$Date, format = "%Y"))
avg_tempTR$date_x <- avg_tempTR$Date %>% as.character %>%
  str_replace("^\\d{4}","2020") %>% as_date

###################Bring in air temp#############################
#airtemp <-read.csv ("Air temp.csv", header = TRUE)
#airtemp$Date <- as.POSIXct(airtemp$DATE, format = "%m/%d/%Y")

#airtemp$avgTemp <- (airtemp$TMAX + airtemp$TMIN)/2 
#airtemp$avgTemp <- (airtemp$avgTemp - 32)*(5/9)
#airtemp <- select(airtemp, Date, avgTemp)  
#airtemp$Year <- as.factor(format(airtemp$Date, format = "%Y"))
#airtemp$date_x <- airtemp$Date %>% as.character %>%
#  str_replace("^\\d{4}","2020") %>% as_date


#testfig <-avg_temp %>%
#  ggplot(aes(x = date_x, y = avgTemp, colour = Year)) +
#  geom_line() +
#  geom_line(data = avg_tempTR, linetype = 5) +
#  geom_line(data = airtemp, linetype = 4) +
#  scale_color_manual (values=c("#1b9e77","#7570b3")) +
#  theme_bw() +
#  xlab("Date") + ylab("Mean Daily Temperature °C") +
#  scale_x_date(labels = date_format("%b %d"))

################################################################################
############use April 2013 and 2014 data to regress Shenandoah on TR############
############then predict Shenandoah temperatures###############################
###############################################################################

#regdata<- merge(avg_tempTR, avg_temp, by.x= 'Date', by.y = 'Date')
#regdata$Month<- as.factor(format(regdata$Date, format = "%m"))

#regdata <- regdata %>%
#  filter(Month == "04") %>%
#  select('Date', 'avgTemp.x', 'avgTemp.y') %>%
#  rename('TR' = 'avgTemp.x') %>%
#  rename('LS' = 'avgTemp.y')

#summary(lm(regdata$LS~regdata$TR))
#regoutput <- lm(regdata$LS~regdata$TR)
#  plot(regoutput, pch=3)
#regdata$LSreg <- (1.04092*regdata$TR) + 0.56939

LSreg <- avg_tempTR 
LSreg$avgTemp<-(1.04092*LSreg$avgTemp) + 0.56939
#LSreg <- filter(LSreg, Date >= "2013-04-17")
LSreg[116,2] <-LSreg[115,2] - ((LSreg[118,2]-LSreg[115,2])/3)
LSreg[117,2] <-LSreg[116,2] + ((LSreg[118,2]-LSreg[115,2])/3)
LSreg[166,2] <-LSreg[165,2] + ((LSreg[170,2]-LSreg[165,2])/5)
LSreg[167,2] <-LSreg[166,2] + ((LSreg[170,2]-LSreg[165,2])/5)
LSreg[168,2] <-LSreg[167,2] + ((LSreg[170,2]-LSreg[165,2])/5)
LSreg[169,2] <-LSreg[168,2] + ((LSreg[170,2]-LSreg[165,2])/5)

Fig5 <-LSreg %>%
  ggplot(aes(x = date_x, y = avgTemp, colour = Year)) +
  geom_line() +
  #geom_point(data = BBPtemp) +
  scale_color_manual (values=c("#1b9e77","#7570b3")) +
  theme_bw() +
  xlab("Date") + ylab("Mean Daily Temperature °C") +
  scale_x_date(labels = date_format("%b %d"))
#ggsave("Figure5.jpg")

summary(pairwise.t.test(LSreg$avgTemp,LSreg$Year, p.adjust.method = "bonferroni"))

##################################discharge data#######################################
flow <- read.csv ("USGS discharge data.csv", header = TRUE)
flow$Date_Time <- as.POSIXct(flow$Date_Time, tz = "", format = "%m/%d/%Y %H:%M")
flow$Date <- as.POSIXct(format(flow$Date_Time, format = "%Y/%m/%d"))

avg_flow <-flow %>% 
  group_by(Date) %>% 
  summarise(
    avgFlow=mean(Discharge, na.rm = TRUE)
  )

avg_flow$Year <- as.factor(format(avg_flow$Date, format = "%Y"))
avg_flow$date_x <- avg_flow$Date %>% as.character %>%
  str_replace("^\\d{4}","2020") %>% as_date

Fig3 <-avg_flow %>%
  ggplot(aes(x = date_x, y = avgFlow, colour = Year)) +
  geom_line() +
  scale_color_manual (values=c("#1b9e77","#7570b3")) +
  theme_bw() +
  xlab("Date") + ylab("Mean Daily Discharge cfs") +
  scale_x_date(labels = date_format("%b %d")) 
#ggsave("Figure3.jpg")


BBPflow <- read.csv ("BBP flow.csv", header = TRUE)
BBPflow$Date <- as.POSIXct(BBPflow$Date, format = "%m/%d/%Y")
BBPflow <- BBPflow %>%
  select("Date", "stream_meters", "ladder_meters") %>%
  rename(stream = stream_meters) %>%
  rename(ladder = ladder_meters) %>%
  gather(key = "Location", value = "flow", stream, ladder) %>%
  arrange(Date)

BBPflow$Year <- as.factor(format(BBPflow$Date, format = "%Y"))
BBPflow$date_x <- BBPflow$Date %>% as.character %>%
  str_replace("^\\d{4}","2020") %>% as_date  

Fig4 <-BBPflow %>%
  ggplot(aes(x = date_x, y = flow, colour=Year)) +
  geom_point(aes(shape=Location)) +
  scale_color_manual (values=c("#1b9e77","#7570b3")) +
  scale_shape_manual (values=c(15,18)) +
  theme_bw() +
  xlab("Date") + ylab("Flow m/s") +
  #scale_x_date(date_labels = "%b %d")
  scale_x_date(labels = date_format("%b %d")) 
#ggsave("Figure4.jpg")

flowresults <-BBPflow %>% 
  group_by(Location) %>% 
  summarise(
    number=n(),
    maxflow=max(flow, na.rm = TRUE),
    minflow=min(flow, na.rm = TRUE),
    avgflow=mean(flow, na.rm = TRUE)
  )  
  


