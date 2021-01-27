##########################################
#####Lake Shenandoah PIT Tagging Data#####
##########################################

library(PITR)
library("xlsx")
library(dplyr)
library(lubridate)
##set working directory for file locations##
setwd("Z:/BBP Projects/Herring work/Lake Shenandoah ladder/Analysis")

#####2013 files are from firmware earlier than V5 (April 2014)
old <- "Z:/BBP Projects/Herring work/Lake Shenandoah ladder/Analysis/array_old"

######2014 files are from current firmware
new <- "Z:/BBP Projects/Herring work/Lake Shenandoah ladder/Analysis/array_new"

######assign test tag numbers#####
tt <- c("0000_0000000174764544","0000_0000000174764573", "0000_0000000180573686", "0000_0000000181177608", "0000_0000000174764573", "0000_00000181177608")


######################################################################################################
### pull the data together, subset for only test tags, and look to see when the system was running ###
######################################################################################################
old_dat_all <- old_pit(data=old, test_tags = NULL, print_to_file = FALSE)

new_dat_all <- new_pit(data=new, test_tags = NULL, print_to_file = FALSE)

full_dat <-rbind(old_dat_all$all_det, new_dat_all$all_det)
full_dat$tag_type<- as.factor(full_dat$tag_type)

marker_tags<-full_dat %>%
  filter(tag_type !="A") %>%
  group_by(antenna) %>%
  arrange(date, .by_group=TRUE) %>%
  count(antenna, date) %>%
  mutate(year=year(date)) %>%
  mutate(ant = case_when (
    antenna ==1 ~ "exit",
    antenna ==2 ~ "entrance",
    antenna ==3 ~ "downstream"
  ))

marker_tags$ant <- as.factor(marker_tags$ant)

marker_2013 <-filter(marker_tags, year ==2013)
marker_2014 <- filter(marker_tags, year ==2014)

library(ggplot2)

ggplot(marker_2014, aes(x=date, y=ant)) +
  geom_point(alpha=0.5)+
  ggtitle("2014 Marker Tag detections")

rm(old_dat_all, new_dat_all)

#################################################
### collate the data, removing the test tags ####
#################################################

old_dat <- old_pit(data=old, test_tags = tt, print_to_file = FALSE)
new_dat <- new_pit(data=new, test_tags = tt, print_to_file = FALSE)

tag_fish<- rbind(old_dat$all_det, new_dat$all_det)####dataframe with only detections of tagged fish
  tag_fish<- filter(tag_fish, tag_code != "900_226000135123") ##tag used for test on exit antenna
  tag_fish<- filter(tag_fish, tag_code != "900_226000135118") ##tag used for test on exit antenna
tag_all<-alldat$multi_data ##dataframe with all detections (test tags and tagged fish)

######rename antennas so that A1 is downstream and A4 is upstream##############
rename_one <- array_config(data = tag_fish, configuration = "rename_antennas",
                           reader_name = "R1", ao1 = "1", an1 = "4")
rename_two <- array_config(data = rename_one, configuration = "rename_antennas",
                           reader_name = "R1", ao1 = "3", an1 = "1")

##############calculate detection efficiency##################################
efficiency_2013_week <-det_eff(data=rename_two, resolution = "week", by_array = FALSE,
                               direction = "up", start_date = "2013-04-15 12:27:23", 
                               end_date = "2013-06-19 05:24:25")

efficiency_2014_week <-det_eff(data=rename_two, resolution = "week", by_array = FALSE,
                               direction = "up", start_date = "2014-04-11 07:27:28", 
                               end_date = "2014-07-01 09:48:08")

write.xlsx(efficiency_2013_week, "Z:/BBP Projects/Herring work/Lake Shenandoah ladder/Analysis/detection_efficiency.xlsx", sheetName = "2013",
           row.names=FALSE, showNA = TRUE)
write.xlsx(efficiency_2014_week, "Z:/BBP Projects/Herring work/Lake Shenandoah ladder/Analysis/detection_efficiency.xlsx", sheetName = "2014",
           row.names=FALSE, append = TRUE, showNA = TRUE)

####################### determine first and last hits on an antenna#################
fi_la2013 <- first_last(data=rename_two, resolution = "year", 
                    start_date = "2013-04-15 12:27:23", end_date = "2013-06-19 05:24:25")

fi_la2014 <-first_last(data=rename_two, resolution = "year", start_date = "2014-04-11 07:27:28", 
                        end_date = "2014-07-01 09:48:08")

write.xlsx(fi_la2013, "Z:/BBP Projects/Herring work/Lake Shenandoah ladder/Analysis/first_last_detections.xlsx", sheetName = "2013",
           row.names=FALSE)
write.xlsx(fi_la2014, "Z:/BBP Projects/Herring work/Lake Shenandoah ladder/Analysis/first_last_detections.xlsx", sheetName = "2014",
           row.names=FALSE, append = TRUE)

########################fishway efficiency####################
fish <-read.xlsx("Z:/BBP Projects/Herring work/Lake Shenandoah ladder/Analysis/tagged fish.xlsx", sheetName = "Sheet1", as.data.frame=TRUE, header=TRUE)

fish$tag <- '900_'
fish$Tag..<- as.character((fish$Tag..))
fish$tag_code <- paste(fish$tag, fish$Tag.., sep = "")

fish<- fish %>%
  rename('Tag_date'='Date') %>%
  select (-'tag', -'Tag..')

detect <- left_join(tag_fish, fish, by = 'tag_code')
detect$year<- as.factor(format(detect$date_time, format = "%Y"))

    
detect_by_year <-detect %>% 
  group_by(year) %>% 
  summarise(
    n_distinct(tag_code)
  )
#ant 1 is upstream, 3 is downstream
antenna_by_year <-detect %>% 
  group_by(year,antenna) %>% 
  summarise(
    n_distinct(tag_code)
  )

detect2014 <- detect %>%
  filter(year == "2014") %>%
  distinct(tag_code, .keep_all = TRUE) %>%
  arrange(Species)

detect2014A3 <- detect %>%
  filter(year == "2014" & antenna == 3) %>%
  distinct(tag_code, .keep_all = TRUE) %>%
  arrange(Species)

detect2014A2 <- detect %>%
  filter(year == "2014" & antenna == 2) %>%
  distinct(tag_code, .keep_all = TRUE) %>%
  arrange(tag_code)

detect2014A1 <- detect %>%
  filter(year == "2014" & antenna == 1) %>%
  distinct(tag_code, .keep_all = TRUE) %>%
  arrange(Species)
