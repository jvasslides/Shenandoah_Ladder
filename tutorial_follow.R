#Quick Analysis based off of https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/

library(survival)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(ranger)
#Load and Prep data
herring<-read.csv("Herring_from_history.csv")
View(herring)
herring<-herring[,-1]
herring<-herring[,-c(15:16)]

herring<-herring %>% 
  mutate(event_time=End-Start) %>% 
  select(-Correlated_Flow,-Temperature)
#KM Model Fits
km<- with(herring, Surv(event_time, Pass))
km_fit <- survfit(Surv(event_time, Pass) ~ 1, data=herring)
summary(km_fit)
autoplot(km_fit)

km_species_fit <- survfit(Surv(event_time, Pass) ~ Species, data=herring)
autoplot(km_species_fit)

#COX PH Model Fits
#Cox model assumes that the covariates do not vary with time. In a vignette [12] that
#accompanies the survival package Therneau, Crowson and Atkinson demonstrate that the 
#Karnofsky score (karno) is, in fact, time-dependent so the assumptions for the Cox model 
#are not met. The vignette authors go on to present a strategy for dealing with time 
#dependent covariates.

cox1 <- coxph(Surv(event_time, Pass) ~ Species + Fultons + USGS_Flow, data = herring)
summary(cox1)

cox1_fit <- survfit(cox1)
autoplot(cox1_fit)

aa_fit <-aareg(Surv(event_time, Pass) ~ Species + Fultons + USGS_Flow, data = herring)
autoplot(aa_fit)
