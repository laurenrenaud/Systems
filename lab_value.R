library(dplyr)
library(ggplot2)
library(knitr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)
library(plotly)
library(reshape)

# Set working directory & load data ---------------------------
setwd("~/CMU/Spring 2017/Systems/Data")

# read in the data
foreign_matter <- read.csv('biotrackthc_labresults_foreign_matter.csv')
micro <- read.csv('biotrackthc_labresults_micro_screening.csv')
moisture <- read.csv('biotrackthc_labresults_moisture_content.csv')
solvent <- read.csv('biotrackthc_labresults_solvent_screening.csv')
potency <- read.csv('biotrackthc_labresults_potency_analysis.csv')

# Micro -------------------------------------
options(scipen=999)

mic2 <- micro %>%
  dplyr::filter(value > 0 & value < 150000) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))
ggplot(mic2, aes(x=value, fill =  fail)) + geom_density(alpha=0.6) + ylab("Value of Microbio Detected") +
  xlab("Density") +
  ggtitle("Microbial Screen Distribution") 

aerobic_bacteria <- micro %>%
  dplyr::filter(name == "aerobic_bacteria") %>%
  dplyr::filter(value > 0 & value < 150000) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))

ggplot(aerobic_bacteria, aes(x=value, fill =  fail)) + geom_density(alpha=0.6) + geom_vline(aes(xintercept=0), color="red", linetype="dashed", size=1) + xlab("Value of Aerobic Bacteria Detected") +
  ylab("Density") +
  ggtitle("Aerobic Bacteria Distribution") 

yeast_and_mold <- micro %>%
  dplyr::filter(name == "yeast_and_mold") %>%
  dplyr::filter(value > 0 & value < 7000) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))

ggplot(yeast_and_mold, aes(x=value, fill =  fail)) + geom_density(alpha=0.6) + geom_vline(aes(xintercept=0), color="red", linetype="dashed", size=1) + xlab("Value of Yeast & Mold Detected") +
  ylab("Density") +
  ggtitle("Aerobic Bacteria Distribution") 

coliforms <- micro %>%
  dplyr::filter(name == "coliforms") %>%
  dplyr::filter(value > 0 & value < 7000) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))

ggplot(coliforms, aes(x=value, fill =  fail)) + geom_density(alpha=0.6)

bile_tolerant <- micro %>%
  dplyr::filter(name == "bile_tolerant") %>%
  dplyr::filter(value > 0 & value < 20000) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))

ggplot(bile_tolerant, aes(x=value, fill =  fail)) + geom_density(alpha=0.6)

e_coli_and_salmonella <- micro %>%
  dplyr::filter(name == "e_coli_and_salmonella") %>%
  dplyr::filter(value > 0 & value < 20000) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))

ggplot(e_coli_and_salmonella, aes(x=value, fill = fail)) + geom_density(alpha=0.6)

# Foreign Matter --------------------------------------------------
fm <- foreign_matter %>%
  dplyr::group_by(value) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::filter(value > 0 & value < 50)

fm4 <- foreign_matter %>%
  dplyr::filter(value > 0 & value < 15) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))
ggplot(fm4, aes(x=value, fill =  fail)) + geom_density(alpha=0.6) + geom_vline(aes(xintercept=2.1), color="red", linetype="dashed", size=1) + geom_vline(aes(xintercept=5), color="red", linetype="dashed", size=1) + xlab("Value of Foreign Matter Detected") +
  ylab("Density") +
  ggtitle("Foreign Matter Distribution") 

############################# This doesn't work
fm2 <- foreign_matter %>%
  dplyr::filter(value >= 0) %>%
  dplyr::mutate(value_bin = cut(foreign_matter$value, breaks = c(0.00001, 0.1, 1, 2, 3, 4, 5, Inf), 
                                labels = c('0-0.1', '0.1-1', '1-2', '2-3', '3-4', '4-5', '5+'))) %>%
  dplyr::group_by(value_bin) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::filter(!is.na(value_bin)) %>%
  dplyr::mutate(percent = paste(round(100*count/sum(count),2), '%'))

ggplot(data = fm2, aes(x = value_bin, y = percent))
+ geom_bar(stat = "identity") + ylab("Value Range") +
  xlab("Number of Tests") +
  ggtitle("Foreign Matter Distribution") 

##############################

# Moisture --------------------------------------------------
ms <- moisture %>%
  dplyr::filter(value > 0 & value <= 113) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))
ggplot(ms, aes(x=value, fill =  fail)) + geom_density(alpha=0.6) + geom_vline(aes(xintercept=15), color="red", linetype="dashed", size=1) + xlab("Value of Moisture Detected") +
  ylab("Density") +
  ggtitle("Moisture Content Distribution") 

# Solvent --------------------------------------------------
sv <- solvent %>%
  dplyr::filter(value > 0 & value <= 5000) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))
ggplot(sv, aes(x=value, fill =  fail)) + geom_density(alpha=0.6) + geom_vline(aes(xintercept=500), color="red", linetype="dashed", size=1) + xlab("Value of Residual Solvent Detected") +
  ylab("Density") +
  ggtitle("Residual Solvent Distribution") 

