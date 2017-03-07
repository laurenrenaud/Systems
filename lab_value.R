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

# Filter out extreme values to fit into plot and create a fail/no fail flag
mic2 <- micro %>%
  dplyr::filter(value > 0 & value < 150000) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))

# Density plot for all Micro
ggplot(mic2, aes(x=value, fill =  fail)) + geom_density(alpha=0.6) + ylab("Value of Microbio Detected") +
  xlab("Density") +
  ggtitle("Microbial Screen Distribution") 

# Subset Micro data to Aerobic bacteria and filter out extreme values
aerobic_bacteria <- micro %>%
  dplyr::filter(name == "aerobic_bacteria") %>%
  dplyr::filter(value > 0 & value < 150000) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))

# Density plot for Aerobic bacteria
ggplot(aerobic_bacteria, aes(x=value, fill =  fail)) + geom_density(alpha=0.6) + geom_vline(aes(xintercept=100000), color="red", linetype="dashed", size=1) + xlab("Value of Aerobic Bacteria Detected") +
  ylab("Density") +
  ggtitle("Aerobic Bacteria Distribution") 

# Subset Micro data to Yeast and Mold and filter out extreme values
yeast_and_mold <- micro %>%
  dplyr::filter(name == "yeast_and_mold") %>%
  dplyr::filter(value > 0 & value < 20000) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))

# Density plot for Yeast and Mold
ggplot(yeast_and_mold, aes(x=value, fill =  fail)) + geom_density(alpha=0.6) + geom_vline(aes(xintercept=1000), color="red", linetype="dashed", size=1) + geom_vline(aes(xintercept=10000), color="red", linetype="dashed", size=1)+ xlab("Value of Yeast & Mold Detected") +
  ylab("Density") +
  ggtitle("Yeast & Mold Distribution") 

# Subset Micro data to Coliforms and filter out extreme values
coliforms <- micro %>%
  dplyr::filter(name == "coliforms") %>%
  dplyr::filter(value > 0 & value < 7000) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))

# Density plot Coliforms
ggplot(coliforms, aes(x=value, fill =  fail)) +  geom_vline(aes(xintercept=1000), color="red", linetype="dashed", size=1) + geom_density(alpha=0.6) + xlab("Value of Coliforms Detected") +
  ylab("Density") +
  ggtitle("Coliforms Distribution") 

# Subset Micro data to bile tolerant bacteria and filter out extreme values
bile_tolerant <- micro %>%
  dplyr::filter(name == "bile_tolerant") %>%
  dplyr::filter(value > 0 & value < 20000) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))

# Density plot for bile tolerant bacteria
ggplot(bile_tolerant, aes(x=value, fill =  fail)) + geom_vline(aes(xintercept=1000), color="red", linetype="dashed", size=1) + geom_density(alpha=0.6) + xlab("Value of Bacteria Detected") +
  ylab("Density") +
  ggtitle("Bile Tolerant Gram Negative Bacteria Distribution") + geom_density(alpha=0.6)

# Subset Micro data to E. coli and filter out extreme values
e_coli_and_salmonella <- micro %>%
  dplyr::filter(name == "e_coli_and_salmonella") %>%
  dplyr::filter(value > 0 & value < 20000) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))

# Density plot for E. Coli
ggplot(e_coli_and_salmonella, aes(x=value, fill = fail)) + geom_vline(aes(xintercept=0), color="red", linetype="dashed", size=1) + geom_density(alpha=0.6) + xlab("Value of E.Coli Detected") +
  ylab("Density") +
  ggtitle("E. Coli Distribution") + geom_density(alpha=0.6) + geom_density(alpha=0.6)

# Foreign Matter --------------------------------------------------
# Filter out extreme values to fit into plot and create a fail/no fail flag
fm4 <- foreign_matter %>%
  dplyr::filter(value > 0 & value < 15) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))

# Density plot for foreign matter
ggplot(fm4, aes(x=value, fill =  fail)) + geom_density(alpha=0.6) + geom_vline(aes(xintercept=2.1), color="red", linetype="dashed", size=1) + geom_vline(aes(xintercept=5), color="red", linetype="dashed", size=1) + xlab("Value of Foreign Matter Detected") +
  ylab("Density") +
  ggtitle("Foreign Matter Distribution") 


# Moisture --------------------------------------------------
# Filter out extreme values to fit into plot and create a fail/no fail flag
ms <- moisture %>%
  dplyr::filter(value > 0 & value <= 113) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))

# Density plot for moisture
ggplot(ms, aes(x=value, fill =  fail)) + geom_density(alpha=0.6) + geom_vline(aes(xintercept=15), color="red", linetype="dashed", size=1) + xlab("Value of Moisture Detected") +
  ylab("Density") +
  ggtitle("Moisture Content Distribution") 

# Solvent --------------------------------------------------
# Filter out extreme values to fit into plot and create a fail/no fail flag
sv <- solvent %>%
  dplyr::filter(value > 0 & value <= 5000) %>%
  dplyr::mutate(fail = ifelse(failure == 1, "Fail", "No Fail"))

# Density plot for solvent
ggplot(sv, aes(x=value, fill =  fail)) + geom_density(alpha=0.6) + geom_vline(aes(xintercept=500), color="red", linetype="dashed", size=1) + xlab("Value of Residual Solvent Detected") +
  ylab("Density") +
  ggtitle("Residual Solvent Distribution") 

