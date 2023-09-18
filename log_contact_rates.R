rm(list=ls(all=TRUE))
library(rstudioapi)

dir <- dirname(getActiveDocumentContext()$path)
setwd(dirname(getActiveDocumentContext()$path))

## R-Libraries ----
library(fields)
library(Matrix)
library(clusterPower)
library(graphics)
library(inline)
library(mvtnorm)
library(Rcpp)
library(RcppArmadillo)

library(data.table)
library(socialmixr)
library(dplyr)
library(rstudioapi)
library(svcm)

source("SourceCode_ReadInData_with_weights_CoMix.R")
source("SourceCode_ReadInData_with_weights.R")
source('SourceCode_CreatePenalty.R')
source('SourceCode_PIRWLS_Transformations.R')
source('SourceCode_PIRWLS_NoTransformation.R')
source('SourceCode_TransformedData.R')
source('SourceCode_MultivariateNormal.R')
source('SourceCode_GridSearchPenalties.R')

# load the datasets
part_common <- read.csv("Data/be_comix/BE_Waves_12_43/CoMix_BE_participant_common.csv")
part_extra <- read.csv("Data/be_comix/BE_Waves_12_43/CoMix_BE_participant_extra.csv")
contact_common <- read.csv("Data/be_comix/BE_Waves_12_43/CoMix_BE_contact_common.csv")

participants <- merge(part_common, part_extra, by = c("part_id"))
participants <- rename(participants, dayofweek = weekday)
population_2023 <- read.table("Data/Demography/population_2023.txt")


## Social contact data & demographic data ----
##____________________________________________
in_data = Load_Social_Contact_Data_Comix(participant_data_input = participants,
                                         contact_data_input = contact_common,
                                         wave = 12,
                                         pop_data_input = population_2023,
                                         maximum_age = 83,
                                         weigh_age_input = T,
                                         weigh_dayofweek_input = T)
str(in_data)
in_data$tilde_e

## Preparing some matrices and vectors
max_age = 83
lower_index = 1
upper_index = max_age + 1

m = n = upper_index
One = matrix(1, ncol=m)
Y = in_data$mat_cont[lower_index:upper_index, lower_index:upper_index]
E = in_data$tilde_e[lower_index:upper_index] %*% One
p = in_data$P[lower_index:upper_index]$population
P = p %*% One

GAMMA = Y/E
ETA = log(GAMMA)
Z = GAMMA*P

rangeETA <- range(ETA, finite = TRUE)
zlb <- min(rangeETA) - 0.2
zub <- max(rangeETA) + 0.2

# Observed log contact rates
range(ETA,finite=TRUE)
plot.x1 = (lower_index:upper_index) -1
plot.x2 = (lower_index:upper_index) -1
image.plot(plot.x1, plot.x2, ETA, col=topo.colors(20), zlim = c(zlb, zub),
           xlab="Age of the respondent", 
           ylab="Age of the contact",
           main = "Observed log contact rates", 
           cex.lab=1.5, cex.axis=1.5)
