library(data.table)
library(socialmixr)
library(dplyr)
library(rstudioapi)

dir <- dirname(getActiveDocumentContext()$path)
setwd(dirname(getActiveDocumentContext()$path))
source("SourceCode_ReadInData_with_weights_CoMix.R")

#-----
# load the datasets
part_common <- read.csv("Data/be_comix/BE_Waves_12_43/CoMix_BE_participant_common.csv")
part_extra <- read.csv("Data/be_comix/BE_Waves_12_43/CoMix_BE_participant_extra.csv")
contact_common <- read.csv("Data/be_comix/BE_Waves_12_43/CoMix_BE_contact_common.csv")

participants <- merge(part_common, part_extra, by = c("part_id"))
participants <- rename(participants, dayofweek = weekday)
population_2023 <- read.table("Data/Demography/population_2023.txt")

Load_Social_Contact_Data_Comix(participant_data_input = participants,
                               contact_data_input = contact_common,
                               wave = 12,
                               pop_data_input = population_2023,
                               maximum_age = 77,
                               weigh_age_input = T,
                               weigh_dayofweek_input = T)

