#' R-function to load weighted social contact data from comix
#' The output of this function is;
#' Y (the total number of contacts made by respondent)
#' r (the total number of respondent (weighted))
#' p (population size)
#' 
#' pop_func is a function inside the Load_Social_Contact_Data_Comix
#' to linearly estimating age group sizes from the 5-year age bands
#' since wpp_countries function is using 5 years age band population data
#' 
#' Authors: Neilshan Loedy and Oswaldo Gressani
#' File last updated on 12/08/2023
#_____________________________________________________________________

library(socialmixr)
library(data.table)
library(dplyr)

#' (pop) --> if pop is not described, pop is using data from wpp_countries
#' from the socialmixr library

pop_func <- function (pop, age.limits, pop.age.column = "lower.age.limit", 
                      pop.column = "population", ...) {
  chkDots(...)
  if (!is.data.frame(pop) || !all(hasName(pop, c(pop.age.column, 
                                                 pop.column)))) {
    stop("Expecting 'pop' to be a data.frame with columns ", 
         pop.age.column, " and ", pop.column)
  }
  pop <- data.table(pop)
  setkeyv(pop, pop.age.column)
  if (!missing(age.limits)) {
    age.limits <- sort(age.limits)
    max.age <- max(pop[, pop.age.column, with = FALSE])
    missing.ages <- setdiff(age.limits[age.limits <= max.age], 
                            pop[[pop.age.column]])
    if (length(missing.ages) > 0) {
      message("Not all age groups represented in population data (5-year age band).\n  Linearly estimating age group sizes from the 5-year bands.")
      ..original.upper.age.limit <- NULL
      pop <- pop[, `:=`(..original.upper.age.limit, c(pop[[pop.age.column]][-1], 
                                                      NA))]
      pop <- pop[, `:=`(..original.lower.age.limit, get(pop.age.column))]
      all.ages <- data.frame(age.limits[age.limits <= max(pop[[pop.age.column]])])
      colnames(all.ages) <- pop.age.column
      pop <- merge(pop, all.ages, all = TRUE, by = pop.age.column)
      pop <- pop[, `:=`(..segment, cumsum(!is.na(..original.lower.age.limit)))]
      pop <- pop[, `:=`(..original.lower.age.limit, ..original.lower.age.limit[1]), 
                 by = ..segment]
      pop <- pop[, `:=`(..original.upper.age.limit, ..original.upper.age.limit[1]), 
                 by = ..segment]
      pop <- pop[, `:=`(paste(pop.column), get(pop.column)[1]), 
                 by = ..segment]
      pop <- pop[, `:=`(..upper.age.limit, c(pop[[pop.age.column]][-1], 
                                             NA))]
      pop[!is.na(..original.upper.age.limit), `:=`(population, 
                                                   round(population * (..upper.age.limit - get(pop.age.column))/(..original.upper.age.limit - 
                                                                                                                   ..original.lower.age.limit)))]
      pop <- pop[, c(pop.age.column, pop.column), with = FALSE]
    }
    pop <- pop[get(pop.age.column) >= min(age.limits)]
    pop <- pop[, `:=`(paste(pop.age.column), reduce_agegroups(get(pop.age.column), 
                                                              age.limits))]
    pop <- pop[, list(..population = sum(get(pop.column))), 
               by = pop.age.column]
    setnames(pop, "..population", pop.column)
  }
  setkeyv(pop, pop.age.column)
  return(as.data.frame(pop))
}

#' (participant_data_input) --> the participant data from the comix survey
#' (contact_data_input) --> the contact data from the comix survey
#' (country_input) --> the template is Belgium, this will be useful for adjusting the population
#' data from the wpp_countries function inside the socialmixr
#' (weigh_dayofweek_input) --> to weigh regarding weekday or weekend, to enable this function,
#' we need to have dayofweek column in the dataset, with input is 0, 1, ..., 6 (Sunday, Monday, ... Saturday)
#' (weigh_age_input) --> to weight regarding age
#' (age_breaks_input) --> if NULL, weigh regarding the age
#' eg., age_breaks_input = c(0, 5, 10) then the weight will be within [0,5), [5, 10)
#' age groups
#' (year) --> to set the year of population used from wpp_countries
#' (wave) --> filter the wave utilized from the CoMix survey
#' (maximum_age) --> set the maximum age analysed on participants and contacts data frame
#' (pop_data_input) --> to use user's own population data. The minimum data needed is 2 columns,
#' "lower.age.limit", and "population

Load_Social_Contact_Data_Comix <- function(participant_data_input = partdata,
                                           contact_data_input = contdata,
                                           country_input = c("Belgium"),
                                           weigh_dayofweek_input = T,
                                           weigh_age_input = T,
                                           age_breaks_input = NULL,
                                           year = 2015,
                                           wave = 12,
                                           maximum_age = NULL,
                                           pop_data_input = NULL){
  
  if(!is.null(wave)){
    participant_data_input = participant_data_input[participant_data_input$wave %in% wave,]
    contact_data_input = contact_data_input[contact_data_input$part_id %in% participant_data_input$part_id,]
  }
  
  contact_data_input$cnt_age_mean <- apply(contact_data_input[, c("cnt_age_est_min", "cnt_age_est_max")], 1, mean)
  
  if(!is.null(maximum_age)){
    participant_data_input = participant_data_input[participant_data_input$part_age < maximum_age,]
    contact_data_input = contact_data_input[contact_data_input$part_id %in% participant_data_input$part_id,]
    contact_data_input = contact_data_input[contact_data_input$cnt_age_mean < maximum_age,]
    contact_data_input = contact_data_input[!is.na(contact_data_input$cnt_age_mean),]
  }
  
  survey <- survey(participant_data_input, contact_data_input)
  
  survey$participants[, `:=`(weight, 1)]
  if (weigh_dayofweek_input) {
    found.dayofweek <- FALSE
    if ("dayofweek" %in% colnames(survey$participants)) {
      survey$participants[, `:=`(sum_weight, nrow(.SD)), 
                          by = (dayofweek %in% 1:5), ]
      survey$participants[dayofweek %in% 1:5, `:=`(weight, 
                                                   5/sum_weight)]
      survey$participants[!(dayofweek %in% 1:5), `:=`(weight, 
                                                      2/sum_weight)]
      survey$participants[, `:=`(sum_weight, NULL)]
      found.dayofweek <- TRUE
      survey$participants[, `:=`(is.weekday, dayofweek %in% 
                                   1:5)]
    }
    if (!found.dayofweek) {
      message("'weigh.dayofweek' is TRUE, but no 'dayofweek' column in the data. ", 
              "Will ignore.")
    }
  }
  
  if (weigh_age_input) {
    if (!is.null(pop_data_input)) {
      message("Will use the inputted population data for weighing")
      survey.pop <- data.table(pop_data_input)
      survey.pop <- survey.pop[order(lower.age.limit), ]
      max.age <- max(survey$participants[, "part_age"], 
                     na.rm = TRUE) + 1
      part.age.group.present <- age_breaks_input[age_breaks_input < max.age]
      survey.pop$upper.age.limit <- unlist(c(survey.pop[-1, 
                                                        "lower.age.limit"], 1 + max(survey.pop$lower.age.limit, 
                                                                                    part.age.group.present)))
      
      survey.pop.full <- data.table(pop_func(survey.pop, 
                                             seq(min(survey.pop$lower.age.limit), max(survey.pop$upper.age.limit))))
      
    } else {
      year.list <- unique(data.table(wpp_age(country_input))[, "year"])
      
      if(is.na(year)){
        country.pop <- data.table(wpp_age(country_input))
        survey.year <- data.table(wpp_age(country_input))[, max(year, na.rm = TRUE)]
        message("No data. Will use ", survey.year, " population data.")
      } else if(year %in% year.list$year){
        country.pop <- data.table(wpp_age(country_input))
        survey.year <- year
        message("Will use ", survey.year, " population data.")
      }
      
      survey.pop <- country.pop[year == survey.year][, 
                                                     list(population = sum(population)), by = "lower.age.limit"]
      
      survey.pop <- survey.pop[order(lower.age.limit), ]
      max.age <- max(survey$participants[, "part_age"], 
                     na.rm = TRUE) + 1
      part.age.group.present <- age_breaks_input[age_breaks_input < max.age]
      survey.pop$upper.age.limit <- unlist(c(survey.pop[-1, 
                                                        "lower.age.limit"], 1 + max(survey.pop$lower.age.limit, 
                                                                                    part.age.group.present)))
      
      survey.pop.full <- data.table(pop_func(survey.pop, 
                                             seq(min(survey.pop$lower.age.limit), max(survey.pop$upper.age.limit))))
    }
    
    
    if (!is.null(age_breaks_input)) {
      survey.pop.full <- data.table(pop_func(survey.pop, age_breaks_input))
      survey$participants[, part_age := cut(part_age,
                                            breaks = age_breaks_input, right = F)]
    }
    
    survey$participants[, `:=`(age.count, .N), by = "part_age"]
    survey$participants[, `:=`(age.proportion, age.count/.N)]
    part.age.all <- range(unique(survey$participants[, "part_age"]), na.rm = T)
    survey.pop.detail <- data.table(pop_func(survey.pop.full, 
                                             seq(part.age.all[1], part.age.all[2] + 1)))
    names(survey.pop.detail) <- c("part_age", 
                                  "population.count")
    survey.pop.detail[, `:=`(population.proportion, population.count/sum(population.count))]
    survey$participants <- merge(survey$participants, survey.pop.detail, 
                                 by = eval("part_age"))
    survey$participants[, `:=`(weight.age, population.proportion/age.proportion)]
    survey$participants[, `:=`(weight, weight * weight.age)]
    survey$participants[, `:=`(age.count, NULL)]
    survey$participants[, `:=`(age.proportion, NULL)]
    survey$participants[, `:=`(population.count, NULL)]
    survey$participants[, `:=`(population.proportion, NULL)]
    survey$participants[, `:=`(weight.age, NULL)]
  }
  
  tilde.e.2 = tapply(survey$participants$weight, survey$participants$part_age, sum)
  
  # Total number of contacts by age of participants and contacts
  mat.id=NULL
  un.id=sort(unique(participant_data_input$part_id))
  for (i in 1:length(un.id)){
    sel.id=contact_data_input$part_id==un.id[i]
    vec.id=rep(0,100)
    if (sum(sel.id)>0){
      vec.id=hist(contact_data_input$cnt_age_mean[sel.id],breaks=seq(0,100,1)-0.5,
                  plot=F)$counts
    }
    mat.id=rbind(mat.id,vec.id)
  }
  
  mat.id2 = mat.id
  for (i in 1:length(un.id)){
    mat.id2[i,] = mat.id[i,]*survey$participants$weight[i]
  }
  
  # Per participant's age
  mat.cont=NULL
  weight=NULL
  for (i in 0:100){
    sel.tmp=survey$participants$part_age[order(survey$participants$part_id)]==i
    vec.cont=rep(0,100)
    if(sum(sel.tmp)==1){
      vec.cont=mat.id2[sel.tmp,]
    }
    if(sum(sel.tmp)>1){
      vec.cont=apply(mat.id2[sel.tmp,],2,sum)
    }
    mat.cont=rbind(mat.cont,vec.cont)
  }
  
  return(list(mat_cont=mat.cont, tilde_e=tilde.e.2, P=survey.pop.full))
}
