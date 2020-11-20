#count numbers of each value on each metrics of each model
#ex. withSign
# metrics include -1, 0, 1
# count numbers of each value on each metrics
#  - Eye1_EEG1
#     -1: 5, 0: 2, 1: 7
#  - Eye1_EEG2 ....

setwd("D:/Senkoka/Reseach/Journal/wd/Reason_for_diff_4models/sum")
source("D:/Senkoka/Reseach/Journal/wd/base/person_wave_task.R")

library(dplyr)
library(stringr)
library(openxlsx)
library(reshape2)

## options ##
#which metrics. withLength or withSign or withoutBoth
metrics <- c("withoutBoth", "withSign", "withLength", "withBoth")
#which groups. 
groupNames <- c("All", "Success", "Failure", "Time_up", "Not_time_up",
                "Easy", "Difficult","Success_Es", "Success_Df",
                "Time_up_Es", "Time_up_Df", "Not_time_up_Es",
                "Not_time_up_Df")
## options is up to here ##

## data input ##
data <- vector("list", length = length(metrics))
names(data) <- metrics
for (i in 1:length(metrics)) {
  data[[i]] <- read.xlsx("table_4models_input.xlsx", sheet = i)
}
## data input up to here ##

## sub func. ##
get_groups <- function(data) {
  #1:all, 2:succ., 3: fail., 4:time_up, 5:not_time_up,
  #6:easy, 7:difficult, 8:succ_easy, 9:succ_difficult,
  #10:time_up_easy, 11:time_up_difficult,
  #12:not_time_up_easy, 13:not_time_up_difficult
  groups <- vector("list", length = length(groupNames))
  names(groups) <- groupNames
  groups[[1]] <- data
  groups[[2]] <- filter(data, result=="success")
  groups[[3]] <- filter(data, result=="failure") 
  groups[[4]] <- filter(data, time.up=="time up")
  groups[[5]] <- filter(data, time.up=="not time up")
  groups[[6]] <- filter(data, difficulty=="easy")
  groups[[7]] <- filter(data, difficulty=="difficult")
  groups[[8]] <- filter(data, time.up == "success" & difficulty=="easy")
  groups[[9]] <- filter(data, time.up == "success" & difficulty=="difficult")
  groups[[10]] <- filter(data, time.up == "time up" & difficulty=="easy")
  groups[[11]] <- filter(data, time.up == "time up" & difficulty=="difficult")
  groups[[12]] <- filter(data, time.up == "not time up" & difficulty=="easy")
  groups[[13]] <- filter(data, time.up == "not time up" & difficulty=="difficult")
  return(groups)
}

## sub func. up to here ##

#results have results of 4models.
#results_each_models have results of 13 groups.
results <- vector("list", length = length(metrics))
names(results) <- metrics
results_each_models <- vector("list", length = length(groupNames))
names(results_each_models) <- groupNames

s_idx <- which(colnames(data[[1]])=="Eye1_EEG1")
e_idx <- which(colnames(data[[1]])=="Eye3_EEG3")

for (i in 1:length(metrics)) {
  groups <- get_groups(data[[i]])
  
  #Eye1_EEG1‚ÆŒ”n‚ð1ƒZƒbƒg‚É‚µ‚Ä‘‚«‚½‚¢
  for (j in 1:length(groups)) {#per groups(ex. success)
    results_each_models[[j]] <- t(count(groups[[j]], groups[[j]][s_idx]))
    for (k in (s_idx+1):e_idx) {#per metrics(ex. Eye1_EEG1)
      #
      results_each_models[[j]] <- cbind(results_each_models[[j]],
                                        t(count(groups[[j]], groups[[j]][k])))
      #tmp <- t(count(groups[[j]], groups[[j]][k]))
      #diff_row <- nrow(results_each_models[[j]])-nrow(tmp)
      if (diff_row < 0) {#row less than tmp
        na_df <- as.data.frame(matrix(data = NA, 
                                      nrow = abs(diff_row),
                                      ncol = ncol(results_each_models[[j]])))
        colnames(na_df) <- colnames(results_each_models[[j]])
        results_each_models[[j]] <- rbind(results_each_models[[j]], na_df)
      }else{#row more than tmp
        na_df <- as.data.frame(matrix(data = NA, 
                                      nrow = abs(diff_row),
                                      ncol = ncol(tmp)))
        colnames(na_df) <- colnames(tmp)
        tmp <- rbind(tmp, na_df)
      }
      results_each_models[[j]] <- cbind(results_each_models[[j]], tmp)
      #
    }
    #results_each_models[[j]] <- select(results_each_models[[j]], 
    #                                   colnames(groups[[j]][,s_idx:e_idx]), n)
  }
  results[[i]] <- results_each_models
}

for (i in 1:length(metrics)) {
  write.xlsx(results[[i]], paste("count_",metrics[i], ".xlsx", sep = ""), colNames=T)
}

apply(tmp, 2, function(y){
  x <- xtabs(y, y)
  return(x)
  }
)
