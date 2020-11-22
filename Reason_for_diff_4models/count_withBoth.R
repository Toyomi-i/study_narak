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
#model number 1:withoutBoth, 2:withSign, 3:withLength, 4:withBoth
N_model <- 4
#values of each metrics
v_mtr <- c("minus_mean", "0", "plus_mean")
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
#calculate stats instead of count
get_counts_withBoth <- function(data) {
  counts[[1]] <- apply(data, 2, function(y) {
    return(mean(y[y < 0]))
  })
  counts[[2]] <- apply(data, 2, function(y) {
    return(sum(length(y[y == 0])))
  })
  counts[[3]] <- apply(data, 2, function(y) {
    return(mean(y[y > 0]))
  }
  )
  return(counts)
}
## sub func. up to here ##

#results have results of 4models.
results <- vector("list", length = length(metrics))
names(results) <- metrics

s_idx <- which(colnames(data[[1]])=="Eye1_EEG1")
e_idx <- which(colnames(data[[1]])=="Eye3_EEG3")

groups <- get_groups(data[[N_model]])
counts <- vector("list", length = length(v_mtr))
names(counts) <- v_mtr

for (group_num in 1:length(groups)) {
  counts <- get_counts_withBoth(groups[[group_num]][s_idx:e_idx])
  
  a <- makeErowdataframe()
  for (i in 1:length(counts[[1]])) {
    for (j in 1:length(counts)) {
      a <- cbind(a, counts[[j]][i])
    }
  }
  colnames(a) <- rep(names(counts), length(counts[[1]]))
  rownames(a) <- groupNames[group_num]
  results[[N_model]] <- rbind(results[[N_model]], a)
}

write.xlsx(results[[N_model]], paste("count_",metrics[N_model], ".xlsx", sep = ""), colNames=T)
