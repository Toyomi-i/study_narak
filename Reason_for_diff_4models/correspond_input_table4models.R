#入力系列とtable_4modelsの紐づけ

setwd("D:/Senkoka/Reseach/Journal/wd/Reason_for_diff_4models/correspond_input_table4models")
source("D:/Senkoka/Reseach/Journal/wd/base/person_wave_task.R")

library(dplyr)
library(stringr)
library(openxlsx)

## options ##
#TopN
N<-3
#data name. alpha or code or alpha_code
dataName <- "code"
#which metrics. withLength or withSign or withoutBoth
metrics <- c("withoutBoth", "withSign", "withLength", "withBoth")
## options is up to here ##

## data input ##
data_withoutBoth <- read.csv(paste("accuracies_", dataName, "_", metrics[1], "_Top", N, ".csv", sep=""))
data_withSign <- read.csv(paste("accuracies_", dataName, "_", metrics[2], "_Top", N, ".csv", sep=""))
data_withLength <- read.csv(paste("accuracies_", dataName, "_", metrics[3], "_Top", N, ".csv", sep=""))
data_withBoth <- read.csv(paste("accuracies_", dataName, "_", metrics[4], "_Top", N, ".csv", sep=""))
time_ups <- read.xlsx("cor.xlsx",sheet = 2)

input_withoutBoth <- read.csv(paste("input_forRF_", dataName, "_", metrics[1], "_Top", N, ".csv", sep = ""))
input_withSign <- read.csv(paste("input_forRF_", dataName, "_", metrics[2], "_Top", N, ".csv", sep = ""))
input_withLength <- read.csv(paste("input_forRF_", dataName, "_", metrics[3], "_Top", N, ".csv", sep = ""))
input_withBoth <- read.csv(paste("input_forRF_", dataName, "_", metrics[4], "_Top", N, ".csv", sep = ""))
## data input up to here ##

#calculate mean of input sequence
input_withoutBoth.mean <- apply(input_withoutBoth[, -ncol(input_withoutBoth)][,-1], 1, mean)
input_withSign.mean <- apply(input_withSign[, -ncol(input_withSign)][,-1], 1, mean)
input_withLength.mean <- apply(input_withLength[, -ncol(input_withLength)][,-1], 1, mean)
input_withBoth.mean <- apply(input_withBoth[, -ncol(input_withBoth)][,-1], 1, mean)

#correspond input sequence and accuracies
cbinded_withoutBoth <- cbind(input_withoutBoth[,-ncol(input_withoutBoth)][-1], data_withoutBoth[-1])
cbinded_withSign <- cbind(input_withSign[,-ncol(input_withSign)][-1], data_withSign[-1])
cbinded_withLength <- cbind(input_withLength[,-ncol(input_withLength)][-1], data_withLength[-1])
cbinded_withBoth <- cbind(input_withBoth[,-ncol(input_withBoth)][-1], data_withBoth[-1])

data_all_models <- list(cbinded_withoutBoth, cbinded_withSign, cbinded_withLength, cbinded_withBoth)

#rename "accuracy" to "acc_[each model]"
for (i in 1:length(data_all_models)) {
  names(data_all_models[[i]])[which(names(data_all_models[[i]])=="accuracy")] <- paste("acc_", metrics[i], sep="")
}

#merge time_ups and data ordered by (sbj, task, difficulty, result, task_time, time.up, true_answer, acc_[each_model], input_seqs)
results <- vector("list", length(data_all_models))

for (i in 1:length(data_all_models)) {
  results[[i]] <- merge(time_ups[,c(1,2,3,4,7,8)], 
                        data_all_models[[i]][,c(13,11,12,10,1:9)],
                        by = c("sbj", "task"))
}


for (i in 1:length(data_all_models)) {
  # rename sbj.
  results[[i]][results[[i]]$sbj=="Koga",]$sbj <- "Sbj.1"
  results[[i]][results[[i]]$sbj=="Takemoto",]$sbj <- "Sbj.2"
  results[[i]][results[[i]]$sbj=="Morita",]$sbj <- "Sbj.3"
  results[[i]][results[[i]]$sbj=="Taya",]$sbj <- "Sbj.4"
  results[[i]][results[[i]]$sbj=="Siga",]$sbj <- "Sbj.5"
  
  # convert na 
  results[[i]][!is.na(results[[i]]$time.up),]$time.up <- "time up"
  results[[i]][results[[i]]$result=="success",]$time.up <- "success"
  results[[i]][is.na(results[[i]]$time.up),]$time.up <- "not time up"
  results[[i]][is.na(results[[i]]$task_time),]$task_time <- "-"
}

tmp <- list("withoutBoth" = results[[1]], "withSign" = results[[2]], "withLength" = results[[3]], "withBoth" = results[[4]])
write.xlsx(tmp, paste("out/table_4models_input.xlsx", sep=""), colNames=T)
